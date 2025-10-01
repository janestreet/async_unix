open Core
open Import
module Unix = Unix_syscalls

type env = Unix.env [@@deriving sexp]

type t =
  { pid : Pid.t
  ; stdin : Writer.t
  ; stdout : Reader.t
  ; stderr : Reader.t
  ; prog : string
  ; args : string list
  ; working_dir : string option
  ; env : env
  ; wait : Unix.Exit_or_signal.t Deferred.t Lazy.t
  }
[@@deriving fields ~getters, sexp_of]

module Aliases = struct
  type 'a create =
    ?argv0:string
    -> ?buf_len:int
    -> ?env:env
    -> ?prog_search_path:string list
    -> ?stdin:string
    -> ?working_dir:string
    -> ?setpgid:Core_unix.Pgid.t
    -> prog:string
    -> args:string list
    -> unit
    -> 'a Deferred.t

  type 'a run =
    ?accept_nonzero_exit:int list
    -> ?argv0:string
    -> ?env:env
    -> ?prog_search_path:string list
    -> ?stdin:string
    -> ?working_dir:string
    -> prog:string
    -> args:string list
    -> unit
    -> 'a Deferred.t

  type 'a collect = ?accept_nonzero_exit:int list -> t -> 'a Deferred.t
end

let create_fd ~pid ~prog ~args name file_descr =
  Fd.create
    Fifo
    file_descr
    (Info.create
       "child process"
       ~here:[%here]
       (name, `pid pid, `prog prog, `args args)
       [%sexp_of:
         string * [ `pid of Pid.t ] * [ `prog of string ] * [ `args of string list ]])
;;

let create_stdin_writer ?buf_len fd ~write_to_stdin =
  let writer =
    Writer.create
      ?buf_len
      fd
      ~buffer_age_limit:`Unlimited
      ~raise_when_consumer_leaves:false
  in
  Option.iter write_to_stdin ~f:(Writer.write writer);
  writer
;;

let create
  ?argv0
  ?buf_len
  ?(env = `Extend [])
  ?prog_search_path
  ?stdin:write_to_stdin
  ?working_dir
  ?setpgid
  ~prog
  ~args
  ()
  =
  match%map
    In_thread.syscall ~name:"create_process_env" (fun () ->
      Core_unix.create_process_env
        ~prog
        ~args
        ~env
        ?working_dir
        ?prog_search_path
        ?argv0
        ?setpgid
        ())
  with
  | Error exn -> Or_error.of_exn exn
  | Ok { pid; stdin; stdout; stderr } ->
    let create_fd name file_descr = create_fd ~pid ~prog ~args name file_descr in
    let stdin = create_stdin_writer (create_fd "stdin" stdin) ?buf_len ~write_to_stdin in
    let t =
      { pid
      ; stdin
      ; stdout = Reader.create ?buf_len (create_fd "stdout" stdout)
      ; stderr = Reader.create ?buf_len (create_fd "stderr" stderr)
      ; prog
      ; args
      ; working_dir
      ; env
      ; wait =
          (* It's ok that we're not using [Lazy_deferred] here because
             there are no known exceptions that [waitpid] can raise.
             Also, we need the deferred to be determined in the same Async job
             that makes the syscall, to make [send_signal] safe, so we can't use
             [Lazy_deferred] as is.
             The reason we don't want to eagerly reap the process is that the user might
             want to refer to this process by pid. The only way to do this safely
             is to do it before calling [wait].
          *)
          lazy (Unix.waitpid_prompt pid)
      }
    in
    Ok t
;;

let create_exn
  ?argv0
  ?buf_len
  ?env
  ?prog_search_path
  ?stdin
  ?working_dir
  ?setpgid
  ~prog
  ~args
  ()
  =
  create
    ?argv0
    ?buf_len
    ?env
    ?prog_search_path
    ?stdin
    ?working_dir
    ?setpgid
    ~prog
    ~args
    ()
  >>| ok_exn
;;

module Lines_or_sexp = struct
  type t =
    | Lines of string list
    | Sexp of Sexp.t

  let sexp_of_t t =
    match t with
    | Lines ([] | [ "" ]) -> [%sexp ""]
    | Lines lines -> [%sexp (lines : string list)]
    | Sexp sexp -> sexp
  ;;

  let create string =
    try Sexp (Sexp.of_string string) with
    | _ -> Lines (String.split ~on:'\n' string)
  ;;
end

module Output = struct
  module Stable = struct
    module V1 = struct
      type t =
        { stdout : string
        ; stderr : string
        ; exit_status : Unix.Exit_or_signal.t
        }
      [@@deriving compare ~localize, sexp]
    end
  end

  include Stable.V1

  let sexp_of_t t =
    [%message
      ""
        ~stdout:(Lines_or_sexp.create t.stdout : Lines_or_sexp.t)
        ~stderr:(Lines_or_sexp.create t.stderr : Lines_or_sexp.t)
        ~exit_status:(t.exit_status : Unix.Exit_or_signal.t)]
  ;;
end

let wait t = force t.wait

let collect_output_and_wait t =
  let stdout = Reader.contents t.stdout in
  let stderr = Reader.contents t.stderr in
  let%bind () = Writer.close t.stdin ~force_close:(Deferred.never ()) in
  let%bind exit_status = wait t in
  let%bind stdout in
  let%bind stderr in
  return { Output.stdout; stderr; exit_status }
;;

module Failure = struct
  let should_drop_env = function
    | `Extend [] | `Override [] -> true
    | `Extend (_ :: _) | `Override (_ :: _) | `Replace _ | `Replace_raw _ -> false
  ;;

  type t =
    { prog : string
    ; args : string list
    ; working_dir : string option [@sexp.option]
    ; env : env [@sexp_drop_if should_drop_env]
    ; exit_status : Unix.Exit_or_signal.error
    ; stdout : Lines_or_sexp.t
    ; stderr : Lines_or_sexp.t
    }
  [@@deriving sexp_of]
end

let handle_exit_status ?(accept_nonzero_exit = []) = function
  | Ok _ as ok -> ok
  | Error (`Exit_non_zero n) when List.mem accept_nonzero_exit n ~equal:Int.equal -> Ok ()
  | Error _ as e -> e
;;

let collect_stdout_and_wait ?accept_nonzero_exit t =
  let%map { stdout; stderr; exit_status } = collect_output_and_wait t in
  match handle_exit_status ?accept_nonzero_exit exit_status with
  | Ok () -> Ok stdout
  | Error exit_status ->
    let { prog; args; working_dir; env; _ } = t in
    Or_error.error
      "Process.run failed"
      { Failure.prog
      ; args
      ; working_dir
      ; env
      ; exit_status
      ; stdout = Lines_or_sexp.create stdout
      ; stderr = Lines_or_sexp.create stderr
      }
      [%sexp_of: Failure.t]
;;

let map_collect collect f ?accept_nonzero_exit t =
  let%map a = collect ?accept_nonzero_exit t in
  f a
;;

let collect_stdout_and_wait_exn = map_collect collect_stdout_and_wait ok_exn

let collect_stdout_lines_and_wait =
  map_collect collect_stdout_and_wait (Or_error.map ~f:String.split_lines)
;;

let collect_stdout_lines_and_wait_exn = map_collect collect_stdout_lines_and_wait ok_exn

let run
  ?accept_nonzero_exit
  ?argv0
  ?env
  ?prog_search_path
  ?stdin
  ?working_dir
  ~prog
  ~args
  ()
  =
  match%bind create ?argv0 ?env ?prog_search_path ?stdin ?working_dir ~prog ~args () with
  | Error _ as e -> return e
  | Ok t -> collect_stdout_and_wait ?accept_nonzero_exit t
;;

let map_run
  run
  f
  ?accept_nonzero_exit
  ?argv0
  ?env
  ?prog_search_path
  ?stdin
  ?working_dir
  ~prog
  ~args
  ()
  =
  let%map a =
    run
      ?accept_nonzero_exit
      ?argv0
      ?env
      ?prog_search_path
      ?stdin
      ?working_dir
      ~prog
      ~args
      ()
  in
  f a
;;

let run_exn = map_run run ok_exn
let run_lines = map_run run (Or_error.map ~f:String.split_lines)
let run_lines_exn = map_run run_lines ok_exn

let run_expect_no_output
  ?accept_nonzero_exit
  ?argv0
  ?env
  ?prog_search_path
  ?stdin
  ?working_dir
  ~prog
  ~args
  ()
  =
  match%map
    run
      ?accept_nonzero_exit
      ?argv0
      ?env
      ?prog_search_path
      ?stdin
      ?working_dir
      ~prog
      ~args
      ()
  with
  | Error _ as err -> err
  | Ok "" -> Ok ()
  | Ok non_empty_output ->
    Or_error.error "Process.run_expect_no_output: non-empty output" () (fun () ->
      [%sexp { prog : string; args : string list; output = (non_empty_output : string) }])
;;

let run_expect_no_output_exn = map_run run_expect_no_output ok_exn

let transfer_and_close reader writer =
  Reader.with_close reader ~f:(fun () -> Writer.splice ~from:reader writer)
;;

let forward_output_and_wait ?accept_nonzero_exit t =
  let%map () = Writer.close t.stdin ~force_close:(Deferred.never ())
  and () = transfer_and_close t.stdout (Lazy.force Writer.stdout)
  and () = transfer_and_close t.stderr (Lazy.force Writer.stderr)
  and exit_status = wait t in
  match handle_exit_status ?accept_nonzero_exit exit_status with
  | Ok _ as ok -> ok
  | Error exit_status ->
    let { prog; args; working_dir; env; _ } = t in
    Or_error.error_s
      [%message
        "Process.run failed"
          (prog : string)
          (args : string list)
          (working_dir : string option)
          (env : env)
          (exit_status : Unix.Exit_or_signal.error)]
;;

let forward_output_and_wait_exn = map_collect forward_output_and_wait ok_exn

let run_forwarding_with_spliced_fds
  ?accept_nonzero_exit
  ?argv0
  ?env
  ?prog_search_path
  ?stdin
  ?working_dir
  ~prog
  ~args
  ()
  =
  match%bind create ?argv0 ?env ?prog_search_path ?stdin ?working_dir ~prog ~args () with
  | Error _ as e -> return e
  | Ok t -> forward_output_and_wait ?accept_nonzero_exit t
;;

let run_forwarding_with_shared_fds
  ?accept_nonzero_exit
  ?argv0
  ?env
  ?prog_search_path
  ?stdin:write_to_stdin
  ?working_dir
  ~prog
  ~args
  ()
  =
  let%bind () = Writer.flushed (Lazy.force Writer.stdout)
  and () = Writer.flushed (Lazy.force Writer.stderr) in
  match%bind
    In_thread.syscall ~name:"create_process_with_fds" (fun () ->
      Core_unix.create_process_with_fds
        ?argv0
        ?env
        ?prog_search_path
        ?working_dir
        ~prog
        ~args
        ~stdin:Generate
        ~stdout:(Use_this Core_unix.stdout)
        ~stderr:(Use_this Core_unix.stderr)
        ())
  with
  | Error exn -> Deferred.Or_error.of_exn exn
  | Ok { pid; stdin; stdout = `Did_not_create_fd; stderr = `Did_not_create_fd } ->
    let%map () =
      let writer = create_fd ~pid ~prog ~args "stdin" stdin |> Writer.create in
      Writer.with_close writer ~f:(fun () ->
        (match write_to_stdin with
         | None -> ()
         | Some write_to_stdin -> Writer.write writer write_to_stdin);
        Writer.flushed_or_failed_unit writer)
    and exit_status = Unix.waitpid_prompt pid in
    (match handle_exit_status ?accept_nonzero_exit exit_status with
     | Ok _ as ok -> ok
     | Error exit_status ->
       Or_error.error_s
         [%message
           "Process.run_forwarding_with_shared_fds failed"
             (prog : string)
             (args : string list)
             (working_dir : string option)
             (env : env option)
             (exit_status : Unix.Exit_or_signal.error)])
;;

let run_forwarding ?(child_fds = `Splice) =
  match child_fds with
  | `Splice -> run_forwarding_with_spliced_fds
  | `Share -> run_forwarding_with_shared_fds
;;

let run_forwarding_exn ?child_fds = map_run (run_forwarding ?child_fds) ok_exn

module How_to_handle_output = struct
  type ('output, 'child_fds) t =
    | Collect_stdout_and_wait : (string, 'child_fds) t
    | Collect_stdout_lines_and_wait : (string list, 'child_fds) t
    | Forward_output_and_wait : 'child_fds -> (unit, 'child_fds) t
end

let run' (type output)
  : (output, _) How_to_handle_output.t -> output Or_error.t Aliases.run
  = function
  | Collect_stdout_and_wait -> run
  | Collect_stdout_lines_and_wait -> run_lines
  | Forward_output_and_wait (`Share_fds_with_child share_fds_with_child) ->
    let child_fds =
      match share_fds_with_child with
      | true -> `Share
      | false -> `Splice
    in
    run_forwarding ~child_fds
;;

let run'_exn (type output) : (output, _) How_to_handle_output.t -> output Aliases.run
  = function
  | Collect_stdout_and_wait -> run_exn
  | Collect_stdout_lines_and_wait -> run_lines_exn
  | Forward_output_and_wait (`Share_fds_with_child share_fds_with_child) ->
    let child_fds =
      match share_fds_with_child with
      | true -> `Share
      | false -> `Splice
    in
    run_forwarding_exn ~child_fds
;;

let handle_output (type output)
  : (output, unit) How_to_handle_output.t -> output Or_error.t Aliases.collect
  = function
  | Collect_stdout_and_wait -> collect_stdout_and_wait
  | Collect_stdout_lines_and_wait -> collect_stdout_lines_and_wait
  | Forward_output_and_wait () -> forward_output_and_wait
;;

let handle_output_exn (type output)
  : (output, unit) How_to_handle_output.t -> output Aliases.collect
  = function
  | Collect_stdout_and_wait -> collect_stdout_and_wait_exn
  | Collect_stdout_lines_and_wait -> collect_stdout_lines_and_wait_exn
  | Forward_output_and_wait () -> forward_output_and_wait_exn
;;

let send_signal_internal t signal =
  (* We don't force the lazy (and therefore we don't reap the PID) here. We only do
     that if the user calls [wait] explicitly. *)
  if Lazy.is_val t.wait && Deferred.is_determined (Lazy.force t.wait)
  then
    (* The process was reaped, so it's not safe to send signals to this pid. *)
    `No_such_process_internal
  else (
    match Signal_unix.send signal (`Pid t.pid) with
    | `No_such_process ->
      (* Normally this should not be reachable: even for a zombie process (a process that
         has already been terminated, but wasn't waited for), the [kill] system call
         returns successfully. And we know that we haven't waited for this process because
         otherwise [t.wait] would have been determined.

         However, we do expose the [pid] so the users can and do sometimes call
         [Unix.waitpid] on that pid, which can still lead to the race we are trying to
         prevent.

         The right fix would be to prevent users from calling [waitpid] on our pid. *)
      `No_such_process_OS
    | `Ok -> `Ok)
;;

let send_signal_compat t signal =
  match send_signal_internal t signal with
  | `No_such_process_OS | `No_such_process_internal -> `No_such_process
  | `Ok -> `Ok
;;

let send_signal_compat_exn t signal =
  match (send_signal_compat t signal : [ `Ok | `No_such_process ]) with
  | `Ok -> ()
  | `No_such_process ->
    failwithf
      "Process.send_signal_compat_exn %s pid:%s"
      (Signal.to_string signal)
      (Pid.to_string t.pid)
      ()
;;

let send_signal t signal =
  ignore (send_signal_compat t signal : [ `Ok | `No_such_process ])
;;

module For_tests = struct
  let send_signal_internal = send_signal_internal
end

let create_with_shared_stderr
  ?argv0
  ?buf_len
  ?(env = `Extend [])
  ?prog_search_path
  ?stdin:write_to_stdin
  ?working_dir
  ?setpgid
  ~prog
  ~args
  ()
  =
  let%bind () = Writer.flushed (Lazy.force Writer.stderr) in
  match%map
    In_thread.syscall ~name:"create_process_with_fds" (fun () ->
      Core_unix.create_process_with_fds
        ~prog
        ~args
        ~env
        ?working_dir
        ?prog_search_path
        ?argv0
        ?setpgid
        ~stdin:Generate
        ~stdout:Generate
        ~stderr:(Use_this Core_unix.stderr)
        ())
  with
  | Error exn -> Or_error.of_exn exn
  | Ok { pid; stdin; stdout; stderr = `Did_not_create_fd } ->
    let dummy_reader =
      Reader.create
        (Fd.create
           File
           (Core_unix.openfile ~mode:[ O_RDONLY ] "/dev/null")
           (Info.of_string "shared stderr (dummy)"))
    in
    let create_fd name file_descr = create_fd ~pid ~prog ~args name file_descr in
    let stdin = create_stdin_writer (create_fd "stdin" stdin) ?buf_len ~write_to_stdin in
    let t =
      { pid
      ; stdin
      ; stdout = Reader.create ?buf_len (create_fd "stdout" stdout)
      ; stderr = dummy_reader
      ; prog
      ; args
      ; working_dir
      ; env
      ; wait = lazy (Unix.waitpid_prompt pid)
      }
    in
    Ok t
;;

let create_with_shared_stderr_exn
  ?argv0
  ?buf_len
  ?env
  ?prog_search_path
  ?stdin
  ?working_dir
  ?setpgid
  ~prog
  ~args
  ()
  =
  create_with_shared_stderr
    ?argv0
    ?buf_len
    ?env
    ?prog_search_path
    ?stdin
    ?working_dir
    ?setpgid
    ~prog
    ~args
    ()
  >>| ok_exn
;;

module Expert = struct
  let wrap_existing ~pid ~stdin ~stdout ~stderr ~prog ~args ~working_dir ~env ~wait =
    { pid; stdin; stdout; stderr; prog; args; working_dir; env; wait }
  ;;
end
