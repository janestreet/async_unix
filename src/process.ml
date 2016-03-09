open Core.Std
open Import

module Unix = Unix_syscalls

type env = Core.Std.Unix.env [@@deriving sexp]

type t =
  { pid         : Pid.t
  ; stdin       : Writer.t
  ; stdout      : Reader.t
  ; stderr      : Reader.t
  ; prog        : string
  ; args        : string list
  ; working_dir : string option
  ; env         : env
  }
[@@deriving fields, sexp_of]

type 'a create
  =  ?env         : env
  -> ?working_dir : string
  -> prog         : string
  -> args         : string list
  -> unit
  -> 'a Deferred.t

let create ?(env = `Extend []) ?working_dir ~prog ~args () =
  In_thread.syscall ~name:"create_process_env" (fun () ->
    Core.Std.Unix.create_process_env ~prog ~args ~env ?working_dir ())
  >>| function
  | Error exn -> Or_error.of_exn exn
  | Ok { pid; stdin; stdout; stderr } ->
    let create_fd name file_descr =
      Fd.create Fifo file_descr
        (Info.create "child process" ~here:[%here] (name, `pid pid, `prog prog, `args args)
           [%sexp_of:
               string * [ `pid of Pid.t ] * [ `prog of string ] * [ `args of string list ]
            ])
    in
    Ok { pid
       ; stdin  = Writer.create (create_fd "stdin"  stdin )
       ; stdout = Reader.create (create_fd "stdout" stdout)
       ; stderr = Reader.create (create_fd "stderr" stderr)
       ; prog
       ; args
       ; working_dir
       ; env
       }
;;

let create_exn ?env ?working_dir ~prog ~args () =
  create ?env ?working_dir ~prog ~args () >>| ok_exn
;;

module Output = struct
  module Stable = struct
    module V1 = struct
      type t =
        { stdout      : string
        ; stderr      : string
        ; exit_status : Unix.Exit_or_signal.t
        }
      [@@deriving compare, sexp]
    end
  end

  include Stable.V1
end

let wait t = Unix.waitpid t.pid

let collect_output_and_wait t =
  let stdout = Reader.contents t.stdout in
  let stderr = Reader.contents t.stderr in
  Writer.close t.stdin
  >>= fun () ->
  wait t
  >>= fun exit_status ->
  stdout
  >>= fun stdout ->
  stderr
  >>= fun stderr ->
  return { Output. stdout; stderr; exit_status }
;;

module Lines_or_sexp = struct
  type t =
    | Lines of string list
    | Sexp of Sexp.t
  [@@deriving sexp_of]

  let create string =
    try Sexp (Sexp.of_string (String.strip string))
    with _ -> Lines (String.split ~on:'\n' string)
  ;;
end

module Failure = struct
  type t =
    { prog        : string
    ; args        : string list
    ; working_dir : string option
    ; env         : env
    ; exit_status : Unix.Exit_or_signal.error
    ; stdout      : Lines_or_sexp.t
    ; stderr      : Lines_or_sexp.t
    }
  [@@deriving sexp_of]
end

type 'a collect
  =  ?accept_nonzero_exit : int list
  -> t
  -> 'a Deferred.t

let collect_stdout_and_wait ?(accept_nonzero_exit = []) t =
  collect_output_and_wait t
  >>| fun { stdout; stderr; exit_status } ->
  match exit_status with
  | Ok () -> Ok stdout
  | Error (`Exit_non_zero n) when List.mem accept_nonzero_exit n -> Ok stdout
  | Error exit_status ->
    let { prog; args; working_dir; env; _ } = t in
    Or_error.error "Process.run failed"
      { Failure.
        prog; args; working_dir; env; exit_status
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

let%test_unit "first arg is not prog" =
  let args = [ "219068700202774381" ] in
  [%test_pred: string list Or_error.t]
    (Poly.equal (Ok args))
    (Thread_safe.block_on_async_exn
       (fun () ->
          create ~prog:"echo" ~args ()
          >>=? collect_stdout_lines_and_wait))
;;

type 'a run
  =  ?accept_nonzero_exit : int list
  -> ?env                 : env
  -> ?working_dir         : string
  -> prog                 : string
  -> args                 : string list
  -> unit
  -> 'a Deferred.t

let run ?accept_nonzero_exit ?env ?working_dir ~prog ~args () =
  create ?env ?working_dir ~prog ~args ()
  >>= function
  | Error _ as e -> return e
  | Ok t -> collect_stdout_and_wait ?accept_nonzero_exit t
;;

let map_run run f ?accept_nonzero_exit ?env ?working_dir ~prog ~args () =
  let%map a = run ?accept_nonzero_exit ?env ?working_dir ~prog ~args () in
  f a
;;

let run_exn = map_run run ok_exn

let run_lines = map_run run (Or_error.map ~f:String.split_lines)

let run_lines_exn = map_run run_lines ok_exn

let run_expect_no_output ?accept_nonzero_exit ?env ?working_dir ~prog ~args () =
  run ?accept_nonzero_exit ?env ?working_dir ~prog ~args ()
  >>| function
  | Error _ as err      -> err
  | Ok ""               -> Ok ()
  | Ok non_empty_output ->
    Or_error.error "Process.run_expect_no_output: non-empty output" () (fun () ->
      [%sexp
        { prog   = (prog             : string)
        ; args   = (args             : string list)
        ; output = (non_empty_output : string)
        }])
;;

let run_expect_no_output_exn = map_run run_expect_no_output ok_exn
