open Core.Std
open Import

module Unix = Unix_syscalls

type env = Core.Std.Unix.env with sexp

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
with fields, sexp_of

type 'a with_create_args =
     ?working_dir : string
  -> ?env : env
  -> prog : string
  -> args : string list
  -> unit
  -> 'a

let create ?working_dir ?(env = `Extend []) ~prog ~args () =
  In_thread.syscall ~name:"create_process_env" (fun () ->
    Core.Std.Unix.create_process_env ~prog ~args ~env ?working_dir ())
  >>| function
  | Error exn -> Or_error.of_exn exn
  | Ok { Core.Std.Unix.Process_info. pid; stdin; stdout; stderr } ->
    let create_fd name file_descr =
      Fd.create Fd.Kind.Fifo file_descr
        (Info.create "child process" ~here:_here_ (name, `pid pid, `prog prog, `args args)
           (<:sexp_of<
               string * [ `pid of Pid.t ] * [ `prog of string ] * [ `args of string list ]
            >>))
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

module Output = struct
  type t =
    { stdout : string;
      stderr : string;
      exit_status : Unix.Exit_or_signal.t;
    }
  with sexp_of
end

let wait t =
  let stdout = Reader.contents t.stdout in
  let stderr = Reader.contents t.stderr in
  Writer.close t.stdin
  >>= fun () ->
  Unix.waitpid t.pid
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
  with sexp_of

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
  with sexp_of
end

let wait_stdout ?(accept_nonzero_exit = []) t =
  wait t
  >>| fun { Output. stdout; stderr; exit_status } ->
  match exit_status with
  | Ok () -> Ok stdout
  | Error (`Exit_non_zero n) when List.mem accept_nonzero_exit n -> Ok stdout
  | Error exit_status ->
    let { prog; args; working_dir; env; _ } = t in
    Or_error.error "Process.run failed"
      { Failure.
        prog; args; working_dir; env; exit_status;
        stdout = Lines_or_sexp.create stdout;
        stderr = Lines_or_sexp.create stderr;
      }
      (<:sexp_of< Failure.t >>)
;;

let wait_stdout_lines ?accept_nonzero_exit t =
  wait_stdout ?accept_nonzero_exit t
  >>|? fun s ->
  String.split_lines s
;;

let run ?accept_nonzero_exit ?working_dir ?env ~prog ~args () =
  create ?working_dir ?env ~prog ~args ()
  >>= function
  | Error _ as e -> return e
  | Ok t -> wait_stdout ?accept_nonzero_exit t
;;

let run_lines ?accept_nonzero_exit ?working_dir ?env ~prog ~args () =
  run ?accept_nonzero_exit ?working_dir ?env ~prog ~args ()
  >>|? fun s ->
  String.split_lines s
;;
