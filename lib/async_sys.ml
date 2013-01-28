open Core.Std
open Import

let argv = Sys.argv
let executable_name = Sys.executable_name

let run = In_thread.run

let wrap1 f x1 = run (fun () -> f x1)
let wrap2 f x1 x2 = run (fun () -> f x1 x2)

let file_exists = wrap1 Sys.file_exists
let file_exists_exn = wrap1 Sys.file_exists_exn

let when_file_exists ?(poll_delay = sec 0.5) file =
  Deferred.create (fun i ->
    let rec loop () =
      file_exists file >>> function
        | `Yes -> Ivar.fill i ()
        | `No -> upon (Clock.after poll_delay) loop
        | `Unknown ->
          failwiths "when_file_exists can not check file" file <:sexp_of< string >>
    in
    loop ())
;;

let is_directory = wrap1 Sys.is_directory
let is_file = wrap1 Sys.is_file
let remove = wrap1 Sys.remove
let rename = wrap2 Sys.rename
let command = wrap1 Sys.command
let chdir = wrap1 Sys.chdir
let getcwd = wrap1 Sys.getcwd
let readdir = wrap1 Sys.readdir

let interactive = Sys.interactive
let os_type = Sys.os_type
let word_size = Sys.word_size
