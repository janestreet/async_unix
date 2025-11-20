open Core
open Import
module Unix = Unix_syscalls

let argv = (Sys.argv [@warning "-3"])
let get_argv = Sys.get_argv
let executable_name = Sys_unix.executable_name
let wrap1 f x1 = In_thread.run (fun () -> f x1)
let wrap2 f x1 x2 = In_thread.run (fun () -> f x1 x2)
let chdir = wrap1 Sys_unix.chdir
let command = wrap1 Sys_unix.command
let command_exn = wrap1 Sys_unix.command_exn
let quote = Sys.quote
let concat_quoted = Sys.concat_quoted
let getcwd = wrap1 Sys_unix.getcwd
let home_directory = wrap1 Sys_unix.home_directory
let ls_dir = wrap1 Sys_unix.ls_dir
let ls_dir_detailed = wrap1 Core_unix.ls_dir_detailed
let readdir = wrap1 Sys_unix.readdir
let remove = wrap1 Sys_unix.remove
let rename = wrap2 Sys_unix.rename

let raise_stat_exn ~follow_symlinks path err =
  let syscall_name = if follow_symlinks then "stat" else "lstat" in
  raise
    (Unix.Unix_error
       ( err
       , syscall_name
       , Core_unix.Private.sexp_to_string_hum [%sexp { filename : string = path }] ))
;;

let stat_check_exn f ?(follow_symlinks = true) path =
  let stat =
    if follow_symlinks
    then Unix_syscalls.stat_or_unix_error
    else Unix_syscalls.lstat_or_unix_error
  in
  stat path
  >>| function
  | Ok stat -> f stat
  | Error (ENOENT | ENOTDIR) -> false
  | Error err -> raise_stat_exn ~follow_symlinks path err
;;

let stat_check f ?(follow_symlinks = true) path =
  let stat =
    if follow_symlinks
    then Unix_syscalls.stat_or_unix_error
    else Unix_syscalls.lstat_or_unix_error
  in
  stat path
  >>| function
  | Ok stat -> if f stat then `Yes else `No
  | Error (ENOENT | ENOTDIR) -> `No
  | Error (EACCES | ELOOP) -> `Unknown
  | Error err -> raise_stat_exn ~follow_symlinks path err
;;

let file_exists = stat_check (fun _ -> true)
let file_exists_exn = stat_check_exn (fun _ -> true)

let is_directory =
  stat_check (fun stat -> [%equal: Unix.File_kind.t] stat.kind `Directory)
;;

let is_directory_exn =
  stat_check_exn (fun stat -> [%equal: Unix.File_kind.t] stat.kind `Directory)
;;

let is_file = stat_check (fun stat -> [%equal: Unix.File_kind.t] stat.kind `File)
let is_file_exn = stat_check_exn (fun stat -> [%equal: Unix.File_kind.t] stat.kind `File)

let is_symlink =
  stat_check
    (fun stat -> [%equal: Unix.File_kind.t] stat.kind `Link)
    ~follow_symlinks:false
;;

let is_symlink_exn =
  stat_check_exn
    (fun stat -> [%equal: Unix.File_kind.t] stat.kind `Link)
    ~follow_symlinks:false
;;

let when_file_changes
  ?(time_source = Time_source.wall_clock ())
  ?(poll_delay = sec 0.5)
  file
  =
  let last_reported_mtime = ref None in
  let reader, writer = Pipe.create () in
  let rec loop () =
    Monitor.try_with ~run:`Schedule ~rest:`Log ~extract_exn:true (fun () ->
      Unix.stat file)
    >>> fun stat_result ->
    if not (Pipe.is_closed writer)
    then (
      (match stat_result with
       | Error exn ->
         last_reported_mtime := None;
         Pipe.write_without_pushback writer (Error exn)
       | Ok st ->
         let mtime = st.mtime in
         let should_report =
           match !last_reported_mtime with
           | None -> true
           | Some last_reported_mtime -> not (Time.equal mtime last_reported_mtime)
         in
         if should_report
         then (
           last_reported_mtime := Some mtime;
           Pipe.write_without_pushback writer (Ok mtime)));
      Time_source.after time_source (Time_ns.Span.of_span_float_round_nearest poll_delay)
      >>> loop)
  in
  loop ();
  reader
;;

let when_file_exists ?follow_symlinks ?(poll_delay = sec 0.5) file =
  Deferred.create (fun i ->
    let rec loop () =
      file_exists ?follow_symlinks file
      >>> function
      | `Yes -> Ivar.fill_exn i ()
      | `No -> upon (Clock.after poll_delay) loop
      | `Unknown ->
        raise_s [%message "when_file_exists can not check file" (file : string)]
    in
    loop ())
;;

(* We redeclare everything from Core.Sys that we're just passing through here so that we
   are required to have everything enumerated and can consider whether it needs to be
   turned into an async version. *)
include struct
  open Core.Sys

  let interactive = interactive
  let os_type = os_type
  let unix = unix
  let win32 = win32
  let cygwin = cygwin

  type nonrec backend_type : value mod contended portable = backend_type =
    | Native
    | Bytecode
    | Other of string

  let backend_type = backend_type
  let word_size_in_bits = word_size_in_bits
  let int_size_in_bits = int_size_in_bits
  let big_endian = big_endian
  let max_string_length = max_string_length
  let max_array_length = max_array_length
  let runtime_variant = runtime_variant
  let runtime_parameters = runtime_parameters
  let ocaml_version = ocaml_version
  let enable_runtime_warnings = enable_runtime_warnings
  let runtime_warnings_enabled = runtime_warnings_enabled
  let getenv = getenv
  let getenv_exn = getenv_exn

  include (
    Base.Sys :
    sig
      (* It seems like just aliasing primitives doesn't satisfy the compiler, so this is
         brought in through [include] instead of a [let]. *)
      external opaque_identity
        : ('a : any).
        ('a[@local_opt]) -> ('a[@local_opt])
        = "%opaque"
      [@@layout_poly]

      external opaque_identity_global : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
    end)
end

include struct
  open Sys_unix

  let execution_mode = execution_mode
end

include (
  Sys_unix :
  sig
    (* It seems like just aliasing primitives doesn't satisfy the compiler, so this is
       brought in through [include] instead of a [let]. *)
    external c_int_size : unit -> int = "c_int_size" [@@noalloc]
  end)

let word_size = word_size_in_bits
let int_size = int_size_in_bits
