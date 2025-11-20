open! Core
open Import

type t = Io_uring_raw.t

let create = Io_uring_raw.create
let exit = Io_uring_raw.exit
let submit = Io_uring_raw.submit
let fill_completions = Io_uring_raw.fill_completions
let the_one_and_only = Io_uring_raw_singleton.the_one_and_only
let max_tries = 1000

let rec attempt_syscall_internal f count =
  if count = max_tries then failwith "syscall interrupted too many times";
  match%bind Io_uring_raw.syscall_result_noretry (f ()) with
  | Error Unix.Error.EINTR ->
    (* We don't know if io_uring completions can actually return [EINTR] (probably not?),
       so this is possibly dead code. To be on the safe side, we're just replicating the
       traditional retry loop from [Syscall.syscall]. *)
    let%bind () = Raw_scheduler.yield () in
    attempt_syscall_internal f (count + 1)
  | Error (Unix.EUNKNOWNERR 125) ->
    (* We've seen some weird behavior where our calls return with ECANCELED even though
       we're not asking to cancel. Work around this issue, which seems like it may be a
       kernel bug.

       This can't be a real cancellation because the job handle is made by [f ()] above
       and we don't ever call cancel on that. *)
    attempt_syscall_internal f (count + 1)
  | Error err -> return (Error err)
  | Ok result -> return (Ok result)
;;

let attempt_syscall f = attempt_syscall_internal f 0

let with_file_descr_deferred ~name fd f =
  match%map Fd.with_file_descr_deferred ~extract_exn:true fd (fun fd -> f fd) with
  | `Already_closed ->
    (* We have to match the error messages of [Fd.syscall_in_thread_exn] because if we
       default [Async] to using [Io_uring], inline tests that catch error messages will
       start failing. *)
    Error
      (try
         raise_s
           [%message "Fd.syscall_in_thread_exn of a closed fd" name ~_:(fd : Fd.t_hum)]
       with
       | exn -> exn)
  | `Error exn -> raise exn
  | `Ok ok -> ok
;;

let with_file_descr_deferred_opt ~name fd_opt ~f =
  match fd_opt with
  | None -> f None
  | Some fd -> with_file_descr_deferred ~name fd (fun fd -> f (Some fd))
;;

let read_file_descr_or_unix_error t ?(file_offset = -1) file_descr ?off ?len buf =
  attempt_syscall (fun () ->
    Io_uring_raw.read
      t
      ~file_offset:(Io_uring_raw.Int63.of_int file_offset)
      file_descr
      (Cstruct.of_bigarray ?off ?len buf))
;;

let read_file_descr t ?file_offset file_descr ?off ?len buf =
  match%map read_file_descr_or_unix_error t ?file_offset file_descr ?off ?len buf with
  | Error err -> Error (Unix.Unix_error (err, "read", ""))
  | Ok res -> Ok res
;;

let read t ?(file_offset = -1) fd ?off ?len buf =
  with_file_descr_deferred ~name:"read" fd (fun fd ->
    read_file_descr t ~file_offset fd ?off ?len buf)
;;

let write t ?(file_offset = -1) fd ?off ?len buf =
  with_file_descr_deferred ~name:"write" fd (fun fd ->
    match%map
      attempt_syscall (fun () ->
        Io_uring_raw.write
          t
          ~file_offset:(Io_uring_raw.Int63.of_int file_offset)
          fd
          (Cstruct.of_bigarray ?off ?len buf))
    with
    | Error err -> Error (Unix.Unix_error (err, "write", ""))
    | Ok res -> Ok res)
;;

let to_cstruct (iovecs : Bigstring.t Core_unix.IOVec.t array) =
  Array.to_list iovecs
  |> List.map ~f:(fun { buf; pos; len } -> Cstruct.of_bigarray ~off:pos ~len buf)
;;

let readv t ?(file_offset = -1) fd bufs =
  with_file_descr_deferred ~name:"readv" fd (fun fd ->
    match%map
      attempt_syscall (fun () ->
        Io_uring_raw.readv
          t
          ~file_offset:(Io_uring_raw.Int63.of_int file_offset)
          fd
          (to_cstruct bufs))
    with
    | Error err -> Error (Unix.Unix_error (err, "readv", ""))
    | Ok res -> Ok res)
;;

let writev t ?(file_offset = -1) fd (bufs : Bigstring.t Core_unix.IOVec.t array) =
  with_file_descr_deferred ~name:"writev" fd (fun fd ->
    match%map
      attempt_syscall (fun () ->
        Io_uring_raw.writev
          t
          ~file_offset:(Io_uring_raw.Int63.of_int file_offset)
          fd
          (to_cstruct bufs))
    with
    | Error err -> Error (Unix.Unix_error (err, "writev", ""))
    | Ok res -> Ok res)
;;

let openat2 t ~access ~flags ?(perm = 0o644) ~resolve ?info ?fd filename =
  let perm =
    let open Io_uring_raw.Open_flags in
    if mem creat flags || mem tmpfile flags then perm else 0
  in
  let info = Option.value info ~default:(Info.create_s [%sexp (filename : string)]) in
  let openat2_syscall fd_opt =
    attempt_syscall (fun () ->
      Io_uring_raw.openat2 t ~access ~flags ~perm ~resolve ?fd:fd_opt filename)
  in
  let unix_error err = Unix.Unix_error (err, "open", Info.to_string_mach info) in
  let success_fd res = Fd.create Fd.Kind.File (File_descr.of_int res) info in
  if String.contains filename '\000'
  then return (Error (unix_error ENOENT))
  else
    with_file_descr_deferred_opt ~name:"open" fd ~f:(fun fd ->
      match%map openat2_syscall fd with
      | Error err -> Error (unix_error err)
      | Ok res -> Ok (success_fd res))
;;

let unlink t ~dir ?fd filename =
  let to_exn err =
    Unix.Unix_error
      (err, "unlink", Core_unix.Private.sexp_to_string_hum [%sexp { filename : string }])
  in
  if String.contains filename '\000'
  then return (Error (to_exn ENOENT))
  else (
    let unlink_syscall fd_opt =
      attempt_syscall (fun () -> Io_uring_raw.unlink t ~dir ?fd:fd_opt filename)
    in
    with_file_descr_deferred_opt fd ~name:"unlink" ~f:(fun fd ->
      match%map unlink_syscall fd with
      | Error err -> Error (to_exn err)
      | Ok _ -> Ok ()))
;;

(* The [force] case for this function is implemented this way in order to align with the
   [Unix_syscalls] error message which outputs the arguments of the [link] in the case
   [unlink] fails.
*)
let link t ?(follow = false) ?(force = false) ~target ~link_name () =
  let args_for_error () =
    Core_unix.Private.sexp_to_string_hum [%sexp { target : string; link_name : string }]
  in
  let unix_error s e = Error (Unix.Unix_error (e, s, args_for_error ())) in
  if String.contains target '\000' || String.contains link_name '\000'
  then return (unix_error "link" ENOENT)
  else (
    let%bind unlink_res =
      match force with
      | true ->
        (match%map unlink t ~dir:false link_name with
         | Error (Unix.Unix_error (Unix.ENOENT, _, _)) -> Ok ()
         | Error (Unix.Unix_error (e, s, _)) -> unix_error s e
         | Error exn -> Error exn
         | Ok () -> Ok ())
      | false -> return (Ok ())
    in
    match unlink_res with
    | Error exn -> return (Error exn)
    | Ok () ->
      (match%map
         attempt_syscall (fun () -> Io_uring_raw.link t ~follow ~target ~link_name)
       with
       | Error err -> unix_error "link" err
       | Ok _ -> Ok ()))
;;

let do_statx t ?fd ?(mask = Io_uring_raw.Statx.Mask.basic_stats) path flags =
  if String.contains path '\000'
  then return (Error (ENOENT : Core_unix.Error.t))
  else (
    let statx_buffer = Io_uring_raw.Statx.create () in
    match%map
      attempt_syscall (fun () -> Io_uring_raw.statx t ?fd ~mask path statx_buffer flags)
    with
    | Error err -> Error err
    | Ok res ->
      assert (res = 0);
      Ok statx_buffer)
;;

let statx t ?fd ?(mask = Io_uring_raw.Statx.Mask.basic_stats) path flags =
  let statx_syscall fd_opt = do_statx t ?fd:fd_opt ~mask path flags in
  let unix_error err =
    Unix.Unix_error
      ( err
      , "statx"
      , Core_unix.Private.sexp_to_string_hum [%sexp { fd : Fd.t option; path : string }]
      )
  in
  with_file_descr_deferred_opt ~name:"statx" fd ~f:(fun fd ->
    match%map statx_syscall fd with
    | Error err -> Error (unix_error err)
    | Ok res -> Ok res)
;;

let stat_or_unix_error t ?mask filename =
  do_statx t ?mask filename Io_uring_raw.Statx.Flags.empty
;;

let stat t ?mask filename =
  let unix_error err =
    Unix.Unix_error
      (err, "stat", Core_unix.Private.sexp_to_string_hum [%sexp { filename : string }])
  in
  match%map stat_or_unix_error t ?mask filename with
  | Error err -> Error (unix_error err)
  | Ok res -> Ok res
;;

let fstat t ?mask fd =
  with_file_descr_deferred ~name:"fstat" fd (fun fd ->
    let unix_error err =
      Unix.Unix_error
        (err, "fstat", Core_unix.Private.sexp_to_string_hum [%sexp { fd : File_descr.t }])
    in
    match%map do_statx t ?mask ~fd "" Io_uring_raw.Statx.Flags.empty_path with
    | Ok res -> Ok res
    | Error err -> Error (unix_error err))
;;

let lstat_or_unix_error t ?mask filename =
  do_statx t ?mask filename Io_uring_raw.Statx.Flags.symlink_nofollow
;;

let lstat t ?mask filename =
  let unix_error err =
    Unix.Unix_error
      (err, "lstat", Core_unix.Private.sexp_to_string_hum [%sexp { filename : string }])
  in
  match%map lstat_or_unix_error t ?mask filename with
  | Ok res -> Ok res
  | Error err -> Error (unix_error err)
;;
