open Core.Std
open Import
open File_descr_watcher_intf
open Read_write.Export

module Table = Bounded_int_table

type t = (File_descr.t, unit) Table.t Read_write.t
with sexp_of

let backend = Config.File_descr_watcher.Select

let invariant t : unit =
  try
    Read_write.iter t ~f:(Table.invariant ignore ignore);
  with exn ->
    failwiths "Select_file_descr_watcher.invariant failed" (exn, t)
      (<:sexp_of< exn * t >>)
;;

let create ~num_file_descrs =
  Read_write.create_fn (fun () ->
    Table.create
      ~num_keys:num_file_descrs
      ~key_to_int:File_descr.to_int
      ~sexp_of_key:File_descr.sexp_of_t
      ())
;;

let reset_in_forked_process _ = ()

let iter t ~f =
  Read_write.iteri t ~f:(fun read_or_write table ->
    Table.iter table ~f:(fun ~key ~data:_ -> f key read_or_write))
;;

module Pre = struct
  type t = File_descr.t list Read_write.t
  with sexp_of
end

let set t file_descr desired =
  Read_write.iteri t ~f:(fun read_or_write table ->
    if Read_write.get desired read_or_write then
      Table.set table ~key:file_descr ~data:()
    else
      Table.remove table file_descr)
;;

let pre_check t = Read_write.map t ~f:Table.keys

module Check_result = struct
  type t =
    { pre : Pre.t;
      select_result : (Unix.Select_fds.t, exn) Result.t;
    }
  with sexp_of
end

let thread_safe_check _t pre ~timeout =
  let timeout =
    match timeout with
    | `Never | `Immediately as x -> x
    | `After span ->
      (* Wait no longer than one second, which avoids any weirdness due to feeding large
         timeouts to select. *)
      `After (Float.min 1. (Time.Span.to_sec span))
  in
  { Check_result.
    pre;
    select_result =
      Result.try_with (fun () ->
        Unix.select ~read:pre.read ~write:pre.write ~except:[] ~timeout ());
  }
;;

let post_check t ({ Check_result. pre; select_result } as check_result) =
  try
    match select_result with
    (* We think 514 should be treated like EINTR. *)
    | Error (Unix.Unix_error ((Unix.EINTR | Unix.EUNKNOWNERR 514), _, _)) ->
      `Syscall_interrupted
    | Ok { Unix.Select_fds. read; write; except } ->
      assert (List.is_empty except);
      if List.is_empty read && List.is_empty write then
        `Timeout
      else
        `Ok (Read_write.createi (fun read_or_write ->
          let ready =
            match read_or_write with
            | `Read -> read
            | `Write -> write
          in
          { Post. ready; bad = [] }))
    | Error (Unix.Unix_error (Unix.EBADF, _, _)) ->
      `Ok (Read_write.map pre ~f:(fun fds ->
        let bad =
          List.fold fds ~init:[] ~f:(fun ac file_descr ->
            match Syscall.syscall (fun () -> ignore (Unix.fstat file_descr)) with
            | Ok () -> ac
            | Error (Unix.Unix_error (Unix.EBADF, _, _)) -> file_descr :: ac
            | Error exn ->
              failwiths "fstat raised unexpected exn" (file_descr, exn)
                (<:sexp_of< File_descr.t * exn >>))
        in
        { Post. ready = []; bad }))
    | Error exn -> failwiths "select raised unexpected exn" exn <:sexp_of< exn >>
  with exn ->
    failwiths "File_descr_watcher.post_check bug" (exn, check_result, t)
      (<:sexp_of< exn * Check_result.t * t >>)
;;
