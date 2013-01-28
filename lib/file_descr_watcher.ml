open Core.Std
open Import

module File_descr = Unix.File_descr
module Table = Bounded_int_table

module State = struct
  type 'a t =
  | Watching of 'a
  | Stop_requested
  with sexp_of
end

open State

module Entry = struct
  type ('a, 'watching) t =
    { mutable state : 'watching State.t;
      value : 'a;
    }
  with fields, sexp_of

  let am_watching t =
    match t.state with
    | Watching _ -> true
    | Stop_requested -> false
  ;;

  let stop_requested t =
    match t.state with
    | Watching _ -> false
    | Stop_requested -> true
end

type ('a, 'watching) t = (File_descr.t, ('a, 'watching) Entry.t) Table.t Read_write.t
with sexp_of

open Read_write.Export

let table t read_or_write = Read_write.get t read_or_write

let entry t read_or_write file_descr = Table.find (table t read_or_write) file_descr

let state t file_descr read_or_write =
  Option.map (entry t read_or_write file_descr) ~f:Entry.state
;;

let iter t ~f =
  List.iter [ `Read; `Write ] ~f:(fun read_or_write ->
    Table.iter_vals (table t read_or_write) ~f:(fun entry ->
      f read_or_write entry.Entry.state entry.Entry.value))
;;

let invariant t =
  try
    Read_write.iter t ~f:Table.invariant;
  with exn ->
    failwiths "File_descr_watcher.invariant failed" (exn, t) <:sexp_of< exn * (a, b) t >>
;;

let create ~num_file_descrs =
  Read_write.create_fn (fun () ->
    Table.create
      ~num_keys:num_file_descrs
      ~key_to_int:File_descr.to_int
      ~sexp_of_key:File_descr.sexp_of_t
      ())
;;

let request_start_watching t file_descr value watching read_or_write =
  let table = table t read_or_write in
  match
    Table.add table ~key:file_descr ~data:{ Entry. state = Watching watching; value }
  with
  | `Ok -> `Ok
  | `Duplicate -> `Already_watching
;;

let request_stop_watching t file_descr read_or_write =
  match entry t read_or_write file_descr with
  | None -> `Was_not_watching
  | Some entry ->
    match entry.Entry.state with
    | State.Stop_requested -> `Was_not_watching
    | State.Watching watching ->
      entry.Entry.state <- Stop_requested;
      `Ok watching
;;

type 'a pre = File_descr.t list Read_write.t with sexp_of

let pre_check t =
  Read_write.map t ~f:(fun table ->
    Table.fold table ~init:[] ~f:(fun ~key:file_descr ~data:entry ac ->
      if Entry.am_watching entry then file_descr :: ac else ac))
;;

module Check_result = struct
  type 'a t =
    { pre : 'a pre;
      select_result : (Unix.Select_fds.t, exn) Result.t;
    }
  with sexp_of
end

type 'a check_result = 'a Check_result.t with sexp_of

let thread_safe_check _t pre ~timeout =
  { Check_result.
    pre;
    select_result =
      Result.try_with (fun () ->
        Unix.select ~read:pre.read ~write:pre.write ~except:[] ~timeout ());
  }
;;

type ('a, 'watching) post =
  { ready : ('a * 'watching) list;
    bad : ('a * 'watching) list;
    no_longer_watching : 'a list;
  }

let post_check t check_result =
  let pre = check_result.Check_result.pre in
  try
    let no_longer_watching table =
      let to_stop =
        Table.fold table ~init:[] ~f:(fun ~key ~data:entry ac ->
          if Entry.stop_requested entry then (key, entry) :: ac else ac)
      in
      List.fold to_stop ~init:[] ~f:(fun ac (key, entry) ->
        Table.remove table key;
        entry.Entry.value :: ac)
    in
    match check_result.Check_result.select_result with
    (* We think 514 should be treated like EINTR. *)
    | Error (Unix.Unix_error ((Unix.EINTR | Unix.EUNKNOWNERR 514), _, _)) -> `Retry
    | Ok { Unix.Select_fds. read; write; except } ->
      assert (List.is_empty except);
      `Ok (Read_write.mapi t ~f:(fun read_or_write table ->
        let ready =
          match read_or_write with
          | `Read -> read
          | `Write -> write
        in
        (* It is important to compute [ready] before [no_longer_watching], because
           computing the latter might remove entries from the tables that were being
           watched when select was called, and hence might be returned by select. *)
        let ready =
          List.fold ready ~init:[] ~f:(fun ac file_descr ->
            match Table.find table file_descr with
            | None ->
              failwiths "select returned unexpected ready file descr" file_descr
                (<:sexp_of< File_descr.t >>)
            | Some entry ->
              match Entry.state entry with
              | State.Stop_requested -> ac
              | State.Watching watching ->
                Table.remove table file_descr;
                (entry.Entry.value, watching) :: ac)
        in
        { ready;
          bad = [];
          no_longer_watching = no_longer_watching table;
        }))
    | Error (Unix.Unix_error (Unix.EBADF, _, _)) ->
      `Ok (Read_write.mapi t ~f:(fun read_or_write table ->
        (* It is important to compute [bad] before [no_longer_watching], because computing
           the latter might remove entries from the tables that were being watched when
           select was called, and hence might be returned by select. *)
        let bad =
          List.fold (Read_write.get pre read_or_write) ~init:[] ~f:(fun ac file_descr ->
            match Table.find table file_descr with
            | None ->
              failwiths "select returned unexpected bad file descr" file_descr
                (<:sexp_of< File_descr.t >>)
            | Some entry ->
              match Entry.state entry with
              | State.Stop_requested -> ac
              | State.Watching watching ->
                match Syscall.syscall (fun () -> ignore (Unix.fstat file_descr)) with
                | Ok () -> ac
                | Error (Unix.Unix_error (Unix.EBADF, _, _)) ->
                  Table.remove table file_descr;
                  (entry.Entry.value, watching) :: ac
                | Error exn ->
                  failwiths "fstat raised unexpected exn" (file_descr, exn)
                    (<:sexp_of< File_descr.t * exn >>))
        in
        { ready = [];
          bad;
          no_longer_watching = no_longer_watching table;
        }))
    | Error exn -> failwiths "select raised unexpected exn" exn <:sexp_of< exn >>
  with
  | exn ->
    failwiths "File_descr_watcher.post_check bug" (exn, check_result, t)
      (<:sexp_of< exn * a Check_result.t * (a, b) t >>)
;;



