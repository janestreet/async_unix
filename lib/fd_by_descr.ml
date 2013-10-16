open Core.Std
open Import

module Fd = Raw_fd
module Table = Bounded_int_table

type t = (File_descr.t, Fd.t) Table.t with sexp_of

let invariant t =
  try
    Table.invariant ignore Fd.invariant t;
    Table.iter t ~f:(fun ~key:file_descr ~data:fd ->
      assert (file_descr = Fd.file_descr fd));
  with exn ->
    failwiths "Fd_by_descr.invariant failure" (exn, t) <:sexp_of< exn * t >>
;;

let create ~num_file_descrs =
  Table.create
    ~num_keys:num_file_descrs
    ~key_to_int:File_descr.to_int
    ~sexp_of_key:File_descr.sexp_of_t
    ()
;;

let find t file_descr = Table.find t file_descr

let remove t fd = Table.remove t fd.Fd.file_descr

let add_exn t fd =
  match Table.add t ~key:fd.Fd.file_descr ~data:fd with
  | `Ok -> ()
  | `Duplicate _ ->
    failwiths "\
Error in Async.Fd_by_descr.add_exn: attempt to register a file descriptor with async
that async believes it is already managing.  This likely indicates either:

  * a bug in async
  * code outside of async manipulating a file descriptor that async is managing
"
      (fd, t) <:sexp_of< Fd.t * t >>
;;

let fold t ~init ~f = Table.fold t ~init ~f:(fun ~key:_ ~data:fd a -> f a fd)

let iter t ~f = Table.iter t ~f:(fun ~key:_ ~data:fd -> f fd)
