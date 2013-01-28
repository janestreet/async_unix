open Core.Std
open Import

module Fd = Raw_fd
module Table = Bounded_int_table

type t = (File_descr.t, Fd.t) Table.t with sexp_of

let invariant t =
  try
    Table.iter t ~f:(fun ~key:file_descr ~data:fd ->
      assert (file_descr = Fd.file_descr fd);
      Fd.invariant fd);
  with exn ->
    failwiths "Fd_by_descr.invariant failure" exn <:sexp_of< exn >>
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

let add t fd =
  match Table.add t ~key:fd.Fd.file_descr ~data:fd with
  | `Ok -> ()
  | `Duplicate ->
    failwiths "attempt to overwrite existing fd" (fd, t) <:sexp_of< Fd.t * t >>
;;
