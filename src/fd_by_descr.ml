open Core
open Import

module Fd = Raw_fd
module Table = Bounded_int_table

type t = (File_descr.t, Fd.t) Table.t [@@deriving sexp_of]

let invariant t =
  try
    Table.invariant ignore Fd.invariant t;
    Table.iteri t ~f:(fun ~key:file_descr ~data:fd ->
      assert (file_descr = Fd.file_descr fd));
  with exn ->
    raise_s [%message "Fd_by_descr.invariant failure" (exn : exn) ~fd:(t : t)]
;;

let create ~num_file_descrs =
  Table.create
    ~num_keys:num_file_descrs
    ~key_to_int:File_descr.to_int
    ~sexp_of_key:File_descr.sexp_of_t
    ()
;;

let mem t file_descr = Table.mem t file_descr

let find     t file_descr = Table.find     t file_descr
let find_exn t file_descr = Table.find_exn t file_descr

let remove t (fd : Fd.t) = Table.remove t fd.file_descr

let add t (fd : Fd.t) =
  let file_descr = fd.file_descr in
  match Table.add t ~key:file_descr ~data:fd with
  | `Ok -> Ok ()
  | `Duplicate _ ->
    error_s [%message "\
Attempt to register a file descriptor with Async that Async believes it is already \
managing.  This likely indicates either a bug in Async or code outside of Async
manipulating a file descriptor that Async is managing."]
  | exception _ ->
    error_s [%message
      "\
The file descriptor number is larger than the maximum Async allows, which probably means \
that the program has created too many file descriptors without closing them.  \
You can cause Async to allow more file descriptors via the [ASYNC_CONFIG] environment \
variable, like this: \
ASYNC_CONFIG='((max_num_open_file_descrs <NUMBER>))' foo.exe arg1 arg2 ..."
        ~max_file_descriptor_number:(Table.num_keys t - 1 : int)]
;;

let fold t ~init ~f = Table.fold t ~init ~f:(fun ~key:_ ~data:fd a -> f a fd)

let iter t ~f = Table.iteri t ~f:(fun ~key:_ ~data:fd -> f fd)
