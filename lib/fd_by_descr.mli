(** [Fd_by_descr] is a table of the open [Fd.t]s, indexed by file descriptor number.

    In this interface, we use [Raw_fd.t] rather than [Fd.t] to avoid a dependency cycle,
    because the [Fd] module can't be defined yet. *)

open Core.Std
open Import

type t with sexp_of

val invariant : t -> unit

val create : num_file_descrs:int -> t

val add : t -> Raw_fd.t -> unit

val find : t -> File_descr.t -> Raw_fd.t option

val remove : t -> Raw_fd.t -> unit
