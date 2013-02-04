(** [Fd_by_descr] is a table of the open [Fd.t]s, indexed by file descriptor number.

    In this interface, we use [Raw_fd.t] rather than [Fd.t] to avoid a dependency cycle,
    because the [Fd] module can't be defined yet. *)

open Core.Std
open Import

type t with sexp_of

include Invariant.S with type t := t

val create : num_file_descrs:int -> t

(** [add_exn t fd] fails if the file descriptor for [fd] is already in [t].  *)
val add_exn : t -> Raw_fd.t -> unit

val find : t -> File_descr.t -> Raw_fd.t option

val remove : t -> Raw_fd.t -> unit

val fold : t -> init:'a -> f:('a -> Raw_fd.t -> 'a) -> 'a

val iter : t -> f:(Raw_fd.t -> unit) -> unit
