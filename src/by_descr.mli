(** [Fd_by_descr] is a table of the open [Fd.t]s, indexed by file descriptor number.

    In this interface, we use [Raw_fd.t] rather than [Fd.t] to avoid a dependency cycle,
    because the [Fd] module can't be defined yet. *)

open! Core
open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S with type t := Raw_fd.t t

val create : num_file_descrs:int -> 'a t
val capacity : 'a t -> int
val add : 'a t -> File_descr.t -> 'a -> unit Or_error.t
val mem : 'a t -> File_descr.t -> bool
val find : 'a t -> File_descr.t -> 'a option
val find_exn : 'a t -> File_descr.t -> 'a
val remove : 'a t -> File_descr.t -> unit
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val iter : 'a t -> f:('a -> unit) -> unit
val exists : 'a t -> f:('a -> bool) -> bool
