(* An interruptor provides a file descriptor that can be used to cause functions like
   [select] to return due the file descriptor being ready for reading. *)

open Core.Std
open Import

type t with sexp_of

val invariant : t -> unit

val create
  :  create_fd:(Raw_fd.Kind.t -> Unix.File_descr.t -> name:string -> Raw_fd.t)
  -> t

val read_fd : t -> Raw_fd.t

(** [thread_safe_interrupt t] causes [read_fd t] to become ready for reading. *)
val thread_safe_interrupt : t -> unit

(** [clear t] causes [read_fd t] to become not ready for reading.  It is guaranteed that
    any calls to [thread_safe_interrupt] after [clear t] returns (and prior to another
    call to [clear t]) will cause [read_fd] to become ready for reading. *)
val clear : t -> unit
