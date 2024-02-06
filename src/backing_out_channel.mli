(** [Backing_out_channel] generalizes [Out_channel] to a narrow interface that can be used
    to collect strings, etc. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val of_out_channel : Out_channel.t -> t

val create
  :  output:(bigstring -> pos:int -> len:int -> unit)
  -> flush:(unit -> unit)
  -> sexp:(unit -> Sexp.t)
  -> t

val output_iovec : t -> Bigstring.t Core_unix.IOVec.t -> unit
val flush : t -> unit
