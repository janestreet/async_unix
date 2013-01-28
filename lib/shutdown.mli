open Core.Std
open Import

(** [shutdown ?force status] initiates shutdown, which runs all the [at_shutdown]
    functions, waits for them to finish, and then exits with the supplied status.  The
    [at_shutdown] functions can block -- one can use [~force] to forcibly exit (with
    status 1) if the [at_shutdown] functions do not finish in a reasonable amount of time.

    By default, [force] is [after (sec 10.)].

    Repeated calls to [shutdown] with the same status will have no effect.  Any call to
    [shutdown] with nonzero status will cause that to be the status that is exited with.
    A call to [shutdown] with different nonzero status from the original call will
    raise. *)
val shutdown : ?force:unit Deferred.t -> int -> unit

(** [shutting_down ()] reports whether we are currently shutting down, and if so, with
    what status. *)
val shutting_down : unit -> [ `No | `Yes of int ]


(** [shutdown_and_raise ?force status] initiates shutdown and immediately raises. *)
val shutdown_and_raise : ?force:unit Deferred.t -> int -> never_returns

(** [at_shutdown f] causes [f ()] to be run when [shutdown] is called, and for [shutdown]
    to wait until the returned deferred finishes. *)
val at_shutdown : (unit -> unit Deferred.t) -> unit
