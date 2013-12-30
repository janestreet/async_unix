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

(** [exit ?force status] is [shutdown ?force status; Deferred.never ()].

    We do not have an exit function that returns a non-deferred:

    {[
      val exit : ?force:unit Deferred.t -> int -> _
    ]}

    Such a function should not exist, for the same reason that we do not have:

    {[
      val block : 'a Deferred.t -> 'a
    ]}

    The semantics of such an exit function would allow one to block a running Async job,
    and to switch to another one (to run the [at_shutdown] handlers), without expressing
    that switch in the type system via a [Deferred.t].  That would eliminate all the nice
    reasoning guarantees that Async gives about concurrent jobs. *)
val exit : ?force:unit Deferred.t -> int -> _ Deferred.t

(** [shutting_down ()] reports whether we are currently shutting down, and if so, with
    what status. *)
val shutting_down : unit -> [ `No | `Yes of int ]

(** [at_shutdown f] causes [f ()] to be run when [shutdown] is called, and for [shutdown]
    to wait until the returned deferred finishes.

    If [shutdown] has already been called, then calling [at_shutdown f] does nothing. *)
val at_shutdown : (unit -> unit Deferred.t) -> unit

(** [don't_finish_before d] causes [shutdown] to wait until [d] becomes determined before
    finishing.  It is like [at_shutdown (fun _ -> d)], except it is more efficient, and
    will not take any space once [d] is determined.  There is a a single [at_shutdown]
    shared among all deferreds supplied to [don't_finish_before]. *)
val don't_finish_before : unit Deferred.t -> unit
