(** A thread-safe pipe is a thread-safe interface to the write end of a normal
    [Async.Pipe].  All operations except for [create] must be called from threads outside
    Async.  [create] can be called from inside or outside Async.

    For [Pipe] functions that return a [unit Deferred.t], the analog in [Thread_safe_pipe]
    blocks.

    For documentation of [wakeup_scheduler], see the {!Thread_safe} module. *)

open Core.Std
open Import

type 'a t with sexp_of

(** [create ()] returns a reader end, which must be used inside Async, and a writer end,
    which must be used outside Async.  [create] can be called inside or outside Async. *)
val create : unit -> 'a Pipe.Reader.t * 'a t

(** All the following functions must be called outside Async.  They behave as their
    counterpart in the {!Pipe} module. *)

(** [pushback writer] blocks the current thread until the pipe is empty or closed. *)
val pushback : _ t -> unit

(** [write_without_pushback'] and [write_without_pushback] transfer the element(s) into
    the pipe and return immediately. *)
val write_without_pushback'
  :  ?wakeup_scheduler:bool  (** default is [true] *)
  -> 'a t
  -> 'a Queue.t
  -> unit
val write_without_pushback
  :  ?wakeup_scheduler:bool  (** default is [true] *)
  -> 'a t
  -> 'a
  -> unit

(** [write'] and [write] transfer the element(s) into the pipe and block the current
    thread until the pipe is empty or closed (like {!pushback}). *)
val write' : 'a t -> 'a Queue.t -> unit
val write  : 'a t -> 'a         -> unit

val close : _ t -> unit
val is_closed : _ t -> bool

(** [closed writer] blocks the current thread until the pipe is closed. *)
val closed : _ t -> unit
