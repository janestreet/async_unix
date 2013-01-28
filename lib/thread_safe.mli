(** The [Thread_safe] module has functions that are safe to call from threads outside
    async. *)

open Core.Std
open Async_core

(** [am_holding_async_lock ()] returns true if the currently running thread is holding the
    async lock. *)
val am_holding_async_lock : unit -> bool

(** [deferred ()] returns [(d, fill)] where [d] is a deferred that will become determined
    with value [v] once [fill v] is called.

    It is ok to call [deferred] from inside or outside async.  [fill] must be called from
    outside async. *)
val deferred : unit -> 'a Deferred.t * ('a -> unit)

(** [pipe ()] returns [(reader, write, close)], where [reader] receives all of the values
    supplied to [write] and is closed when [close] is called.

    It is ok to call [pipe] from inside or outside async.  [write] and [close] must be
    called from threads outside async.

    [write] will block (i.e. pushback) based on [reader]'s pushback.

    It is ok to call [close] multiple times.  Only the first will have an effect.

    Calling [write] after [close] will raise. *)
val pipe : unit -> 'a Pipe.Reader.t * ('a -> unit) * (unit -> unit)

(** [run_in_async_with_optional_cycle f] acquires the async lock and runs [f ()] while
    holding the lock.  Depending on the result of [f], it may also run a cycle. *)
val run_in_async_with_optional_cycle
  :  (unit -> [ `Run_a_cycle | `Do_not_run_a_cycle ] * 'a)
  -> ('a, exn) Result.t

(** [run_in_async f] acquires the async lock and runs [f ()] and an async cycle while
    holding the lock.  It returns the result of [f ()] to the outside world.  The caller
    is blocked until the cycle is complete.

    [run_in_async] does not automatically start the async scheduler.  You still need to
    call [Scheduler.go] elsewhere in your program.

    [run_in_async] runs an async cycle in its thread to give good latency -- we
    immediately run the async code that depends on [f ()] without needing another
    thread. *)
val run_in_async     : (unit -> 'a) -> ('a, exn) Result.t
val run_in_async_exn : (unit -> 'a) ->  'a

(** [block_on_async f] runs [f ()] in the async world and blocks until the result becomes
    determined.  This function can be called from the main thread or from a thread outside
    async.  It will automatically start the scheduler if it isn't already running. *)
val block_on_async     : (unit -> 'a Deferred.t) -> ('a, exn) Result.t
val block_on_async_exn : (unit -> 'a Deferred.t) ->  'a

(** [run_in_async_wait f] is like [block_on_async f], except that it will raise an
    exception if it is called from the main thread as well as from within async.  Upon
    returning from [run_in_async_wait], it is guaranteed that the caller does not have the
    async lock.  For experts only; casual users should stick with [block_on_async]. *)
val run_in_async_wait     : (unit -> 'a Deferred.t) -> ('a, exn) Result.t
val run_in_async_wait_exn : (unit -> 'a Deferred.t) ->  'a
