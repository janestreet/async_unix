(** The [Thread_safe] module has functions that are safe to call from threads outside
    async.

    All the [Thread_safe.block*] and [Thread_safe.run*] functions wake up the async
    scheduler to ensure that it continues in a timely manned with whatever jobs got
    started.
*)

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

(** [run_in_async_with_optional_cycle f] acquires the async lock and runs [f ()] while
    holding the lock.  Depending on the result of [f], it may also run a cycle. *)
val run_in_async_with_optional_cycle
  :  (unit -> [ `Run_a_cycle | `Do_not_run_a_cycle ] * 'a)
  -> ('a, exn) Result.t

(** [run_in_async f] acquires the async lock and runs [f ()] while holding the lock. It
    returns the result of [f ()] to the outside world.  The scheduler is woken up to
    ensures the code that depends on [f ()] is run soon enough.

    [run_in_async] doesn't run a cycle.

    [run_in_async] does not automatically start the async scheduler.  You still need to
    call [Scheduler.go] elsewhere in your program. *)
val run_in_async     : (unit -> 'a) -> ('a, exn) Result.t
val run_in_async_exn : (unit -> 'a) ->  'a

(** [block_on_async f] runs [f ()] in the async world and blocks until the result becomes
    determined.  This function can be called from the main thread or from a thread outside
    async.

    [block_on_async] will run a cycle if the deferred isn't determined, in the hope that
    running the cycle will cause the deferred to become determined.

    [block_on_async] will automatically start the scheduler if it isn't already
    running. *)
val block_on_async     : (unit -> 'a Deferred.t) -> ('a, exn) Result.t
val block_on_async_exn : (unit -> 'a Deferred.t) ->  'a

(** [run_in_async_wait f] is like [block_on_async f], except that it must be called from a
    thread outside async.  Upon returning from [run_in_async_wait], it is guaranteed that
    the caller does not have the async lock. *)
val run_in_async_wait     : (unit -> 'a Deferred.t) -> ('a, exn) Result.t
val run_in_async_wait_exn : (unit -> 'a Deferred.t) ->  'a
