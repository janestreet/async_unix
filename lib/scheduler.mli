(** Threading model:

    Only one thread is running Async code at a time.  This is enforced by a single lock in
    Async's scheduler data structure.  There are any number of threads running code
    without holding the lock that get data from the outside world and want to affect the
    Async world.  They do this by calling [Thread_safe.run_in_async*], which acquires the
    lock, does a computation (e.g. fills an ivar), and then runs a "cycle" of Async
    computations. *)

open Core.Std
open Import

(** [go ?raise_unhandled_exn ()] passes control to Async, at which point Async starts
    running handlers, one by one without interruption, until there are no more handlers to
    run.  When Async is out of handlers it blocks until the outside world schedules more
    of them.  Because of this, Async programs do not exit until [shutdown] is called.

    [go ()] calls [handle_signal Sys.sigpipe], which causes the SIGPIPE signal to be
    ignored.  Low-level syscalls (e.g. write) still raise EPIPE.

    If any async job raises an unhandled exception that is not handled by any monitor,
    async execution ceases.  Then, by default, async pretty prints the exception, and
    exits with status 1.  If you don't want this, pass [~raise_unhandled_exn:()], which
    will cause the unhandled exception to be raised to the caller of [go ()]. *)
val go
  :  ?raise_unhandled_exn:Bool.False_.default
  -> unit
  -> never_returns

(** [go_main] is like [go], except that one supplies a [main] function that will be run to
    initialize the async computation, and that [go_main] will fail if any async has been
    used prior to [go_main] being called. *)
val go_main
  :  ?raise_unhandled_exn:Bool.False_.default
  -> main:(unit -> unit)
  -> unit
  -> never_returns

type 'a with_options =
  ?block_group:Block_group.t
  -> ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> 'a

val current_execution_context : unit -> Execution_context.t

(** [within_context context f] runs [f ()] right now with the specified execution
    context.  If [f] raises, then the exception is sent to the monitor of [context], and
    [Error ()] is returned. *)
val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

(** [within' f ~block_group ~monitor ~priority] runs [f ()] right now, with the specified
    block group, monitor, and priority set as specified.  They will be reset to their
    original values when [f] returns.  If [f] raises, then the result of [within'] will
    never become determined, but the exception will end up in the specified monitor. *)
val within' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options

(** [within] is like [within'], but doesn't require thunk to return a deferred. *)
val within : ((unit -> unit) -> unit) with_options

(** [within_v] is like [within], but allows a value to be returned by [f]. *)
val within_v : ((unit -> 'a) -> 'a option) with_options

(** Just like [within'], but instead of running thunk right now, adds
    it to the async queue to be run with other async jobs. *)
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options

(** Just like schedule', but doesn't require thunk to return a deferred. *)
val schedule : ((unit -> unit) -> unit) with_options

(** [cycle_start ()] returns the result of [Time.now ()] called at the beginning of
    cycle. *)
val cycle_start : unit -> Time.t


(** [cycle_times ()] returns a stream that will have one element for each cycle that Async
    runs, with the amount of time that the cycle took (as determined by calls to Time.now
    at the beginning and end of the cycle). *)
val cycle_times : unit -> Time.Span.t Stream.t

(** [report_long_cycle_times ?cutoff ()] sets up something that will print a warning to
    stderr whenever there is an async cycle that is too long, as specified by [cutoff],
    whose default is 1s. *)
val report_long_cycle_times : ?cutoff:Time.Span.t -> unit -> unit

(** [cycle_count ()] returns the total number of async cycles since Scheduler.go was
    called *)
val cycle_count : unit -> int

(** [is_running ()] returns true if Scheduler.go has been called. *)
val is_running : unit -> bool

(** [num_pending_jobs ()] returns the number of jobs that are scheduled that haven't
    yet been run. *)
val num_pending_jobs : unit -> int

(** [set_max_num_jobs_per_priority_per_cycle int] sets the maximum number of jobs that
    will be done at each priority within each async cycle.  The default is [500]. *)
val set_max_num_jobs_per_priority_per_cycle : int -> unit

val is_ready_to_initialize : unit -> bool

