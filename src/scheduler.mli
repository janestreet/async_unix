(** Dispatches and monitors Async processes.

    The threading model is as follows.  Only one thread runs Async code at a time.  This
    is enforced by a single lock in Async's scheduler data structure.  There are any
    number of threads running code without holding the lock that get data from the outside
    world and want to affect the Async world.  They do this by calling
    [Thread_safe.run_in_async*], which acquires the lock, does a computation (e.g., fills
    an ivar), and then runs a "cycle" of Async computations. *)

open! Core
open! Import

type t = Raw_scheduler.t [@@deriving sexp_of]

include module type of struct
  include Async_kernel_scheduler
end

module Which_watcher : sig
  module Custom : sig
    module type S =
      File_descr_watcher_intf.S
        with type 'a additional_create_args =
          handle_fd_read_bad:(File_descr.t -> unit)
          -> handle_fd_write_bad:(File_descr.t -> unit)
          -> 'a

    type t = (module S)
  end

  type t =
    | Config of Config.File_descr_watcher.t
    | Custom of Custom.t
end

(** [t ()] returns the Async scheduler.  If the scheduler hasn't been created yet, this
    will create it and acquire the Async lock. *)
val t : unit -> t

(** Accessors *)

val max_num_open_file_descrs : unit -> int
val max_num_threads : unit -> int

(** [go ?raise_unhandled_exn ()] passes control to Async, at which point Async starts
    running handlers, one by one without interruption, until there are no more handlers to
    run.  When Async is out of handlers, it blocks until the outside world schedules more
    of them.  Because of this, Async programs do not exit until [shutdown] is called.

    [go ()] calls [handle_signal Sys.sigpipe], which causes the SIGPIPE signal to be
    ignored.  Low-level syscalls (e.g., write) still raise EPIPE.

    If any Async job raises an unhandled exception that is not handled by any monitor,
    Async execution ceases.  Then, by default, Async pretty prints the exception, and
    exits with status 1.  If you don't want this, pass [~raise_unhandled_exn:true], which
    will cause the unhandled exception to be raised to the caller of [go ()]. *)
val go : ?raise_unhandled_exn:bool (** default is [false] *) -> unit -> never_returns

(** [go_main] is like [go], except that you supply a [main] function that will be run to
    initialize the Async computation, and that [go_main] will fail if any Async has been
    used prior to [go_main] being called.  Moreover it allows you to configure more static
    options of the scheduler. *)
val go_main
  :  ?raise_unhandled_exn:bool (** default is [false] *)
  -> ?file_descr_watcher:Which_watcher.t
       (** default is [Config Config.file_descr_watcher] *)
  -> ?max_num_open_file_descrs:int (** default is [Config] *)
  -> ?max_num_threads:int (** default is [Config] *)
  -> main:(unit -> unit)
  -> unit
  -> never_returns

(** [raise_if_any_jobs_were_scheduled ()] will raise an exception if any async work has
    ever been scheduled. This is intended to be called before a program starts, for
    instance before {!Command.run}, to ensure that no libraries have started async work.
    This can happen by mistake (by calling a deferred function at toplevel), which can
    make the behavior of the program unexpectedly non-deterministic. *)
val raise_if_any_jobs_were_scheduled : unit -> unit

(** [report_long_cycle_times ?cutoff ()] sets up something that will print a warning to
    stderr whenever there is an Async cycle that is too long, as specified by [cutoff],
    whose default is 1s. *)
val report_long_cycle_times : ?cutoff:Time.Span.t -> unit -> unit

(** [is_running ()] returns true if the scheduler has been started. *)
val is_running : unit -> bool

(** [set_max_inter_cycle_timeout span] sets the maximum amount of time the scheduler will
    remain blocked (on epoll or select) between cycles. *)
val set_max_inter_cycle_timeout : Time.Span.t -> unit

(** [set_check_invariants do_check] sets whether Async should check invariants of its
    internal data structures.  [set_check_invariants true] can substantially slow down
    your program. *)
val set_check_invariants : bool -> unit

(** [set_detect_invalid_access_from_thread do_check] sets whether Async routines should
    check if they are being accessed from some thread other than the thread currently
    holding the Async lock, which is not allowed and can lead to very confusing
    behavior. *)
val set_detect_invalid_access_from_thread : bool -> unit

type 'b folder = { folder : 'a. 'b -> t -> (t, 'a) Field.t -> 'b }

(** [fold_fields ~init folder] folds [folder] over each field in the scheduler.  The
    fields themselves are not exposed -- [folder] must be a polymorphic function that
    can work on any field.  So, it's only useful for generic operations, e.g., getting
    the size of each field. *)
val fold_fields : init:'b -> 'b folder -> 'b

val is_ready_to_initialize : unit -> bool
val is_initialized : unit -> bool

(** If a process that has already created, but not started, the Async scheduler would like
    to fork, and would like the child to have a clean Async, i.e., not inherit any of the
    Async work that was done in the parent, it can call [reset_in_forked_process] at the
    start of execution in the child process.  After that, the child can do Async stuff and
    then start the Async scheduler.

    [reset_in_forked_process ()] is a no-op if [is_initialized () = false] and has
    undefined behavior if [is_running () = true].
*)
val reset_in_forked_process : unit -> unit

(** [reset_in_forked_process_without_taking_lock] is similar to [reset_in_forked_process],
    with the difference that async lock is not taken by the calling thread. This means
    it's not safe to do Async stuff unless you obtain the lock first, usually by calling
    functions from the [Thread_safe] module. *)
val reset_in_forked_process_without_taking_lock : unit -> unit

(** [make_async_unusable ()] makes subsequent attempts to use the Async scheduler raise.
    One use case for [make_async_unusable] is if you fork from a process already running
    the Async scheduler, and want to run non-Async OCaml code in the child process, with
    the guarantee that the child process does not use Async. *)
val make_async_unusable : unit -> unit

(** [handle_thread_pool_stuck f] causes [f] to run whenever Async detects its thread pool
    is stuck (i.e., hasn't completed a job for over a second and has work waiting to
    start).  Async checks every second.  By default, if the thread pool has been stuck for
    less than 60s, Async will [eprintf] a message.  If more than 60s, Async will send an
    exception to the main monitor, which will abort the program unless there is a custom
    handler for the main monitor.

    Calling [handle_thread_pool_stuck] replaces whatever behavior was previously there. *)
val handle_thread_pool_stuck : (stuck_for:Time_ns.Span.t -> unit) -> unit

val default_handle_thread_pool_stuck : Thread_pool.t -> stuck_for:Time_ns.Span.t -> unit

(** [time_spent_waiting_for_io ()] returns the amount of time that the Async scheduler has
    spent in calls to [epoll_wait] (or [select]) since the start of the program. *)
val time_spent_waiting_for_io : unit -> Time_ns.Span.t

(** [set_min_inter_cycle_timeout] sets the minimum timeout that the scheduler will pass to
    the OS when it checks for I/O between cycles.  The minimum is zero by default.
    Setting it to a nonzero value is used to increase thread fairness between the
    scheduler and other threads.  A plausible setting is 10us.  This can also be set via
    the [ASYNC_CONFIG] environment variable. *)
val set_min_inter_cycle_timeout : Time_ns.Span.t -> unit

(** Returns true if any user-created fds are registered with the file descriptor
    watcher.

    The intended use case for this function (together with
    [thread_pool_has_unfinished_work]) is approximate deadlock detection, so that
    a test can crash when it runs out of things to do.
*)
val fds_may_produce_events : unit -> bool

(** Returns true if any of the threads in the thread pool are in use.
    Note that this value can change from [true] to [false] "suddenly" (from a separate
    thread) when a thread pool thread finishes.

    However, the work items submitted to the thread pool by [In_thread.run]
    enqueue their result as an [external_job] before finishing.

    So if you observe [thread_pool_has_unfinished_work () = false] and then confirm that
    there are no external jobs in the scheduler, then you know you didn't miss any thead
    pool work.

*)
val thread_pool_has_unfinished_work : unit -> bool

(** If any busy pollers exist, they will be called in a busy loop whenever the scheduler
    is waiting on I/O before an Async cycle, with the guarantee that they will be called
    at least once before every Async cycle.

    While the busy loop is running the program will only be responsive to the events
    detected by the pollers and to timing wheel alarms, but won't be responsive to
    anything else (signals, fd events, thread interruptions).

    We do not allow the busy loop to run longer than [max_inter_cycle_timeout].
    After adding the new busy poller, this function sets the [max_inter_cycle_timeout] to
    [Time_ns.Span.min max_inter_cycle_timeout max_busy_wait_duration].

    The pollers will run with the async lock held, so it's OK to access Async
    data structures, in particular to schedule jobs and alarms.

    Pollers run in the main monitor.
*)
val add_busy_poller
  :  t
  -> max_busy_wait_duration:Time_ns.Span.t
  -> Busy_poller.packed
  -> unit

val num_busy_pollers : t -> int

(** Instead of calling Scheduler.go, the [External] module can be used to drive Async with
    an external loop, manually running regular Async cycles. This is useful to integrate
    Async with another event loop system. *)
module External : sig
  (** Run a single Async cycle. This function must be called:
      - from the same thread every time
      - nonrecursively
      - while holding the Async lock
      - regularly, to ensure Async callbacks get run

      Whether this function may block is controlled by the [max_wait] parameter.
      Note that e.g. [run_one_cycle ~max_wait:(`Until t)] may return:

      - before [t], if something occurs that wakes up Async, such as activity on
        a file descriptor, a timeout occurring, or reaching [max_inter_cycle_timeout]

      - after [t], if there are Async jobs to run and some of them take a long time.
        That is, [max_wait] only controls the time spent blocking, not the time spent
        running jobs.

      Returns a collection of readiness events on file descriptors registered
      via [register_fd] below *)
  val run_one_cycle
    :  max_wait:[ `Zero | `Until of Time_ns.t | `Indefinite ]
    -> (File_descr.t * Read_write_pair.Key.t) list

  (** Calls [run_one_cycle] zero or more times until the given Deferred is determined *)
  val run_cycles_until_determined : 'a Deferred.t -> 'a

  (** Returns [true] if the current thread can use [run_one_cycle].
      Returns [false] if the Async scheduler is running in another thread.
      Must not be called from within Async.
      Must be called while holding the Async lock. *)
  val current_thread_can_cycle : unit -> bool

  (** Register a file descriptor for which [run_one_cycle] will return readiness events.
      Async will not do any I/O or close this descriptor. *)
  val register_fd : File_descr.t -> bool Read_write_pair.t -> unit Or_error.t

  (** Remove a registration added by [register_fd] *)
  val unregister_fd : File_descr.t -> unit Or_error.t

  (** Check whether a file descriptor is currently registered via [register_fd] *)
  val is_registered : File_descr.t -> bool
end

module For_metrics : sig
  module Thread_pool_stats_subscription : sig
    type t

    (** [create_exn] will create a subscription to stats for the async scheduler's thread
        pool. This function will raise if called more than once, as multiple separate
        callers to [Thread_pool.get_and_reset_stats] will interfere with each other. *)
    val create_exn : unit -> t

    (** See [Thread_pool.Stats] for documentation. Note that since calling this resets
        some stats, multiple different callers trying to use this function will interfere
        with each other. *)
    val get_and_reset : t -> Thread_pool.Stats.t
  end
end

module For_tests : sig
  val warm_up_fds : unit -> unit
end
