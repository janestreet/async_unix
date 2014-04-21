(** A thread pool is a set of OCaml threads used to do work, where each piece of work is
    simply a thunk.  One creates a thread pool, and then uses [add_work] to submit work to
    it.  Work is done first-come-first-served by available threads in the pool.  Any of
    the available threads in the pool could be used to do work submitted to the pool
    (except helper threads, see below).

    A thread pool starts with no threads.  As work is added, the thread pool creates new
    threads to do the work, up to the maximum number of allowed threads,
    [max_num_threads], supplied to [create].  Thread-pool threads never die.  They just
    get created up until [max_num_threads] is reached and then live forever, doing work.
    Each thread in the pool is in a loop, waiting for a piece of work, running the thunk,
    and then repeating.  It may be that all the threads in the pool are not doing
    anything, but in this case, the threads still exist, and are simply blocked waiting
    for work.

    Sometimes one wants work to run in a dedicated thread, e.g. some C libraries require
    this.  To do this, use [Helper_thread], see below.

    All of the functions exposed by this module are thread safe; they synchronize using
    a mutex on the thread pool.

    One can control the priority of threads in the pool (in the sense of
    [Linux_ext.setpriority]).  Work added to the pool can optionally be given a priority,
    and the pool will set the priority of the thread that runs it for the duration of the
    work.  Helper threads can also be given a priority, which will be used for all work
    run by the helper thread, unless the work has an overriding priority.  The thread pool
    has a "default" priority that will be used for all work and helper threads that have
    no specified priority.  The default is simply the priority in effect when [create] is
    called.

    Behavior is unspecified if work calls [setpriority] directly. *)

open Core.Std
open Import

module Priority : module type of Linux_ext.Priority with type t = Linux_ext.Priority.t

type t with sexp_of

include Invariant.S with type t := t

(** [create ~max_num_threads] returns a new thread pool.  It is an error if
    [max_num_threads < 1]. *)
val create : max_num_threads:int -> t Or_error.t

(** [finished_with t] makes it an error to subsequently call [add_work* t] or
    [create_helper_thread t].  And, once all current work in [t] is finished, destroys all
    the threads in [t].  It is OK to call [finished_with] multiple times on the same [t];
    subsequent calls will have no effect. *)
val finished_with : t -> unit

(** [max_num_threads t] returns the maximum number of threads that [t] is allowed to
    create. *)
val max_num_threads : t -> int

(** [num_threads t] returns the number of threads that the pool [t] has created. *)
val num_threads : t -> int

(** [default_priority t] returns the priority that will be used for work performed by
    [t], unless that work is added with an overriding priority. *)
val default_priority : t -> Priority.t

(** [add_work ?priority ?name t f] enqueues [f] to be done by some thread in the pool.

    Exceptions raised by [f] are silently ignored.

    While the work is run, the name of the thread running the work will be set (via
    [Linux_ext.pr_set_name]) to [name] and the priority of the thread will be set
    to [priority].

    It is an error to call [add_work t] after [finished_with t]. *)
val add_work
  :  ?priority:Priority.t (** default is [default_priority t] *)
  -> ?name:string         (** default is ["thread-pool thread"] *)
  -> t
  -> (unit -> unit)
  -> unit Or_error.t

val num_work_completed : t -> int

(** [has_unstarted_work t] returns [true] if [t] has work that it hasn't been assigned
    to start running in a thread. *)
val has_unstarted_work : t -> bool

module Helper_thread : sig
  (** A helper thread is a thread with its own dedicated work queue.  Work added for the
      helper thread is guaranteed to be run by that thread.  The helper thread only runs
      work explicitly supplied to it. *)
  type t

  (** [default_name t] returns the name that will be used for work performed by [t],
      unless that work is added with an overriding name *)
  val default_name : t -> string

  (** [default_priority t] returns the priority that will be used for work performed by
      [t], unless that work is added with an overriding priority. *)
  val default_priority : t -> Priority.t
end

(** [create_helper_thread ?priority ?name t] creates a new helper thread.

    The thread pool does not internally refer to the [Helper_thread.t] it returns.  So, it
    is OK for client code to use a finalizer to detect it becoming unused.

    It is an error if no threads are available.  It is an error to call
    [create_helper_thread t] after [finished_with t].

    When the helper thread runs work, it will be at the helper thread's name and priority,
    except for work that is added with an overriding priority or name. *)
val create_helper_thread
  :  ?priority:Priority.t (** default is [default_priority t] *)
  -> ?name:string         (** default is ["helper thread"] *)
  -> t
  -> Helper_thread.t Or_error.t

(** [add_work_for_helper_thread ?priority ?name t helper_thread f] enqueues [f] on
    [helper_thread]'s work queue.

    Exceptions raised by [f] are silently ignored.

    It is an error to call [add_work_for_helper_thread t] after
    [finished_with_helper_thread t].

    When the helper thread runs [f], it will be at the helper thread's name and priority,
    unless overriden by [name] or [priority]. *)
val add_work_for_helper_thread
  :  ?priority:Priority.t (** default is [Helper_thread.default_priority helper_thread] *)
  -> ?name:string         (** default is [Helper_thread.name helper_thread] *)
  -> t
  -> Helper_thread.t
  -> (unit -> unit)
  -> unit Or_error.t

(** [finished_with_helper_thread t helper_thread] informs thread pool [t] that no future
    work will be added for [helper_thread], and makes it an error to in the future add
    work for [helper_thread].  Furthermore, once [helper_thread] finishes with its last
    piece of work, it will revert to a general thread-pool thread.  It is OK to call
    [finished_with_helper_thread] multiple times on the same [helper_thread]; subsequent
    calls will have no effect. *)
val finished_with_helper_thread : t -> Helper_thread.t -> unit
