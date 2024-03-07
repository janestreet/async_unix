(** Signal handling. *)

open! Import

(** To discourage use of the [Signal.Expert] module, we hide it here.  People can use
    [Core.Signal.Expert] if they need. *)
include
  module type of Core.Signal
    with type t = Core.Signal.t
    with module Expert := Core.Signal.Expert

(** We override values from [Core.Signal] that we don't want people to use with
    Async. *)
val handle_default : [ `Do_not_use_with_async ] -> _

val ignore : [ `Do_not_use_with_async ] -> _

(** [handle ?stop signals ~f] arranges so that whenever a signal in [signals] is
    delivered, [f] is called on that signal.  If [f] raises, then an exception will be
    raised to the monitor in effect when [handle] was called.

    Multiple calls to [handle] with the same signal will cause all the handlers to run
    when that signal is delivered, not just the last handler from the last call to
    [handle].

    The first time [handle] is called for a signal, it will install a C signal handler for
    it, replacing the existing C signal handler for that signal.

    If [stop] is passed, the Async signal handler will be uninstalled when [stop]
    resolves. (the OCaml and C signal handlers are never uninstalled, so if this is
    the last Async signal handler then the signal will start being ignored)
*)
val handle : ?stop:unit Deferred.t -> t list -> f:(t -> unit) -> unit

(** [manage_by_async signal] arranges so that [signal] starts being managed by
    Async, i.e. we install an OCaml signal handler for it.
    The behavior of that handler approximates the default signal behavior with the
    difference that for signals whose default behavior is to terminate the program, we
    run on-shutdown handlers first.

    For terminating signals the exit status of the program will be indistinguishable from
    the signal not being handled. (see [Shutdown.shutdown_with_signal_exn])

    If [handle] is called (before or after), that takes precedence: the shutdown on signal
    behavior is suppressed (even if the corresponding [stop] ivar is determined).
*)
val manage_by_async : t list -> unit

(** [terminating] is a list of signals that can be supplied to [handle] and whose default
    behavior is to terminate the program: [alrm hup int term usr1 usr2].

    Various signals whose [default_sys_behavior] is [`Terminate] are not included:


    {v
       | kill   | it's not allowed to be handled                            |
       | pipe   | Async already ignores this signal, since it handles EPIPE |
       | prof   | so that we can profile things with -p                     |
       | vtalrm | it already has a handler                                  | v} *)
val terminating : t list

(** [is_managed_by_async signal] returns true iff [signal] is being managed by Async, and
    hence its default behavior is no longer in effect. *)
val is_managed_by_async : t -> bool
