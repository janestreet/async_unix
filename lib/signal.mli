open Import

include module type of Core.Std.Signal with type t = Core.Std.Signal.t

(** We override values from [Core.Std.Signal] that we don't want people to use with
    Async. *)
val handle_default : [ `Do_not_use_with_async ] -> _
val ignore         : [ `Do_not_use_with_async ] -> _
val set            : [ `Do_not_use_with_async ] -> _ -> _
val signal         : [ `Do_not_use_with_async ] -> _ -> _

(** [handle ?stop signals ~f] arranges so that whenever a signal in [signals] is
    delivered, [f] is called on that signal.  If [f] raises, then an exception will be
    raised to the monitor in effect when [handle] was called.

    Multiple calls to [handle] with the same signal will cause all the handlers to run
    when that signal is delivered, not just the last handler from the last call to
    [handle].

    The first time [handle] is called for a signal, it will install a C signal handler for
    that signal, which will replace the existing C signal handler for that signal. *)
val handle : ?stop:unit Deferred.t -> t list -> f:(t -> unit) -> unit

(** [terminating] is a list of signals that can be supplied to [handle] and whose default
    behavior is to terminate the program: alrm hup int term usr1 usr2.

    Various signals whose [default_sys_behavior] is [`Terminate] are not included:

    | kill   | it's not allowed to be handled                            |
    | pipe   | Async already ignores this signal, since it handles EPIPE |
    | prof   | so that we can profile things with -p                     |
    | vtalrm | it already has a handler                                  |
*)
val terminating : t list

(** [is_managed_by_async signal] returns true iff [signal] is being managed by Async, and
    hence its default behavior is no longer in effect. *)
val is_managed_by_async : t -> bool
