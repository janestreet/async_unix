open Import

include module type of Core.Std.Signal with type t = Core.Std.Signal.t

(** We override values from [Core.Std.Signal] that we don't want people to use with
    Async. *)
val handle_default : [ `Do_not_use_with_async ] -> _
val ignore         : [ `Do_not_use_with_async ] -> _
val set            : [ `Do_not_use_with_async ] -> _ -> _
val signal         : [ `Do_not_use_with_async ] -> _ -> _

(** [handle ?stop signals ~f] arranges so that whenever a signal in [signals] is
    delivered, [f] is called on that signal.  If [f] raises, than an exception will be
    raised to the monitor in effect when [handle] was called.

    Calling [handle] has the side-effect of installing a C signal handler for every signal
    in [signals], which will replace the existing C signal handler for that signal. *)
val handle : ?stop:unit Deferred.t -> t list -> f:(t -> unit) -> unit

(** [standard] is a list of signals that are reasonable to supply as arguments to deliver:

    abrt alrm chld cont hup int prof quit term tstp ttin ttou usr1 usr2 *)
val standard : t list
