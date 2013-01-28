open Core.Std
open Import

include Module_type.Signal


(** [handle ?stop signals ~f] runs [f] on each signal in [signals] that is delivered,
    stopping when [stop] becomes determined.  Calling [handle] has the side-effect of
    installing a C signal handler for every signal in [signals], which will replace the
    existing C signal handler for that signal. *)
val handle : ?stop:unit Deferred.t -> t list -> f:(t -> unit) -> unit

(** [standard] is a list of signals that are reasonable to supply as arguments to deliver:

    abrt alrm chld cont hup int prof quit term tstp ttin ttou usr1 usr2 *)
val standard : t list
