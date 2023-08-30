(** A signal manager keeps track of a set of signals to be managed and the signal handlers
    for them.  When a signal manager is managing a signal, it installs its own OCaml
    handler for that signal that records delivery of the signal.  It then later, upon
    request, will deliver the signal to all its handlers.

    Once a signal manager starts managing a signal, it never stops. *)

open! Core
module Signal := Core.Signal

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** Signal dispatcher controls what happens to the signals once they are seen by Async. *)
module type Signal_dispatcher = sig
  type t
  type handler

  (** [set_composable_handler] causes [t] to manage the handling of [signals], and
      switches to custom signal handling mode for these signals, which suppresses the
      default behavior, and instead runs the handlers added by [install_handler].

      This function is automatically called by [install_handler], so there's no need to
      call it directly. *)
  val set_composable_handler : t -> Signal.t list -> unit

  (** [install_handler t signals f] causes [t] to manage the handling of [signals], and
      registers [f] to run on every signal in [signals] that is delivered.   It is an
      error if [f] ever raises when it is called. *)
  val install_handler : t -> Signal.t list -> (Signal.t -> unit) -> handler

  (** [remove_handler handler] causes the particular [handler] to no longer handle the
      signals it was registered to handle.  The signal manager continues to manage those
      signals, i.e. the OCaml signal handler remains installed, whether or not they still
      have handlers. *)
  val remove_handler : t -> handler -> unit
end

(** [create] creates and returns a signal manager [t].  Whenever a signal that [t] is
    managing is delivered, it will call [thread_safe_notify_signal_delivered] from within
    the OCaml signal handler.  Therefore [thread_safe_notify_signal_delivered] must be
    thread safe. *)
val create : thread_safe_notify_signal_delivered:(unit -> unit) -> t

(** [manage t signal] causes [t] to manage [signal], thus overriding
    [default_sys_behavior] for that signal, and any other OCaml handler for that
    signal. *)
val manage : t -> Signal.t -> unit

(** Causes [t] to manage [signal], but still approximates the default behavior
    by calling [shutdown] on signals that normally cause the process to terminate. *)
val manage_but_keep_default_behavior : t -> Signal.t -> unit

(** [is_managing t signal] returns true iff [manage t signal] has been called *)
val is_managing : t -> Signal.t -> bool

include Signal_dispatcher with type t := t

(** [handle_delivered t] runs all signal handlers on the signals that have been
    delivered but not yet handled. *)
val handle_delivered : t -> unit
