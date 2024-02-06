(** A signal manager keeps track of a set of signals to be managed.  When a signal
    manager is managing a signal, it installs its own OCaml handler for that signal that
    records delivery of the signal.  It then later, upon request, will report the set
    of signals that it collected.

    Once a signal manager starts managing a signal, it never stops. *)

open! Core
module Signal := Core.Signal

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** [create] creates and returns a signal manager [t].  Whenever a signal that [t] is
    managing is delivered, it will call [thread_safe_notify_signal_delivered] from within
    the OCaml signal handler.  Therefore [thread_safe_notify_signal_delivered] must be
    thread safe. *)
val create : thread_safe_notify_signal_delivered:(unit -> unit) -> t

(** [manage t signal] causes [t] to manage [signal], thus overriding
    [default_sys_behavior] for that signal, and any other OCaml handler for that
    signal. *)
val manage : t -> Signal.t -> unit

(** [is_managing t signal] returns true iff [manage t signal] has been called *)
val is_managing : t -> Signal.t -> bool

(** [handle_delivered t] runs all signal handlers on the signals that have been
    delivered but not yet handled. *)
val iter_delivered
  :  t
  -> f:(original_disposition:Signal.Expert.behavior -> Signal.t -> unit)
  -> unit
