(** An interruptor provides a file descriptor that can be used to cause a
    file-descr-watcher to detect the file descriptor is ready for reading. We use an
    interruptor when a thread needs the Async scheduler to service a request.

    {v
    Knock, knock.
    Who's there?
    Interruptor cow.
    Interrup-
       _________________________
      /                         \
      |   __  __  ____   ____   |
      |  |  \/  |/ __ \ / __ \  |
      |  | \  / | |  | | |  | | |
      |  | |\/| | |  | | |  | | |
      |  | |  | | |__| | |__| | |
      |  |_|  |_|\____/ \____/  |
      \                         /
       -------------------------
              \   ^__^
               \  (oo)\_______
                  (__)\       )\/\
                      ||----w |
                      ||     ||
    v} *)

open! Core
open! Import

type t : value mod portable [@@deriving sexp_of]

include Invariant.S with type t := t

val create : create_fd:(Raw_fd.Kind.t -> Unix.File_descr.t -> Info.t -> Raw_fd.t) -> t
val read_fd : t -> Raw_fd.t

(** [thread_safe_interrupt t] causes an interrupt, which either makes the next [sleep]
    return [Clear_pending_interrupts], or makes [read_fd] ready to read, as appropriate,
    depending on what state the watcher is in. *)
val thread_safe_interrupt : t @ contended -> unit @@ portable

module Sleep : sig
  type t =
    | Clear_pending_interrupts
    | Sleep
end

(** [sleep t] tells the interruptor that the watcher is about to go to sleep. Returns
    [Sleep] when the watcher should go to sleep and [Clear_pending_interrupts] in case an
    interrupt has happened while the watcher was awake and the watcher should now [clear]
    the interruptor and make sure to service queued requests.

    If [Sleep] is returned, then the watcher is allowed to go to sleep while being
    sensitive to [read_fd]. *)
val sleep : t -> Sleep.t @@ portable

(** [clear t] should be called on wakeup after [sleep]. It clears the interruptor's
    knowledge of past interrupts, if any, and makes it responsive to future interrupts.
    Any calls to [thread_safe_interrupt] after [clear t] returns will be reflected in the
    next call to [sleep] (or the readiness of the [read_fd] if we do end up going to
    [Sleep]). *)
val clear : t -> unit

(** [clear_fd] must be called by the scheduler any time it wakes up due to the [read_fd]
    being ready to read. *)
val clear_fd : t -> unit

(** [already_interrupted t] is true if [thread_safe_interrupt t] has completed since the
    last call to [clear t]. *)
val already_interrupted : t -> bool @@ portable
