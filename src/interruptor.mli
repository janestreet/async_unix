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

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : create_fd:(Raw_fd.Kind.t -> Unix.File_descr.t -> Info.t -> Raw_fd.t) -> t
val read_fd : t -> Raw_fd.t

(** [thread_safe_interrupt t] causes [read_fd t] to become ready for reading. *)
val thread_safe_interrupt : t -> unit

(** [clear t] causes [read_fd t] to become not ready for reading. It is guaranteed that
    any calls to [thread_safe_interrupt] after [clear t] returns (and prior to another
    call to [clear t]) will cause [read_fd] to become ready for reading. *)
val clear : t -> unit

(** [already_interrupted t] is true if [thread_safe_interrupt t] has completed since the
    last call to [clear t]. *)
val already_interrupted : t -> bool
