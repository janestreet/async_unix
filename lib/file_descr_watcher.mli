(** [File_descr_watcher] provides an API for interacting with select-like functionality
    for watching a set of file descriptors to see if they are available for reading or
    writing.

    The functions in [File_descr_watcher] are not thread-safe, with the exception
    of [thread_safe_check]. *)
open Core.Std

type ('a, 'watching) t with sexp_of

val invariant : (_, _) t -> unit

(** [create ~num_file_descrs] creates a new file-descr-watcher that is able to watch file
    descriptors in [0, num_file_descrs). *)
val create : num_file_descrs:int -> (_, _) t

(** A file descriptor being monitored by a file_descr_watcher is in one of two states:
    [Watching] or [Stop_requested].  When one initially requests that the fd be monitored
    via [request_start_watching], it starts in the [Watching] state.  Later, when one
    calls [request_stop_watching], it transitions to the [Stop_requested] state.  Finally,
    when the client is notified via [post_check] of the status of the file descriptor,
    it is removed from the file_descr_watcher. *)
module State : sig
  type 'a t =
  | Watching of 'a
  | Stop_requested
  with sexp_of
end

(** [state t file_descr read_or_write] returns [Some state] with the state of [file_descr]
    if it is monitored by [t], or [None] if not. *)
val state
  :  (_, 'watching) t
  -> Unix.File_descr.t
  -> Read_write.Key.t
  -> 'watching State.t option

(** [iter t ~f] iterates over all the file descriptors being watched by [t], supplying
    them to [f] along with their state and whether they are read/write. *)
val iter
  :  ('a, 'watching) t
  -> f:(Read_write.Key.t -> 'watching State.t -> 'a -> unit)
  -> unit

(** [request_start_watching t file_descr a watching read_or_write] requests that [t] start
    watching [file_descr] to see if it is available for reading or writing, depending on
    the value of [read_or_write].  The value [a] is associated with [file_descr] and later
    returned by [post_check] when the file descriptor is ready to read/write, or is bad,
    or is no longer being watched.  The value [watching] is associated with [file_descr]
    and is made available via other API calls when [file_descr] is in the [Watching]
    state. *)
val request_start_watching
  :  ('a, 'watching) t
  -> Unix.File_descr.t
  -> 'a
  -> 'watching
  -> Read_write.Key.t
  -> [ `Ok | `Already_watching ]

(** [request_stop_watching t file_descr read_or_write] requests that [t] stop watching
    [file_descr] for reading or writing, according to [read_or_write].  The value
    associated with [file_descr] will be returned in [no_longer_watching] by the next call
    to [post_check]. *)
val request_stop_watching
  :  (_, 'watching) t
  -> Unix.File_descr.t
  -> Read_write.Key.t
  -> [ `Ok of 'watching | `Was_not_watching ]

(** [pre_check t] returns the set of file descriptors that are being watched by [t],
    in preparation for the system call that checks their status.  [pre_check] does not
    side effect [t]. *)
type 'a pre
val pre_check : ('a, _) t -> 'a pre

(** [thread_safe_check t pre ~timeout] checks the file descriptors in [pre] for their
    status and returns when at least one is available, or the [timeout] passes.
    [thread_safe_check] does not side effect [t].  Unlike the rest of the functions in
    this module, [thread_safe_check] is thread safe. *)
type 'a check_result
val thread_safe_check : ('a, _) t -> 'a pre -> timeout:float -> 'a check_result

(** [post_check t check_result] returns information about the status of file descriptors.
    For every value that occurs in [post], in any of its fields, [post_check] will cause
    [t] to no longer watch its associated file descriptor for the corresponding
    read/write.

    If [post_check] returns [`Retry], then select was interrupted and produced no
    information.  Clients should do [pre_check], [thread_safe_check], and [post_check]
    again. *)
type ('a, 'watching) post =
  { ready : ('a * 'watching) list;
    bad   : ('a * 'watching) list;
    no_longer_watching : 'a list;
  }

val post_check :
  ('a, 'watching) t
  -> 'a check_result
  -> [ `Retry
     | `Ok of ('a, 'watching) post Read_write.t
     ]
