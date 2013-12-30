(** [File_descr_watcher_intf.S] provides an API for for watching a set of file descriptors
    to see if they are ready for reading or writing.

    We have two implementations, one using epoll, and one using select.

    None of the functions need to be thread-safe, with the exception of
    [thread_safe_check].  So that implementations can easily do non-thread-safe actions,
    checking for ready I/O is always done in three steps:

    1. [pre_check], while holding the async lock
    2. [thread_safe_check], while not holding the async lock
    3. [post_check], while holding the async lock
 *)

open Core.Std
open Import

module Post = struct
  type t =
    { ready : File_descr.t list;
      bad   : File_descr.t list;
    }
  with sexp_of

  let empty = { ready = []; bad = [] }
end

module Timeout = struct
  type t = [ `Never | `Immediately | `After of Time.Span.t ] with sexp_of
end

module type S = sig

  (** A file-descr-watcher is essentially a map from [File_descr.t] to [bool
      Read_write.t], which defines the set of file descriptors being watched, and for each
      file descriptor, whether it is being watched for read, write, or both.  If a file
      descriptor is not being watched for either, it is not in the map. *)
  type t with sexp_of

  include Invariant.S with type t := t

  val backend : Config.File_descr_watcher.t

  (** [set] alters the map of file descriptors being watched.  It will take effect on the
      next call to [thread_safe_check].  Calling [set fd] with [{ read = false, write =
      false }] removes [fd] from the map. *)
  val set : t -> File_descr.t -> bool Read_write.t -> unit

  (** [iter t ~f] iterates over every file descriptor in the map, apply [f] to it once
      for each of \{`Read,`Write\} that it is being watched for. *)
  val iter : t -> f:(File_descr.t -> Read_write.Key.t -> unit) -> unit

  (** [pre_check t] does whatever non-thread-safe work is necessary to prepare for the
      system call that checks file descriptors being ready for read or write.  [pre_check]
      does not side effect [t]. *)
  module Pre : sig type t with sexp_of end
  val pre_check : t -> Pre.t

  (** [thread_safe_check t pre ~timeout] checks the file descriptors for their status and
      returns when at least one is available, or the [timeout] passes.
      [thread_safe_check] does not side effect [t].  Unlike the rest of the functions in
      this module, [thread_safe_check] is thread safe. *)
  module Check_result : sig type t with sexp_of end
  val thread_safe_check
    :  t
    -> Pre.t
    -> timeout:Timeout.t
    -> Check_result.t

  (** [post_check t check_result] returns the file descriptors available for read and
      write.  Any file descriptor appearing in [post] for read must have been watched
      for read, as per [set].  Similarly for write. *)
  val post_check
    :  t
    -> Check_result.t
    -> [ `Ok of Post.t Read_write.t
       | `Timeout
       | `Syscall_interrupted
       ]

  val reset_in_forked_process : t -> unit
end
