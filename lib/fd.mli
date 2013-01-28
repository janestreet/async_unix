(* An [Fd.t] is a wrapper around a Unix file descriptor, with additional information about
   the kind of file descriptor and logic to ensure that we don't use a file descriptor
   that has been closed, or close a file descriptor that is in use.  Since Async uses
   multiple threads to make read/write and other system calls on file descriptors, and
   Unix reuses descriptors after they are closed, Async has to be very careful that the
   file descriptor passed to a system call is referring to the file it intends, and not
   some other completely unrelated file that Unix has decided to assign to the same
   descriptor.

   Provided that one only accesses a file descriptor within the context of the functions
   below, [Fd] guarantees that the file descriptor will not have been closed/reused and
   will correspond to the same file that it did when the [Fd.t] was created:

   with_file_descr
   with_file_descr_deferred
   syscall
   syscall_exn
   syscall_in_thread
   syscall_in_thread_exn

   The [Fd] module keeps track of which of these functions that are currently accessing
  the file descriptor, and ensures that any close happens after they complete.  Also,
  once close has been called, it refuses to provide further access to the file descriptor,
  either by returning a variant, `Already_closed, or raising an exception.

  Some of the above functions take an optional [?nonblocking:bool] argument.  The default
  is false, but if it is set to true, then before supplying the underlying [file_descr],
  the [Fd] module will first call [Unix.set_nonblock file_descr], if it hasn't previously
  done so on that file descriptor.  This is intended to support making nonblocking system
  calls (e.g. connect, read, write) directly within async, without releasing the OCaml
  lock or the async lock, and without using another thread. *)
open Core.Std
open Import

module Kind : sig
type t =
| Char (* a terminal *)
| Fifo (* a pipe *)
| File (* a regular file *)
| Socket of [ `Unconnected (* the result of socket() *)
            | `Bound       (* the result of bind() *)
            | `Passive     (* the result of listen() *)
            | `Active      (* the result of connect() or accept() *)
            ]
end

type t with sexp_of

(** [to_string t] returns a pretty sexp of the representation of [t] *)
val to_string : t -> string

(** [create kind file_descr] creates a new [t] of the underlying kind and file
    descriptor.

    We thought about using fstat() rather than requiring the user to supply the kind.  But
    fstat can block, which would require putting this in a thread, which has some
    consequences, and it isn't clear that it gets us that much.  Also, [create] is mostly
    used within the Async implementation -- clients shouldn't need it unless they are
    mixing Async and non-Async code. *)
val create : Kind.t -> Unix.File_descr.t -> name:string -> t

(** [kind t] returns the kind of file descriptor that [t] is. *)
val kind : t -> Kind.t

(** [supports_nonblock t] returns true if [t] supports nonblocking system calls. *)
val supports_nonblock : t -> bool

(** [close t] prevents further use of [t], and closes the underlying file descriptor once
    all the current uses are finished.  The result of [close] becomes determined once
    the underlying file descriptor has been closed. *)
val close : t -> unit Deferred.t

(** [with_close t f] applies [f] to [t], returns the result of [f], and closes [t]. *)
val with_close : t -> f:(t -> 'a Deferred.t) -> 'a Deferred.t

(** [is_closed t] returns [true] if [close t] has been called. *)
val is_closed : t -> bool

(** [close_finished t] becomes determined after the close() system call on [t]'s
    underlying file descriptor has finished. *)
val close_finished : t -> unit Deferred.t

(** [is_open t] is [not (is_closed t]) *)
val is_open : t -> bool

(** [stdin], [stdout], and [stderr] are wrappers around the standard Unix file
    descriptors. *)
val stdin : unit -> t
val stdout : unit -> t
val stderr : unit -> t

(** [with_file_descr t f] runs [f] on the file descriptor underlying [t], if [is_open t],
    and returns [`Ok] or [`Error] according to [f].  If [is_closed t], then it does not
    call [f] and returns [`Already_closed]. *)
val with_file_descr :
  ?nonblocking:bool
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> [ `Ok of 'a
     | `Already_closed
     | `Error of exn
     ]

(** [with_file_descr_exn] is like [with_file_descr] except that it raises rather than
    return [`Already_closed] or [`Error]. *)
val with_file_descr_exn :
  ?nonblocking:bool
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> 'a

(** [with_file_descr_deferred t f] runs [f] on the file descriptor underlying [t], if
    [is_open t], and returns [`Ok] or [`Error] according to [f].  If [is_closed t], then
    it does not call [f] and returns [`Already_closed].  It ensures that the file
    descriptor underlying [t] is not closed until the result of [f] becomes determined (or
    [f] raises). *)
val with_file_descr_deferred :
  t
  -> (Unix.File_descr.t -> 'a Deferred.t)
  -> [ `Ok of 'a
     | `Already_closed
     | `Error of exn
     ] Deferred.t

(** [ready_to_interruptible t read_write ~interrupt] returns a deferred that will become
    determined when [select] indicates that the file descriptor underlying [t] can be read
    from or written to without blocking, or when [interrupt] becomes determined. *)
val ready_to_interruptible :
  t
  -> [ `Read | `Write ]
  -> interrupt:unit Deferred.t
  -> [ `Bad_fd
     | `Closed
     | `Interrupted
     | `Ready
     ] Deferred.t

(** [ready_to t read_write] is like [ready_to_interruptible], but without the possibility
    of interruption. *)
val ready_to :
  t
  -> [ `Read | `Write ]
  -> [ `Bad_fd
     | `Closed
     | `Ready
     ]
  Deferred.t

(** [syscall t f] runs [Async_unix.syscall] with [f] on the file descriptor underlying
    [t], if [is_open t], and returns [`Ok] or [`Error] according to [f].  If
    [is_closed t], it does not call [f] and returns [`Already_closed]. *)
val syscall :
  ?nonblocking:bool
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> [ `Already_closed
     | `Ok of 'a
     | `Error of exn
     ]

(** [syscall_exn t f] is like [syscall], except it raises rather than return
    [`Already_closed] or [`Error]. *)
val syscall_exn :
  ?nonblocking:bool
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> 'a

(** [syscall_in_thread t f] runs [In_thread.syscall] with [f] on the file descriptor
    underlying [t], if [is_open t], and returns a deferred that becomes determined with
    [`Ok] or [`Error] when the system call completes.  If [is_closed t], it does not call
    [f] and returns [`Already_closed]. *)
val syscall_in_thread :
  t
  -> (Unix.File_descr.t -> 'a)
  -> [ `Already_closed
     | `Ok of 'a
     | `Error of exn
     ] Deferred.t

(** [syscall_in_thread_exn] is like [syscall_in_thread], except it raises rather than
    return [`Already_closed] or [`Error]. *)
val syscall_in_thread_exn : t -> (Unix.File_descr.t -> 'a) -> 'a Deferred.t

(** [of_in_channel] and [of_out_channel] create an fd from their underlying file
    descriptor. *)
val of_in_channel : In_channel.t -> Kind.t -> t
val of_out_channel : Out_channel.t -> Kind.t -> t

(** [of_in_channel_auto ic] is just like of_in_channel, but uses [fstat] to determine the
    kind.  It makes some assumptions about sockets, specifically it assumes that a socket
    is either listening, or connected to something (and it uses getsockopt to find out
    which).  Don't pass an in_channel containing an unconnected non-listening socket. *)
val of_in_channel_auto : In_channel.t -> t Deferred.t

(** [of_out_channel_auto ic] is just like of_out_channel, but uses [fstat] to determine
    the kind.  It makes some assumptions about sockets, specifically it assumes that a
    socket is either listening, or connected to something (and it uses getsockopt to find
    out which).  Don't pass an in_channel containing an unconnected non listening
    socket. *)
val of_out_channel_auto : Out_channel.t -> t Deferred.t

(** [file_descr_exn t] returns the file descriptor underlying [t], unless [is_closed t],
    in which case it raises.  One must be very careful when using this function, and
    should try not to, since any uses of the resulting file descriptor are unknown to
    the [Fd] module, and hence can violate the guarantee it is trying to enforce. *)
val file_descr_exn : t -> Unix.File_descr.t

(** [to_int_exn t] returns the the underlying file descriptor as an int.  It has the same
    caveats as [file_descr_exn]. *)
val to_int_exn : t -> int

(** [replace t kind] is for internal use only, by Unix_syscalls.  It is used when one wants
    to reuse a file descriptor in an fd with a new kind.  It marks the input [t] as
    [Replaced] so that one can't accidentally close the result by closing [t]. *)
val replace : t -> Kind.t -> t
