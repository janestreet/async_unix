(** Writer provides an interface to writing strings that batches together writes into a
    single underlying write system call. *)
open Core.Std
open Import

module Id : Unique_id

type t with sexp_of

(** [io_stats] Overall IO statistics for all writers *)
val io_stats : Io_stats.t

(** [stdout] and [stderr] are writers for file descriptors 1 and 2.  They are lazy because
    we don't want to create them in all programs that happen to link with async. *)
val stdout : t Lazy.t
val stderr : t Lazy.t


(** [create ?buf_len ?syscall ?buffer_age_limit fd] creates a new writer.  The file
    descriptor fd should not be in use for writing by anything else.

    By default, a write system call occurs at the end of a cycle in which bytes were
    written.  One can supply ~syscall:(`Periodic span) to get better performance.  This
    batches writes together, doing the write system call periodically according to the
    supplied span.

    A writer can asynchronously fail if the underlying write syscall returns an error,
    e.g. EBADF, EPIPE, ECONNRESET, ....

    [buffer_age_limit] specifies how backed up you can get before raising an exception.
    Default is 2 minutes.  You can supply [`Unlimited] to turn off buffer-age checks.

    [raise_when_consumer_leaves] specifies whether the writer should raise an exception
    when the consumer receiving bytes from the writer leaves, i.e. in Unix, the write
    syscall returns EPIPE or ECONNRESET.  If [not raise_when_consumer_leaves], then the
    writer will silently drop all writes after the consumer leaves, and the writer will
    eventually fail with a writer-buffer-older-than error if the application remains open
    long enough. *)
type buffer_age_limit = [ `At_most of Time.Span.t | `Unlimited ] with bin_io, sexp
val create
  :  ?buf_len:int
  -> ?syscall:[ `Per_cycle | `Periodic of Time.Span.t ]
  -> ?buffer_age_limit:buffer_age_limit
  -> ?raise_when_consumer_leaves:bool (* defaults to true *)
  -> Fd.t
  -> t

(** [set_raise_when_consumer_leaves t bool] sets the [raise_when_consumer_leaves] flag of
    [t], which determies how [t] responds to a write system call raising EPIPE and
    ECONNRESET (see [create]). *)
val set_raise_when_consumer_leaves : t -> bool -> unit

(** [consumer_left t] returns a deferred that becomes determined when [t] attempts to
    write to a pipe that broke because the consumer on the other side left. *)
val consumer_left : t -> unit Deferred.t

val of_out_channel : out_channel -> Fd.Kind.t -> t

(** [open_file ?append file] opens [file] for writing and returns a writer for it.  It
    uses [Unix_syscalls.open_write] to open the file.  *)
val open_file : ?append:bool (* defaults to false *) -> string -> t Deferred.t

(** [with_file ~file f] opens [file] for writing, creates a writer [t], and runs [f t] to
    obtain a deferred [d].  When [d] becomes determined, the writer is closed.  When the
    close completes, the result of [with_file] becomes determined with the value of [d].

    There is no need to call [Writer.flushed] to ensure that [with_file] waits for the
    writer to be flushed before closing it.  [Writer.close] will already wait for the
    flush. *)
val with_file
  :  ?append:bool (* defaults to false *)
  -> ?exclusive:bool (* defaults to false *)
  -> string
  -> f:(t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [id t] @return an id for this writer that is unique among all other writers *)
val id : t -> Id.t

(** [fd t] @return the Fd.t used to create this writer *)
val fd : t -> Fd.t

(** [set_fd t fd] sets the fd used by [t] for its underlying system calls.  It first waits
    until everything being sent to the current fd is flushed.  Of course, one must
    understand how the writer works and what one is doing to use this. *)
val set_fd : t -> Fd.t -> unit Deferred.t

(** [write ?pos ?len t s] adds a job to the writer's queue of pending writes.  The
    contents of the string are copied to an internal buffer before write returns, so
    clients can do whatever they want with [s] after that. *)
val write           : ?pos:int -> ?len:int -> t -> string      -> unit
val write_bigstring : ?pos:int -> ?len:int -> t -> Bigstring.t -> unit

val write_substring    : t -> Substring   .t -> unit
val write_bigsubstring : t -> Bigsubstring.t -> unit

val writef : t -> ('a, unit, string, unit) format4 -> 'a

(** [to_formatter t] @return an OCaml-formatter that one can print to using
    {!Format.fprintf}.  Note that flushing the formatter will only submit all buffered
    data to the writer, but does _not_ guarantee flushing to the operating system. *)
val to_formatter : t -> Format.formatter

(** [write_char t c] writes the character *)
val write_char : t -> char -> unit

val newline : t -> unit

(** [write_byte t i] writes one 8-bit integer (as the single character with that code).
    The given integer is taken modulo 256. *)
val write_byte : t -> int -> unit

val write_sexp : ?hum:bool (* defaults to false *) -> t -> Sexp.t -> unit

(** [write_bin_prot] writes out a value using its bin_prot sizer/writer pair.  The format
    is the "size-prefixed binary protocol", in which the length of the data is written
    before the data itself.  This is the format that Reader.read_bin_prot reads. *)
val write_bin_prot : t -> 'a Bin_prot.Type_class.writer -> 'a -> unit

(** Serialize data using marshal and write it to the writer *)
val write_marshal : t -> flags : Marshal.extern_flags list -> _ -> unit

(** Unlike the [write_] functions, all functions starting with [schedule_] require
    flushing or closing of the writer after returning before it is safe to modify the
    bigstrings which were directly or indirectly passed to these functions.  The reason is
    that these bigstrings will be read from directly when writing; their contents is not
    copied to internal buffers.

    This is important if users need to send the same large data string to a huge number of
    clients simultaneously (e.g. on a cluster), because these functions then avoid
    needlessly exhausting memory by sharing the data. *)

(** [schedule_bigstring t bstr] schedules a write of bigstring [bstr].
    It is not safe to change the bigstring until the writer has been
    successfully flushed or closed after this operation. *)
val schedule_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit

(** [schedule_iovec t iovec] schedules a write of I/O-vector [iovec].  It is not safe to
    change the bigstrings underlying the I/O-vector until the writer has been successfully
    flushed or closed after this operation. *)
val schedule_iovec : t -> Bigstring.t Unix.IOVec.t -> unit

(** [schedule_iovecs t iovecs] like {!schedule_iovec}, but takes a whole queue [iovecs] of
    I/O-vectors as argument.  The queue is guaranteed to be empty when this function
    returns and can be modified.  It is not safe to change the bigstrings underlying the
    I/O-vectors until the writer has been successfully flushed or closed after this
    operation. *)
val schedule_iovecs : t -> Bigstring.t Unix.IOVec.t Queue.t -> unit

(** [flushed t] returns a deferred that will become determined when all prior writes
    complete (i.e. the write() system call returns).  If a prior write fails, then the
    deferred will never become determined.

    It is OK to call [flushed t] after [t] has been closed. *)
val flushed      : t ->   unit Deferred.t
val flushed_time : t -> Time.t Deferred.t

val fsync : t -> unit Deferred.t
val fdatasync : t -> unit Deferred.t

(** [send t s] writes a string to the channel that can be read back
    using Reader.recv *)
val send : t -> string -> unit

(** [monitor t] returns the writer's monitor. *)
val monitor : t -> Monitor.t

(** [close ?force_close t] waits for the writer to be flushed, and then calls [Unix.close]
    on the underlying file descriptor.  [force_close] causes the [Unix.close] to happen
    even if the flush hangs.  By default [force_close] is [Deferred.never ()] for files
    and [after (sec 5)] for other types of file descriptors (e.g. sockets).  If the close
    is forced, data in the writer's buffer may not be written to the file descriptor.  You
    can check this by calling [bytes_to_write] after [close] finishes.

    [close] will raise an exception if the [Unix.close] on the underlying file descriptor
    fails.

    It is required to call [close] on a writer in order to close the underlying file
    descriptor.  Not doing so will cause a file descriptor leak.  It also will cause a
    space leak, because until the writer is closed, it is held on to in order to flush the
    writer on shutdown.

    It is an error to call other operations on [t] after [close t] has been called, except
    that calls of [close] subsequent to the original call to [close] will return the same
    deferred as the original call.

    [close_finished t] becomes determined after [t]'s underlying file descriptor has been
    closed, i.e. it is the same as the result of [close].  [close_finished] differs from
    [close] in that it does not have the side effect of initiating a close.

    [is_closed t] returns [true] iff [close t] has been called.

    [is_open t] is [not (is_closed t)] *)
val close : ?force_close:unit Deferred.t -> t -> unit Deferred.t
val close_finished : t -> unit Deferred.t
val is_closed : t -> bool
val is_open   : t -> bool

(** [bytes_to_write t] returns how many bytes have been requested to write but have not
    yet been written. *)
val bytes_to_write : t -> int

(** [bytes_written t] returns how many bytes have been written. *)
val bytes_written : t -> Int63.t

(** [with_file_atomic ?temp_prefix ?perm ?fsync file ~f] creates a writer to a temp file,
    feeds that writer to [f], and when [f] finishes, atomically moves (i.e. uses
    [Unix.rename]) the temp file to [file].  If [file] currently exists, it will be
    replaced, even if it is read only.  The temp file will be prefixed by [temp_prefix]
    (prefix may refer to a different directory!) if given, and suffixed by a unique random
    sequence of six characters.  The temp file may need to be removed in case of a crash
    so it may be prudent to choose a [temp_prefix] that can be easily found by cleanup
    tools.

    If [fsync] is [true], the temp file will be flushed to disk before it takes the place
    of the target file, thus guaranteeing that the target file will always be in a sound
    state, even after a machine crash.  Since synchronization is extremely slow, this is
    not the default.  Think carefully about the event of machine crashes and whether you
    may need this option!

    We intend for [with_file_atomic] to preserve the behavior of the [open] system call,
    so if [file] does not exist, we will apply the umask to [perm].  If it does exist,
    [perm] will default to the file's current permissions rather than 0o666.

    @param temp_prefix default = no prefix
    @param perm default = [0o666]

    [save] is a special case of [with_file_atomic] that atomically writes the given
    string to the specified file.

    [save_sexp] is a special case of [with_file_atomic] that atomically writes the
    given sexp to the specified file. *)

val with_file_atomic
  :  ?temp_prefix:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (* defaults to false *)
  -> string
  -> f:(t -> 'a Deferred.t)
  -> 'a Deferred.t

val save
  :  ?temp_prefix:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (* defaults to false *)
  -> string
  -> contents:string
  -> unit Deferred.t

val save_sexp
  :  ?temp_prefix:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (* defaults to false *)
  -> ?hum:bool (* defaults to true *)
  -> string
  -> Sexp.t
  -> unit Deferred.t

(** [transfer t pipe_r f] repeatedly pulls values from [pipe_r], and feeds them to [f],
    which should in turn write them to [t].  It provides pushback to [pipe_r] by not
    reading when [t] cannot keep up with the data being pushed in.  The result becomes
    determined when [pipe_r] reaches its EOF.

    [transfer] causes [Pipe.flushed] on [pipe_r]'s writer to ensure that the bytes have
    been flushed to [t] before returning.  It also waits on [Pipe.upstream_flushed] at
    shutdown. *)
val transfer : t -> 'a Pipe.Reader.t -> ('a -> unit) -> unit Deferred.t

(** [pipe t] returns the writing end of a pipe attached to [t] that pushes back when [t]
    cannot keep up with the data being pushed in.  Closing the pipe will close [t]. *)
val pipe : t -> string Pipe.Writer.t

(** [invariant t] raises an exception if the writer is in a bad state *)
val invariant : t -> unit
