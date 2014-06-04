(** [Writer] is Async's main API for output to a file descriptor.  It is the analog of
    [Core.Std.Out_channel].

    Each writer has an internal buffer, to which [Writer.write*] adds data.  Each writer
    uses an Async microthread that makes [write()] system calls to move the data from the
    writer's buffer to an OS buffer via the file descriptor.  There is no guarantee that
    the data sync on the other side of the writer can keep up with the rate at which you
    are writing.  If it cannot, the OS buffer will fill up and the writer's micro-thread
    will be unable to send any bytes.  In that case, calls to [Writer.write*] will grow
    the writer's buffer without bound, as long as your program produces data.  One
    solution to this problem is to call [Writer.flushed] and not continue until that
    becomes determined, which will only happen once the bytes in the writer's buffer have
    been successfully transferred to the OS buffer.  Another solution is to check
    [Writer.bytes_to_write] and not produce any more data if that is beyond some bound.

    There are two kinds of errors that one can handle with writers.  First, a writer can
    be [close]d, which will cause future [write]s (and other operations) to synchronously
    raise an excecption.  Second, the writer's microthread can fail due to a [write()]
    system call failing.  This will cause an exception to be sent to the writer's monitor,
    which will be a child of the monitor in effect when the writer is created.  One can
    deal with such asynchronous exceptions in the usual way, by handling the stream
    returned by [Monitor.detach_and_get_error_stream (Writer.monitor writer)].
*)
open Core.Std
open Import

module Id : Unique_id

type t with sexp_of

include Invariant.S with type t := t

(** [io_stats] Overall IO statistics for all writers *)
val io_stats : Io_stats.t

(** [stdout] and [stderr] are writers for file descriptors 1 and 2.  They are lazy because
    we don't want to create them in all programs that happen to link with Async.

    When either [stdout] or [stderr] is created, they both are created.  Furthermore, if
    they point to the same inode, then they will be the same writer to [Fd.stdout].  This
    can be confusing, because [fd (force stderr)] will be [Fd.stdout], not [Fd.stderr].
    And subsequent modifications of [Fd.stderr] will have no effect on [Writer.stderr].

    Unfortunately, the sharing is necessary because Async uses OS threads to do write()
    syscalls using the writer buffer.  When calling a program that redirects stdout and
    stderr to the same file, as in:

    {v
      foo.exe >/tmp/z.file 2>&1
    v}

    if [Writer.stdout] and [Writer.stderr] weren't the same writer, then they could have
    threads simultaneously writing to the same file, which could easily cause data loss. *)
val stdout : t Lazy.t
val stderr : t Lazy.t

type buffer_age_limit = [ `At_most of Time.Span.t | `Unlimited ] with bin_io, sexp


(** [create ?buf_len ?syscall ?buffer_age_limit fd] creates a new writer.  The file
    descriptor fd should not be in use for writing by anything else.

    By default, a write system call occurs at the end of a cycle in which bytes were
    written.  One can supply ~syscall:(`Periodic span) to get better performance.  This
    batches writes together, doing the write system call periodically according to the
    supplied span.

    A writer can asynchronously fail if the underlying write syscall returns an error,
    e.g. EBADF, EPIPE, ECONNRESET, ....

    [buffer_age_limit] specifies how backed up you can get before raising an exception.
    The default is [`Unlimited] for files, and 2 minutes for other kinds of file
    descriptors.  You can supply [`Unlimited] to turn off buffer-age checks.

    [raise_when_consumer_leaves] specifies whether the writer should raise an exception
    when the consumer receiving bytes from the writer leaves, i.e. in Unix, the write
    syscall returns EPIPE or ECONNRESET.  If [not raise_when_consumer_leaves], then the
    writer will silently drop all writes after the consumer leaves, and the writer will
    eventually fail with a writer-buffer-older-than error if the application remains open
    long enough. *)
val create
  :  ?buf_len:int
  -> ?syscall:[ `Per_cycle | `Periodic of Time.Span.t ]
  -> ?buffer_age_limit:buffer_age_limit
  -> ?raise_when_consumer_leaves:bool (** default is [true] *)
  -> Fd.t
  -> t

val raise_when_consumer_leaves : t -> bool

(** [set_raise_when_consumer_leaves t bool] sets the [raise_when_consumer_leaves] flag of
    [t], which determies how [t] responds to a write system call raising EPIPE and
    ECONNRESET (see [create]). *)
val set_raise_when_consumer_leaves : t -> bool -> unit

(** [set_buffer_age_limit t buffer_age_limit] replaces the existing buffer age limit with
    the new one.  This is useful for stdout and stderr, which are lazily created in a
    context that does not allow applications to specify [buffer_age_limit]. *)
val set_buffer_age_limit : t -> buffer_age_limit -> unit

(** [consumer_left t] returns a deferred that becomes determined when [t] attempts to
    write to a pipe that broke because the consumer on the other side left. *)
val consumer_left : t -> unit Deferred.t

val of_out_channel : out_channel -> Fd.Kind.t -> t

(** [open_file file] opens [file] for writing and returns a writer for it.  It uses
    [Unix_syscalls.openfile] to open the file.  *)
val open_file
  :  ?append:bool        (** default is [false] *)
  -> ?close_on_exec:bool (** default is [true] *)
  -> ?perm:int           (** default is [0o666] *)
  -> string
  -> t Deferred.t

(** [with_file ~file f] opens [file] for writing, creates a writer [t], and runs [f t] to
    obtain a deferred [d].  When [d] becomes determined, the writer is closed.  When the
    close completes, the result of [with_file] becomes determined with the value of [d].

    There is no need to call [Writer.flushed] to ensure that [with_file] waits for the
    writer to be flushed before closing it.  [Writer.close] will already wait for the
    flush. *)
val with_file
  :  ?perm:int       (** default is [0o666] *)
  -> ?append:bool    (** default is [false] *)
  -> ?exclusive:bool (** default is [false] *)
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

(** [write_gen t a] writes [a] to writer [t], with [length] specifying the number of bytes
    needed and [blit_to_bigstring] blitting [a] directly into the [t]'s buffer.  If one
    has a type that has [length] and [blit_to_bigstring] functions, like:

    {[
      module A : sig
        type t
        val length : t -> int
        val blit_to_bigstring : (t, Bigstring.t) Blit.blit
      end
    ]}

    then one can use [write_gen] to implement a custom analog of [Writer.write], like:

    {[
      module Write_a : sig
        val write : ?pos:int -> ?len:int -> A.t -> Writer.t -> unit
      end = struct
        let write ?pos ?len a writer =
          Writer.write_gen
            ~length:A.length
            ~blit_to_bigstring:A.blit_to_bigstring
            ?pos ?len writer a
      end
    ]}

    If it is difficult to write only part of a value, one can choose to not support [?pos]
    and [?len]:

    {[
      module Write_a : sig
        val write : A.t -> Writer.t -> unit
      end = struct
        let write a writer =
          Writer.write_gen
            ~length:A.length
            ~blit_to_bigstring:A.blit_to_bigstring
            writer a
      end
    ]}
*)
val write_gen
  :  length:('a -> int)
  -> blit_to_bigstring:('a, Bigstring.t) Blit.blit
  -> ?pos:int
  -> ?len:int
  -> t
  -> 'a
  -> unit

(** [write ?pos ?len t s] adds a job to the writer's queue of pending writes.  The
    contents of the string are copied to an internal buffer before [write] returns, so
    clients can do whatever they want with [s] after that. *)
val write           : ?pos:int -> ?len:int -> t -> string         -> unit
val write_bigstring : ?pos:int -> ?len:int -> t -> Bigstring.t    -> unit
val write_iobuf     : ?pos:int -> ?len:int -> t -> (_, _) Iobuf.t -> unit

val write_substring    : t -> Substring   .t -> unit
val write_bigsubstring : t -> Bigsubstring.t -> unit

val writef : t -> ('a, unit, string, unit) format4 -> 'a

(** [to_formatter t] @return an OCaml-formatter that one can print to using
    {!Format.fprintf}.  Note that flushing the formatter will only submit all buffered
    data to the writer, but does _not_ guarantee flushing to the operating system. *)
val to_formatter : t -> Format.formatter

(** [write_char t c] writes the character *)
val write_char : t -> char -> unit

(** [newline t] is [write_char t '\n'] *)
val newline : t -> unit

(** [write_line t s] is [write t s; newline t]. *)
val write_line : t -> string -> unit

(** [write_byte t i] writes one 8-bit integer (as the single character with that code).
    The given integer is taken modulo 256. *)
val write_byte : t -> int -> unit

val write_sexp : ?hum:bool (** default is [false] *) -> t -> Sexp.t -> unit

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
val schedule_bigsubstring : t -> Bigsubstring.t -> unit

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
    complete (i.e. the [write()] system call returns).  If a prior write fails, then the
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

    [is_open t] is [not (is_closed t)]

    [with_close t ~f] runs [f ()], and closes [t] after [f] finishes or raises. *)
val close : ?force_close:unit Deferred.t -> t -> unit Deferred.t
val close_finished : t -> unit Deferred.t
val is_closed : t -> bool
val is_open   : t -> bool
val with_close : t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t

(* In addition to flushing its internal buffer prior to closing, a writer keeps track of
   producers that are feeding it data, so that when [Writer.close] is called, it does the
   following:

   + requests that the writer's producers flush their data to it
   + flushes the writer's internal buffer
   + calls [Unix.close] on the writer's underlying file descriptor

   [with_flushed_at_close t ~flushed ~f] calls [f] and adds [flushed] to the set of
   producers that should be flushed-at-close, for the duration of [f]. *)
val with_flushed_at_close
  :  t
  -> flushed:(unit -> unit Deferred.t)
  -> f:(unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** [bytes_to_write t] returns how many bytes have been requested to write but have not
    yet been written. *)
val bytes_to_write : t -> int

(** [bytes_written t] returns how many bytes have been written. *)
val bytes_written : t -> Int63.t

(** [bytes_received t] returns how many bytes have been received by the writer.  As long
    as the writer is running, [bytes_received = bytes_written + bytes_to_write]. *)
val bytes_received : t -> Int63.t

(** [with_file_atomic ?temp_file ?perm ?fsync file ~f] creates a writer to a temp file,
    feeds that writer to [f], and when the result of [f] becomes determined, atomically
    moves (i.e. uses [Unix.rename]) the temp file to [file].  If [file] currently exists,
    it will be replaced, even if it is read only.  The temp file will be [file] (or
    [temp_file] if supplied) suffixed by a unique random sequence of six characters.  The
    temp file may need to be removed in case of a crash so it may be prudent to choose a
    temp file that can be easily found by cleanup tools.

    If [fsync] is [true], the temp file will be flushed to disk before it takes the place
    of the target file, thus guaranteeing that the target file will always be in a sound
    state, even after a machine crash.  Since synchronization is extremely slow, this is
    not the default.  Think carefully about the event of machine crashes and whether you
    may need this option!

    We intend for [with_file_atomic] to preserve the behavior of the [open] system call,
    so if [file] does not exist, we will apply the umask to [perm].  If [file] does exist,
    [perm] will default to the file's current permissions rather than 0o666.

    [save] is a special case of [with_file_atomic] that atomically writes the given
    string to the specified file.

    [save_sexp] is a special case of [with_file_atomic] that atomically writes the
    given sexp to the specified file. *)

val with_file_atomic
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> string
  -> f:(t -> 'a Deferred.t)
  -> 'a Deferred.t

val save
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> string
  -> contents:string
  -> unit Deferred.t

(** [save_lines file lines] writes all lines in [lines] to [file], with each line followed
    by a newline. *)
val save_lines
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> string
  -> string list
  -> unit Deferred.t

(** [save_sexp t sexp] writes [sexp] to [t], followed by a newline.  To read a file
    produced using [save_sexp], one would typically use [Reader.load_sexp], which deals
    with the additional whitespace and works nicely with converting the sexp to a
    value. *)
val save_sexp
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> ?hum:bool (** default is [true] *)
  -> string
  -> Sexp.t
  -> unit Deferred.t

(** [transfer' t pipe_r f] repeatedly pulls values from [pipe_r], and feeds them to [f],
    which should in turn write them to [t].  It provides pushback to [pipe_r] by not
    reading when [t] cannot keep up with the data being pushed in.

    The [transfer'] stops and the result becomes determined when [pipe_r] reaches its EOF,
    when [t]'s consumer leaves, or when [stop] becomes determined.

    [transfer'] causes [Pipe.flushed] on [pipe_r]'s writer to ensure that the bytes have
    been flushed to [t] before returning.  It also waits on [Pipe.upstream_flushed] at
    shutdown.

    [transfer t pipe_r f] is equivalent to:

    {[
      transfer' t pipe_r (fun q -> Queue.iter q ~f; Deferred.unit)
    ]}
*)
val transfer'
  :  ?stop:unit Deferred.t
  -> t
  -> 'a Pipe.Reader.t
  -> ('a Queue.t -> unit Deferred.t)
  -> unit Deferred.t
val transfer
  :  ?stop:unit Deferred.t
  -> t
  -> 'a Pipe.Reader.t
  -> ('a -> unit)
  -> unit Deferred.t

(** [pipe t] returns the writing end of a pipe attached to [t] that pushes back when [t]
    cannot keep up with the data being pushed in.  Closing the pipe will close [t]. *)
val pipe : t -> string Pipe.Writer.t

(** [of_pipe info pipe_w] returns a writer [t] such that data written to [t] will appear
    on [pipe_w].  If either [t] or [pipe_w] are closed, the other is closed as well.

    [of_pipe] is implemented by attaching [t] to the write-end of a Unix pipe, and
    shuttling bytes from the read-end of the Unix pipe to [pipe_w]. *)
val of_pipe
  :  Info.t
  -> string Pipe.Writer.t
  -> (t * [`Closed_and_flushed_downstream of unit Deferred.t]) Deferred.t
