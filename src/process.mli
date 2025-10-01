(** [Async.Process] is for creating child processes of the current process, and
    communicating with children via their stdin, stdout, and stderr. [Async.Process] is
    the Async analog of [Core_unix.create_process] and related functions. *)

open! Core
open! Import

type t [@@deriving sexp_of]

(** accessors *)

val pid : t -> Pid.t
val stdin : t -> Writer.t
val stdout : t -> Reader.t
val stderr : t -> Reader.t

type env = Unix.env [@@deriving sexp]

(** [create ~prog ~args ()] uses [Core_unix.create_process_env] to create a child process
    that runs the executable [prog] with [args] as arguments.

    This creates pipes to communicate with the child process's [stdin], [stdout], and
    [stderr]. The caller is responsible for closing all these pipes. A lot of calls in the
    [Reader] module will close the underlying fd (e.g. iterating on [Reader.pipe]). You
    likely will have to explicitly call [Writer.close] on the [stdin] writer unless you
    call [collect_output_and_wait].

    Unlike [exec], [args] should not include [prog] as the first argument.

    If [buf_len] is supplied, it determines the size of the reader and writer buffers used
    to communicate with the child process's [stdin], [stdout], and [stderr].

    If [stdin] is supplied, then the writer to the child's stdin will have
    [~raise_when_consumer_leaves:false] and [~buffer_age_limit:`Unlimited], which makes it
    more robust.

    [env] specifies the environment of the child process.

    If [working_dir] is supplied, then the child process will [chdir()] there before
    calling [exec()].

    If [argv0] is given, it is used (instead of [prog]) as the first element of the [argv]
    array passed to [exec].

    [create] returns [Error] if it is unable to create the child process. This can happen
    in any number of situations (unable to fork, unable to create the pipes, unable to cd
    to [working_dir], unable to [exec] etc.). [create] does not return [Error] if the
    binary exits with non-zero exit code; instead, it returns [OK t], where [wait t]
    returns an [Error].

    See [Core_unix.create_process_env] for more details. *)
type 'a create :=
  ?argv0:string
  -> ?buf_len:int
  -> ?env:env (** default is [`Extend []] *)
  -> ?prog_search_path:string list
  -> ?stdin:string
  -> ?working_dir:string
  -> ?setpgid:Unix.Pgid.t
  -> prog:string
  -> args:string list
  -> unit
  -> 'a Deferred.t

val create : t Or_error.t create
val create_exn : t create

(** [wait t = Unix.waitpid (pid t)]. [wait]'s result becomes determined when the child
    process terminates, via exit or signal. [wait] does not touch [stdin], [stdout] or
    [stderr]. The caller should ensure that [stdout] and [stderr] are being drained in the
    background to avoid the child process blocking on a write due to pushback. See
    [collect_output_and_wait] for a higher-level alternative that handles this. *)
val wait : t -> Unix.Exit_or_signal.t Deferred.t

module Output : sig
  type t =
    { stdout : string
    ; stderr : string
    ; exit_status : Unix.Exit_or_signal.t
    }
  [@@deriving compare ~localize, sexp_of]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving compare ~localize, sexp]
    end
  end
end

(** [collect_output_and_wait t] closes [stdin t] and then begins collecting the output
    produced on [t]'s [stdout] and [stderr], continuing to collect output until [t]
    terminates and the pipes for [stdout] and [stderr] are closed. Usually when [t]
    terminates, the pipes are closed; however, [t] could fork other processes which
    survive after [t] terminates and in turn keep the pipes open --
    [collect_output_and_wait] will not become determined until both pipes are closed in
    all descendant processes. *)
val collect_output_and_wait : t -> Output.t Deferred.t

(** [run] [create]s a process, feeds it [stdin] if provided, and [wait]s for it to
    complete. If the process exits with an acceptable status, then [run] returns its
    stdout. If the process exits unacceptably, then [run] returns an error indicating what
    went wrong that includes stdout and stderr.

    Acceptable statuses are zero, and any nonzero values specified in
    [accept_nonzero_exit].

    Some care is taken so that an error displays nicely as a sexp---in particular, if the
    child's output can already be parsed as a sexp, then it will display as a sexp (rather
    than a sexp embedded in a string). Also, if the output isn't a sexp, it will be split
    on newlines into a list of strings, so that it displays on multiple lines rather than
    a single giant line with embedded "\n"'s.

    [run_lines] is like [run] but returns the lines of stdout as a string list, using
    [String.split_lines].

    [run_expect_no_output] is like [run] but expects the command to produce no output, and
    returns an error if the command does produce output.

    [run_forwarding] is like [run] but it forwards the stdout and stderr of the child
    process to the stdout and stderr of the calling process. One can choose to share the
    stdout/stderr file descriptors with the child process ([`Share]) or copy the data
    ([`Splice] [0], which is the default). Sharing the file descriptors minimizes
    performance overhead, but it may change behavior. For example if a shared fd
    corresponds to the terminal then the child process may choose to write colored output
    where it'd otherwise write ASCII. If there's an error (e.g. SIGPIPE) writing to a
    shared fd, that will be handled by the child process directly, instead of being
    handled in the parent. If [`Share] is passed, [run_forwarding] will wait for
    [Writer.stdout] and [Writer.stderr] to be flushed before spawning the child process to
    avoid interleaving output with anything previously written to the writers.

    [0] The name splice is a reference to the linux splice syscall, though note that it's
    not actually used here for portability reasons. *)
type 'a run :=
  ?accept_nonzero_exit:int list (** default is [] *)
  -> ?argv0:string
  -> ?env:env (** default is [`Extend []] *)
  -> ?prog_search_path:string list
  -> ?stdin:string
  -> ?working_dir:string
  -> prog:string
  -> args:string list
  -> unit
  -> 'a Deferred.t

val run : string Or_error.t run
val run_exn : string run
val run_lines : string list Or_error.t run
val run_lines_exn : string list run
val run_expect_no_output : unit Or_error.t run
val run_expect_no_output_exn : unit run
val run_forwarding : ?child_fds:[ `Share | `Splice ] -> unit Or_error.t run
val run_forwarding_exn : ?child_fds:[ `Share | `Splice ] -> unit run

(** [collect_stdout_and_wait] and [collect_stdout_lines_and_wait] are like [run] and
    [run_lines] but work from an existing process instead of creating a new one. *)
type 'a collect :=
  ?accept_nonzero_exit:int list (** default is [] *) -> t -> 'a Deferred.t

val collect_stdout_and_wait : string Or_error.t collect
val collect_stdout_and_wait_exn : string collect
val collect_stdout_lines_and_wait : string list Or_error.t collect
val collect_stdout_lines_and_wait_exn : string list collect

(** [forward_output_and_wait] is like [run_forwarding] but works from an existing process
    instead of creating a new one. *)

val forward_output_and_wait : unit Or_error.t collect
val forward_output_and_wait_exn : unit collect

module How_to_handle_output : sig
  type ('output, 'child_fds) t =
    | Collect_stdout_and_wait : (string, 'child_fds) t
    | Collect_stdout_lines_and_wait : (string list, 'child_fds) t
    | Forward_output_and_wait : 'child_fds -> (unit, 'child_fds) t
end

(** Same as the above run functions but uses a GADT to represent how to handle the output.
    When forwarding output, if you aren't sure you want the child process to share file
    descriptors with the parent, pass [false]. *)
val run'
  :  ('a, [ `Share_fds_with_child of bool ]) How_to_handle_output.t
  -> 'a Or_error.t run

val run'_exn : ('a, [ `Share_fds_with_child of bool ]) How_to_handle_output.t -> 'a run

(** Save as the above output-collecting functions but uses a GADT to represent how to
    handle the output. *)
val handle_output : ('a, unit) How_to_handle_output.t -> 'a Or_error.t collect

val handle_output_exn : ('a, unit) How_to_handle_output.t -> 'a collect

(** Sends a signal to this process. This is safe to call concurrently with [wait t], even
    if the Pid is reused after the process died.

    If the process was already terminated, the call succeeds and silently does nothing,
    regardless of whether or not the process was waited for. *)
val send_signal : t -> Signal.t -> unit

(** Similar to [send_signal], but additionally reports if a signal was actually sent, or a
    process was already terminated and waited for.

    Note that if you never called [wait] on this process, you will always get [`Ok], which
    can be surprising. This function is exposed for compatibility with the code that used
    [Signal_unix.send]. *)
val send_signal_compat : t -> Signal.t -> [ `Ok | `No_such_process ]

(** Similar to [send_signal_compat], but raises an exception on [`No_such_process]. Used
    to migrate the code that uses [Signal_unix.send_exn]. *)
val send_signal_compat_exn : t -> Signal.t -> unit

(** [Lines_or_sexp] is useful for rendering a string nicely in a sexp, avoiding quoting if
    the string is multi-line or was produced by converting a sexp to a string.
    [Output.sexp_of_t] uses [Lines_or_sexp] to nicely render stdout and stderr of a child
    process. *)
module Lines_or_sexp : sig
  type t [@@deriving sexp_of]

  val create : string -> t
end

(** Aliases exposed for other libraries that want to match Process's style of process
    manipulation, but not really part of the modules proper interface. *)
module Aliases : sig
  type nonrec 'a create = 'a create
  type nonrec 'a run = 'a run
  type nonrec 'a collect = 'a collect
end

module For_tests : sig
  val send_signal_internal
    :  t
    -> Signal.t
    -> [ `Ok | `No_such_process_internal | `No_such_process_OS ]
end

(** [create_with_shared_stderr] creates a child process with programmatic control over
    stdin and stdout, while stderr is shared with the parent process's stderr. This is
    useful for interactive programs that need to display UI on the terminal (via stderr)
    while still allowing programmatic input/output through stdin/stdout. *)
val create_with_shared_stderr : t Or_error.t create

val create_with_shared_stderr_exn : t create

module Expert : sig
  (** Construct a [t] from its constituent parts. No validation is performed (e.g.,
      [stdin] could be an arbitrary pipe, not necessarily one owned by [pid]).

      The intent is to allow interoperability between APIs expecting [t] and process
      spawning code that isn't using [Async]. *)
  val wrap_existing
    :  pid:Pid.t
    -> stdin:Writer.t
    -> stdout:Reader.t
    -> stderr:Reader.t
    -> prog:string
    -> args:string list
    -> working_dir:string option
    -> env:env
    -> wait:Unix.Exit_or_signal.t Deferred.t Lazy.t
         (** To match the semantics of the non-expert API, the underlying process should
             only be reaped (if applicable) once [wait] is forced. To avoid race
             conditions around PID reuse, [wait] must not become filled in a later async
             job than the one that is responsible for reaping the process. *)
    -> t
end
