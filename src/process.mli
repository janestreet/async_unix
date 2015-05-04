(** [Async.Process] is for creating child processes of the current process, and
    communicating with children via their stdin, stdout, and stderr.  [Async.Process] is
    the Async analog of [Core.Unix.create_process] and related functions. *)
open Core.Std
open Import

type t with sexp_of

(** accessors *)
val pid    : t -> Pid.t
val stdin  : t -> Writer.t
val stdout : t -> Reader.t
val stderr : t -> Reader.t

type env = Core.Std.Unix.env with sexp

(** [with_create_args] specifies the arguments used to create a child process. *)
type 'a with_create_args
  =  ?working_dir : string
  -> ?env         : env  (** default is [`Extend []] *)
  -> prog         : string
  -> args         : string list
  -> unit
  -> 'a

(** [create ~prog ~args ?working_dir ?env ()] uses [fork] and [exec] to create a child
    process that runs the executable [prog] with [args] as arguments.  It creates pipes to
    communicate with the child process's [stdin], [stdout], and [stderr].

    Unlike [exec], [args] should not include [prog] as the first argument.

    If [working_dir] is supplied, then the child process will [chdir()] there before
    calling [exec()].

    [env] specifies the environment of the child process.

    [create] returns [Error] if it is unable to create the child process.  This can happen
    in any number of situations (unable to fork, unable to create the pipes, unable to cd
    to [working_dir], unable to exec, etc.). *)
val create : t Or_error.t Deferred.t with_create_args

(** [wait t = Unix.waitpid (pid t)] *)
val wait : t -> Unix.Exit_or_signal.t Deferred.t

(** [collect_output_and_wait t] closes [stdin t] and then begins collecting the output
    produced on [t]'s [stdout] and [stderr], continuing to collect output until [t]
    terminates and the pipes for [stdout] and [stderr] are closed.  Usually when [t]
    terminates, the pipes are closed; however, [t] could fork other processes which
    survive after [t] terminates and in turn keep the pipes open -- [wait] will not become
    determined until both pipes are closed in all descendant processes. *)
module Output : sig
  type t =
    { stdout      : string
    ; stderr      : string
    ; exit_status : Unix.Exit_or_signal.t
    }
  with compare, sexp_of

  module Stable : sig
    module V1 : sig
      type nonrec t = t with compare, sexp
    end
  end
end
val collect_output_and_wait : t -> Output.t Deferred.t

(** [run] [create]s a process and [wait]s for it to complete.  If the process exits with
    an acceptable status, then [run] returns its stdout.  Acceptable statuses are zero,
    and any nonzero values specified in [accept_nonzero_exit].  If the process exits
    unacceptably, then [run] returns an error indicating what went wrong that includes
    stdout and stderr.

    Some care is taken so that an error displays nicely as a sexp---in particular, if the
    child's output can already be parsed as a sexp, then it will display as a sexp (rather
    than a sexp embedded in a string).  Also, if the output isn't a sexp, it will be split
    on newlines into a list of strings, so that it displays on multiple lines rather than
    a single giant line with embedded "\n"'s. *)
val run
  :  ?accept_nonzero_exit : int list  (** default is [] *)
  -> string Or_error.t Deferred.t with_create_args

(** [run_lines] is like [run] but returns the lines of stdout as a string list, using
    [String.split_lines]. *)
val run_lines
  :  ?accept_nonzero_exit : int list  (** default is [] *)
  -> string list Or_error.t Deferred.t with_create_args

(** [collect_stdout_and_wait] and [collect_stdout_lines_and_wait] are like [run] and
    [run_lines] but work from an existing process instead of creating a new one. *)

val collect_stdout_and_wait
  :  ?accept_nonzero_exit : int list  (** default is [] *)
  -> t
  -> string Or_error.t Deferred.t

val collect_stdout_lines_and_wait
  :  ?accept_nonzero_exit : int list  (** default is [] *)
  -> t
  -> string list Or_error.t Deferred.t
