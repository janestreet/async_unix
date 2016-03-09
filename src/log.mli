(** A library for general logging.

    Although this module is fully Async-safe it exposes almost no Deferreds.  This is
    partially a design choice to minimize the impact of logging in code, and partially the
    result of organic design (i.e. older versions of this interface did the same thing).

    A (limited) [Blocking] module is supplied to accommodate the portion of a program that
    runs outside of Async.
*)
open! Core.Std
open! Import

module Level : sig
  (** Describes both the level of a log and the level of a message sent to a log.  There
      is an ordering to levels (`Debug < `Info < `Error), and a log set to a level will
      never display messages at a lower log level. *)
  type t =
    [ `Debug
    | `Info  (** default level *)
    | `Error
    ]
  [@@deriving bin_io, compare, sexp]

  include Stringable with type t := t

  val arg : t Command.Spec.Arg_type.t
end

module Message : sig
  type t [@@deriving sexp_of]

  val create
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> [ `String of string | `Sexp of Sexp.t ]
    -> t

  val time        : t -> Time.t
  val message     : t -> string
  val raw_message : t -> [ `String of string | `Sexp of Sexp.t ]
  val level       : t -> Level.t option
  val set_level   : t -> Level.t option -> t
  val tags        : t -> (string * string) list
  val add_tags    : t -> (string * string) list -> t

  module Stable : sig
    module V0 : sig
      (* [V0.bin_t] is the [Message.bin_t] in jane-111.18 and before *)
      type nonrec t = t [@@deriving bin_io, sexp]
    end

    module V2 : sig
      type nonrec t = t [@@deriving bin_io, sexp]
    end
  end
end

module Rotation : sig
  (** description of boundaries for file rotation.

      If all fields are [None] the file will never be rotated.  Any field set to [Some]
      will cause rotation to happen when that boundary is crossed.  Multiple boundaries
      may be set.  Log rotation always causes incrementing rotation conditions (e.g. size)
      to reset.

      The condition [keep] is special and does not follow the rules above.  When a log is
      rotated [keep] is examined and logs that do not fall under its instructions are
      deleted.  This deletion takes place on rotation only, and so may not happen.  The
      meaning of keep options are:

      - [`All] -- never delete
      - [`Newer_than span] --
      delete files with a timestamp older than [Time.sub (Time.now ()) span].  This
      normally means keeping files that contain at least one message logged within
      span.  If span is short enough this option can delete a just rotated file.
      - [`At_least i] -- keep the i most recent files

      Log rotation does not support symlinks, and you're encouraged to avoid them in
      production applications. Issues with symlinks:
      - You can't tail symlinks without being careful (e.g. you must remember to pass
      "-F" to
      `tail`).
      - Symlinks are hard to reason about when the program crashes, especially on
      startup (i.e., is the symlink pointing me at the right log file?).
      - Atomicity is hard.
      - Symlinks encourage tailing, which is a bad way to communicate information.
      - They complicate archiving processes (the symlink must be skipped).
  *)
  type t [@@deriving sexp_of]

  module type Id_intf = sig
    type t

    val create : Time.Zone.t -> t

    (* For any rotation scheme that renames logs on rotation, this defines how to do
       the renaming. *)
    val rotate_one : t -> t

    val to_string_opt    : t -> string option
    val of_string_opt    : string option -> t option
    val cmp_newest_first : t -> t -> int
  end

  val create
    :  ?messages     : int
    -> ?size         : Byte_units.t
    -> ?time         : Time.Ofday.t
    -> ?zone         : Time.Zone.t
    -> keep          : [ `All | `Newer_than of Time.Span.t | `At_least of int ]
    -> naming_scheme : [ `Numbered | `Timestamped | `Dated | `User_defined of (module Id_intf)]
    -> unit
    -> t

  (** Sane log rotation defaults.

      Writes dated log files. Files are rotated every time the day changes in the given
      zone (uses the machine's zone by default). If the dated log file already exists,
      it's appended to.

      Logs are never deleted. Best practice is to have an external mechanism archive old
      logs for long term storage. *)
  val default
    :  ?zone : Time.Zone.t
    -> unit
    -> t
end

module Output : sig
  type machine_readable_format = [`Sexp | `Sexp_hum | `Bin_prot ] [@@deriving sexp]
  type format = [ machine_readable_format | `Text ] [@@deriving sexp]

  type t

  (** [create f] returns a t, given a function that actually performs the final output
      work. It is the responsibility of the write function to contain all state, and to
      clean up after itself when it is garbage collected (which may require a finalizer).
      The function should avoid modifying the contents of the queue; it's reused for each
      [Output.t].

      The "stock" output modules support a sexp and bin_prot output format, and other
      output modules should make efforts to support them as well where it is
      meaningful/appropriate to do so.

      The unit Deferred returned by the function should not be fulfilled until the all of
      the messages in the given queue are completely handled (e.g. written to disk).

      An optional [rotate] function may be given which will be called when [Log.rotate t]
      is called while this output is in effect.  This is useful for programs that want
      very precise control over rotation. *)
  val create
    :  ?rotate:(unit -> unit Deferred.t)
    -> (Message.t Queue.t -> unit Deferred.t)
    -> t

  val stdout        : unit -> t
  val stderr        : unit -> t
  val writer        : format -> Writer.t -> t
  val file          : format -> filename:string -> t
  val rotating_file : format -> basename:string -> Rotation.t -> t
  (** returns a tail of the filenames. When rotate is called, the previous filename is put
      on the tail *)
  val rotating_file_with_tail : format -> basename:string -> Rotation.t -> t * string Tail.t

  (** See {!Async_extended.Std.Log} for syslog and colorized console output. *)
end

module Blocking : sig
  (** Async programs often have a non-Async portion that runs before the scheduler begins
      to capture command line options, do setup, read configs, etc.  This module provides
      limited global logging functions to be used during that period.  Calling these
      functions after the scheduler has started will raise an exception.  They otherwise
      behave similarly to the logging functions in the Async world. *)

  module Output : sig
    type t

    val stdout : t
    val stderr : t

    (** See {!Async_extended.Std.Log} for syslog and colorized console output. *)

    val create : (Message.t -> unit) -> t
  end

  val level : unit -> Level.t
  val set_level : Level.t -> unit
  val set_output : Output.t -> unit

  (** [raw] printf like logging for raw (no level) messages.  Raw messages are still
      output with a timestamp. *)
  val raw
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  (** [info] printf like logging at the `Info log level *)
  val info
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  (** [error] printf like logging at the `Info log level *)
  val error
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  (** [error] printf like logging at the `Info log level *)
  val debug
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a
end

type t [@@deriving sexp_of]

(** An interface for singleton logs *)
module type Global_intf = sig
  val log : t Lazy.t

  val level        : unit -> Level.t
  val set_level    : Level.t -> unit
  val set_output   : Output.t list -> unit
  val get_output   : unit -> Output.t list
  val set_on_error : [ `Raise | `Call of (Error.t -> unit) ] -> unit
  val would_log    : Level.t option -> bool

  (** logging functions as the functions that operate on a given log.  In this case they
      operate on a single log global to the module *)
  val raw
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val info
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val error
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val debug
    :  ?time : Time.t
    -> ?tags : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val flushed : unit -> unit Deferred.t

  val rotate : unit -> unit Deferred.t

  val printf
    :  ?level : Level.t
    -> ?time  : Time.t
    -> ?tags  : (string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val sexp
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> unit

  val string
    :  ?level : Level.t
    -> ?time  : Time.t
    -> ?tags  : (string * string) list
    -> string
    -> unit

  val message : Message.t -> unit
end

(** This functor can be called to generate "singleton" logging modules *)
module Make_global () : Global_intf

(** Programs that want simplistic single-channel logging can open this module.  It
    provides a global logging facility to a single output type at a single level.  More
    nuanced logging can be had by using the functions that operate on a distinct Log.t
    type. *)
module Global : Global_intf

(** messages sent at a level less than the current level will not be output. *)
val set_level : t -> Level.t -> unit

(** returns the last level passed to [set_level], which will be the log level
    checked as a threshold against the level of the next message sent. *)
val level : t -> Level.t

(** changes the output type of the log, which can be useful when daemonizing.
    The new output type will be applied to all subsequent messages. *)
val set_output : t -> Output.t list -> unit

val get_output : t -> Output.t list

(** if [`Raise] is given then background errors raised by logging will be raised to the
    monitor that was in scope when [create] was called.  Errors can be redirected anywhere
    by providing [`Call f]. *)
val set_on_error : t -> [ `Raise | `Call of (Error.t -> unit) ] -> unit

(** any call that writes to a log after [close] is called will raise. *)
val close : t -> unit

(** returns true if [close] has been called *)
val is_closed : t -> bool

(** returns a Deferred.t that is fulfilled when the last message delivered to t before the
    call to flushed is out the door. *)
val flushed : t -> unit Deferred.t

(** informs the current [Output]s to rotate if possible *)
val rotate : t -> unit Deferred.t

(** create a new log.  See [set_level], [set_on_error] and [set_output] for
    more. *)
val create
  :  level:Level.t
  -> output:Output.t list
  -> on_error:[ `Raise | `Call of (Error.t -> unit) ]
  -> t

(** [raw] printf like logging for raw (no level) messages.  Raw messages are still
    output with a timestamp. *)
val raw
  :  ?time : Time.t
  -> ?tags : (string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** [debug] printf like logging at the `Debug log level *)
val debug
  :  ?time : Time.t
  -> ?tags : (string * string) list
  -> t
  -> ('a, unit, string, unit) format4 -> 'a

(** [info] printf like logging at the `Info log level *)
val info
  :  ?time : Time.t
  -> ?tags : (string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** [error] printf like logging at the `Error log level *)
val error
  :  ?time : Time.t
  -> ?tags : (string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** [printf] generalized printf style logging *)
val printf
  :  ?level : Level.t
  -> ?time  : Time.t
  -> ?tags  : (string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** [sexp] log sexps directly *)
val sexp
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> unit

(** [string] logging of string values *)
val string
  :  ?level : Level.t
  -> ?time  : Time.t
  -> ?tags  : (string * string) list
  -> t
  -> string
  -> unit

(** [message] log a preexisting message *)
val message : t -> Message.t -> unit

(** [would_log] returns true if a message at the given log level would be logged if sent
    immediately. *)
val would_log : t -> Level.t option -> bool

module Reader : sig
  (** [pipe format filename] returns a pipe of all the messages in the log.  Errors
      encountered when opening or reading the file will be thrown as exceptions into the
      monitor current at the time pipe is called. *)
  val pipe
    :  [< Output.machine_readable_format ]
    -> string
    -> Message.t Pipe.Reader.t
end
