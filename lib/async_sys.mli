(** This module overrides everything in the [Sys] module that might block.  Functions do
    the same thing as their counterparts in [Sys], but instead return deferreds.  For a
    description of the semantics of their semantics see the documentation for the [Sys]
    module. *)

open Core.Std
open Import

val argv : string array
val executable_name : string

val file_exists     : string -> [`Yes | `No | `Unknown ] Deferred.t
val file_exists_exn : string -> bool Deferred.t

(** [when_file_exists ?poll_delay file] returns a deferred that becomes determined when
    [file] exists.  The default poll delay is 0.5 seconds.  It raises an exception if it
    can not check whether the file is there, in the same cases [file_exists] returns
    [`Unknown]. *)
val when_file_exists : ?poll_delay:Time.Span.t -> string -> unit Deferred.t

val is_directory : string -> [`Yes | `No | `Unknown ] Deferred.t
val is_file : string -> [`Yes | `No | `Unknown ] Deferred.t
val remove : string -> unit Deferred.t
val rename : string -> string -> unit Deferred.t
val command : string -> int Deferred.t
val chdir : string -> unit Deferred.t
val getcwd : unit -> string Deferred.t
val readdir : string -> string array Deferred.t

val interactive : bool ref
val os_type : string
val word_size : int

