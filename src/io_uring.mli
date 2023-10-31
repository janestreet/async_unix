open! Core
open Import

type t = Io_uring_raw.t

val create : ?polling_timeout:int -> queue_depth:int -> unit -> t Or_error.t
val exit : t -> unit
val submit : t -> int
val fill_completions : t -> int

val read
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> (int, Exn.t) Result.t Deferred.t

val read_file_descr
  :  t
  -> ?file_offset:int
  -> File_descr.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> (int, Exn.t) Result.t Deferred.t

val write
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> (int, Exn.t) Result.t Deferred.t

val readv
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> Bigstring.t Unix.IOVec.t Array.t
  -> (int, Exn.t) Result.t Deferred.t

val writev
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> Bigstring.t Unix.IOVec.t array
  -> (int, Exn.t) Result.t Deferred.t

val openat2
  :  t
  -> access:[ `R | `W | `RW ]
  -> flags:Io_uring_raw.Open_flags.t
  -> ?perm:Unix.file_perm
  -> resolve:Io_uring_raw.Resolve.t
  -> ?info:Info.t
  -> ?fd:Fd.t
  -> string
  -> (Fd.t, Exn.t) Result.t Deferred.t

val unlink : t -> dir:bool -> ?fd:Fd.t -> string -> (unit, Exn.t) Result.t Deferred.t

val link
  :  t
  -> ?follow:bool
  -> ?force:bool
  -> target:string
  -> link_name:string
  -> unit
  -> (unit, Exn.t) Result.t Deferred.t

val statx
  :  t
  -> ?fd:Fd.t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> Io_uring_raw.Statx.Flags.t
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val stat
  :  t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val fstat
  :  t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> Fd.t
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val lstat
  :  t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val the_one_and_only : unit -> t option
