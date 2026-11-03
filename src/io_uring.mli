open! Core
open Import

type t = Io_uring_raw.t

val create : ?polling_timeout:int -> queue_depth:int -> unit -> t Or_error.t
val exit : t -> unit
val submit : t -> Io_uring_raw.Submit_outcome.t
val fill_completions : t -> int

val read
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?file_offset:int
  -> Fd.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> (int, Exn.t) Result.t Deferred.t

val read_file_descr_or_unix_error
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?file_offset:int
  -> File_descr.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> (int, Unix.Error.t) Result.t Deferred.t

val write
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?file_offset:int
  -> Fd.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> (int, Exn.t) Result.t Deferred.t

val readv
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?file_offset:int
  -> Fd.t
  -> Bigstring.t Unix.IOVec.t Array.t
  -> (int, Exn.t) Result.t Deferred.t

val writev
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?file_offset:int
  -> Fd.t
  -> Bigstring.t Unix.IOVec.t array
  -> (int, Exn.t) Result.t Deferred.t

val openat2
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> access:[ `R | `W | `RW ]
  -> flags:Io_uring_raw.Open_flags.t
  -> ?perm:Unix.file_perm
  -> resolve:Io_uring_raw.Resolve.t
  -> ?info:Info.t
  -> ?fd:Fd.t
  -> string
  -> (Fd.t, Exn.t) Result.t Deferred.t

val unlink
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> dir:bool
  -> ?fd:Fd.t
  -> string
  -> (unit, Exn.t) Result.t Deferred.t

val link
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?follow:bool
  -> ?force:bool
  -> target:string
  -> link_name:string
  -> unit
  -> (unit, Exn.t) Result.t Deferred.t

val statx
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?fd:Fd.t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> Io_uring_raw.Statx.Flags.t
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val stat
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val stat_or_unix_error
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> (Io_uring_raw.Statx.t, Unix.Error.t) Result.t Deferred.t

val fstat
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> Fd.t
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val lstat
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> (Io_uring_raw.Statx.t, Exn.t) Result.t Deferred.t

val lstat_or_unix_error
  :  t
  -> ?sqe_flags:Io_uring_raw.Sqe_flags.t @ local
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> (Io_uring_raw.Statx.t, Unix.Error.t) Result.t Deferred.t

val the_one_and_only : unit -> t option
