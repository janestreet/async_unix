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
  -> [ `Already_closed | `Error of exn | `Ok of int ] Deferred.t

val write
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> ?off:int
  -> ?len:int
  -> Bigstring.t
  -> [ `Already_closed | `Error of exn | `Ok of int ] Deferred.t

val readv
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> Bigstring.t Unix.IOVec.t Array.t
  -> [ `Already_closed | `Error of exn | `Ok of int ] Deferred.t

val writev
  :  t
  -> ?file_offset:int
  -> Fd.t
  -> Bigstring.t Unix.IOVec.t array
  -> [ `Already_closed | `Error of exn | `Ok of int ] Deferred.t

val openat2
  :  t
  -> access:[ `R | `W | `RW ]
  -> flags:Io_uring_raw.Open_flags.t
  -> ?perm:Unix.file_perm
  -> resolve:Io_uring_raw.Resolve.t
  -> ?info:Info.t
  -> ?fd:Fd.t
  -> string
  -> [ `Already_closed | `Error of exn | `Ok of Fd.t ] Deferred.t

val unlink
  :  t
  -> dir:bool
  -> ?fd:Fd.t
  -> string
  -> [ `Already_closed | `Error of exn | `Ok of unit ] Deferred.t

val link
  :  t
  -> ?follow:bool
  -> ?force:bool
  -> target:string
  -> link_name:string
  -> unit
  -> [ `Error of exn | `Ok of unit ] Deferred.t

val statx
  :  t
  -> ?fd:Fd.t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> Io_uring_raw.Statx.Flags.t
  -> [ `Already_closed | `Error of exn | `Ok of Io_uring_raw.Statx.t ] Deferred.t

val stat
  :  t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> [ `Error of exn | `Ok of Io_uring_raw.Statx.t ] Deferred.t

val fstat
  :  t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> Fd.t
  -> [ `Already_closed | `Error of exn | `Ok of Io_uring_raw.Statx.t ] Deferred.t

val lstat
  :  t
  -> ?mask:Io_uring_raw.Statx.Mask.t
  -> string
  -> [ `Error of exn | `Ok of Io_uring_raw.Statx.t ] Deferred.t

val the_one_and_only : unit -> t option
