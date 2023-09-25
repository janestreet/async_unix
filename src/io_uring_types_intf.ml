(** This module contains a copy of the interfaces of types used by Io_uring. We keep these
    separated from the main Io_uring_raw_intf where this modules is included. This is done
    such that we can have different implementation for all those types in the case where
    io_uring is/is not supported by the underlying system, such that we do not have any
    dependency on Ocaml_uring.
*)

module type FLAGS = sig
  (** A set of flags. *)
  type t = private int

  val of_int : int -> t

  (** [a + b] is the union of the sets. *)
  val ( + ) : t -> t -> t

  (** [mem x flags] is [true] iff [x] is a subset of [flags]. *)
  val mem : t -> t -> bool
end

module type S = sig
  module Int63 : sig
    (** The type of integers with exactly 63-bits. *)
    type t [@@immediate64]

    val of_int : int -> t
    val to_int : t -> int
  end

  module Poll_mask : sig
    include FLAGS

    val pollin : t
    val pollout : t
    val pollerr : t
    val pollhup : t
  end

  module Clock : sig
    type t =
      | Boottime
      | Realtime
  end

  module Statx : sig
    (** A statx struct. *)
    type t

    type kind =
      [ `Unknown
      | `Fifo
      | `Character_special
      | `Directory
      | `Block_device
      | `Regular_file
      | `Symbolic_link
      | `Socket
      ]

    (** Use [create] to make a statx result buffer to pass to {! statx}. *)
    val create : unit -> t

    module Flags : sig
      include FLAGS

      val empty : t
      val empty_path : t
      val no_automount : t
      val symlink_nofollow : t
      val statx_sync_as_stat : t
      val statx_force_sync : t
      val statx_dont_sync : t
    end

    module Attr : sig
      include FLAGS

      val compressed : t
      val immutable : t
      val append : t
      val nodump : t
      val encrypted : t
      val verity : t

      (** Since Linux 5.8 *)
      val dax : t

      (** [check ?mask attr t] will check if [t] is set in [attr].

          If [mask] is not [None] then it will first check the mask to see
          if the file attribute is supported and if not raise [Invalid_argument]. *)
      val check : ?mask:Int64.t -> Int64.t -> t -> bool
    end

    module Mask : sig
      include FLAGS

      val type' : t
      val mode : t
      val nlink : t
      val uid : t
      val gid : t
      val atime : t
      val mtime : t
      val ctime : t
      val ino : t
      val size : t
      val blocks : t

      (** All of the above flags. *)
      val basic_stats : t

      val btime : t

      (** As of Linux 5.8 *)
      val mnt_id : t

      (** As of Linux 6.1 *)
      val dioalign : t

      (** [check mask t] checks if [t] is set in [mask]. *)
      val check : Int64.t -> t -> bool
    end

    (** You may wish to use {! Mask.check} to verify the field has actually
        been returned with a sensible value first. *)

    val blksize : t -> Int64.t
    val attributes : t -> Int64.t
    val nlink : t -> Int64.t
    val uid : t -> Int64.t
    val gid : t -> Int64.t
    val ino : t -> Int64.t
    val size : t -> Int64.t
    val blocks : t -> Int64.t
    val attributes_mask : t -> Int64.t
    val rdev : t -> Int64.t
    val dev : t -> Int64.t
    val mask : t -> Int64.t

    (** See {! Mask.mnt_id}. *)
    val mnt_id : t -> Int64.t

    (** See {! Mask.dioalign}. *)
    val dio_mem_align : t -> Int64.t

    (** See {! Mask.dioalign}. *)
    val dio_offset_align : t -> Int64.t

    val atime_sec : t -> int64
    val btime_sec : t -> int64
    val ctime_sec : t -> int64
    val mtime_sec : t -> int64
    val atime_nsec : t -> int
    val btime_nsec : t -> int
    val ctime_nsec : t -> int
    val mtime_nsec : t -> int
    val mode : t -> int
    val perm : t -> int
    val kind : t -> kind
  end

  (** Flags that can be passed to openat2. *)
  module Open_flags : sig
    include FLAGS

    val empty : t
    val append : t
    val cloexec : t
    val creat : t
    val direct : t
    val directory : t
    val dsync : t
    val excl : t
    val largefile : t
    val noatime : t
    val noctty : t
    val nofollow : t
    val nonblock : t
    val path : t
    val sync : t
    val tmpfile : t
    val trunc : t
  end

  (** Flags that can be passed to openat2 to control path resolution. *)
  module Resolve : sig
    include FLAGS

    val empty : t
    val beneath : t
    val in_root : t
    val no_magiclinks : t
    val no_symlinks : t
    val no_xdev : t
    val cached : t
  end
end
