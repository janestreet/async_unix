open! Core
open Import

type t

module Int63 = struct
  type t [@@immediate64]

  let of_int _ = assert false
  let to_int _ = assert false
end

module FLAGS = struct
  type t = int

  let empty = 0
  let _ = empty
  let of_int x = x
  let ( + ) = ( lor )
  let mem a b = a land b = a
end

module Poll_mask = struct
  include FLAGS

  let pollin = 0
  let pollout = 0
  let pollerr = 0
  let pollhup = 0
end

module Clock = struct
  type t =
    | Boottime
    | Realtime
end

module Statx = struct
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

  let create _ = assert false

  module Flags = struct
    include FLAGS

    let empty = 0
    let empty_path = 0
    let no_automount = 0
    let symlink_nofollow = 0
    let statx_sync_as_stat = 0
    let statx_force_sync = 0
    let statx_dont_sync = 0
  end

  module Attr = struct
    include FLAGS

    let compressed = 0
    let immutable = 0
    let append = 0
    let nodump = 0
    let encrypted = 0
    let verity = 0

    (** Since Linux 5.8 *)
    let dax = 0

    let check ?mask:_ _ _ = false
  end

  module Mask = struct
    include FLAGS

    let type' = 0
    let mode = 0
    let nlink = 0
    let uid = 0
    let gid = 0
    let atime = 0
    let mtime = 0
    let ctime = 0
    let ino = 0
    let size = 0
    let blocks = 0
    let basic_stats = 0
    let btime = 0

    (** As of Linux 5.8 *)
    let mnt_id = 0

    (** As of Linux 6.1 *)
    let dioalign = 0

    let check _ _ = false
  end

  let blksize _ = Int64.zero
  let attributes _ = Int64.zero
  let nlink _ = Int64.zero
  let uid _ = Int64.zero
  let gid _ = Int64.zero
  let ino _ = Int64.zero
  let size _ = Int64.zero
  let blocks _ = Int64.zero
  let attributes_mask _ = Int64.zero
  let rdev _ = Int64.zero
  let dev _ = Int64.zero
  let mask _ = Int64.zero

  (** See {! Mask.mnt_id}. *)
  let mnt_id _ = Int64.zero

  (** See {! Mask.dioalign}. *)
  let dio_mem_align _ = Int64.zero

  (** See {! Mask.dioalign}. *)
  let dio_offset_align _ = Int64.zero

  let atime_sec _ = Int64.zero
  let btime_sec _ = Int64.zero
  let ctime_sec _ = Int64.zero
  let mtime_sec _ = Int64.zero
  let atime_nsec _ = 0
  let btime_nsec _ = 0
  let ctime_nsec _ = 0
  let mtime_nsec _ = 0
  let mode _ = 0
  let perm _ = 0
  let kind _ = `Unknown
end

module Syscall_result = struct
  type t = (int, Unix.Error.t) Result.t [@@deriving sexp_of]
end

module Handle = struct
  type t

  let invariant _ = ()
end

(** Flags that can be passed to openat2. *)
module Open_flags = struct
  include FLAGS

  let empty = 0
  let append = 0
  let cloexec = 0
  let creat = 0
  let direct = 0
  let directory = 0
  let dsync = 0
  let excl = 0
  let largefile = 0
  let noatime = 0
  let noctty = 0
  let nofollow = 0
  let nonblock = 0
  let path = 0
  let sync = 0
  let tmpfile = 0
  let trunc = 0
end

(** Flags that can be passed to openat2 to control path resolution. *)
module Resolve = struct
  include FLAGS

  let empty = 0
  let beneath = 0
  let in_root = 0
  let no_magiclinks = 0
  let no_symlinks = 0
  let no_xdev = 0
  let cached = 0
end

let create ?polling_timeout:_ ~queue_depth:_ () =
  Or_error.unimplemented "Io_uring_raw.create"
;;

let exit _ = assert false
let supports_ext_arg _ = assert false
let submit _ = assert false
let cqe_ready _ ~timeout:_ = assert false
let fill_completions _ = assert false
let noop _ = assert false
let read _ = assert false
let write _ = assert false
let readv _ = assert false
let writev _ = assert false
let poll_add _ = assert false
let openat2 _ = assert false
let close _ = assert false
let unlink _ = assert false
let link _ = assert false
let timeout _ = assert false
let statx _ = assert false
let cancel _ = assert false
let syscall_result _ = assert false
let register_eventfd _ = assert false
