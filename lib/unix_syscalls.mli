(** Unix_syscalls provides an interface to many of the functions in OCaml's standard Unix
    module.  It uses a deferred in the return type of functions that would block.  The
    idea is that in an async program one does not use the standard Unix module, since in
    doing so one could accidentally block the whole program.

    There are also a number of cosmetic changes (e.g. polymorphic variants) and other
    improvements (e.g. phantom types on sockets) over the standard Unix module. *)

open Core.Std
open Import

module Exit                   : Module_type.Exit
module Exit_or_signal         : Module_type.Exit_or_signal
module Exit_or_signal_or_stop : Module_type.Exit_or_signal_or_stop

val system     : string -> Exit_or_signal.t Deferred.t
val system_exn : string -> unit             Deferred.t

val getpid      : unit -> Pid.t
val getppid     : unit -> Pid.t option
val getppid_exn : unit -> Pid.t

(** [this_process_became_child_of_init] returns a deferred that becomes determined when
    the current process becomes a child of init(8).  This is useful to determine if one's
    parent has died, because in that case init will becomes one's parent.

    See Linux_ext.pr_set_pdeathsig : Signal.t -> unit for related way to
    get information about parent death

    @poll_delay controls how often to check *)
val this_process_became_child_of_init : ?poll_delay:Time.Span.t -> unit -> unit Deferred.t

val nice : int -> int

(** [cores ()] Returns the number of cores *)
val cores : (unit -> int Deferred.t) Or_error.t

type open_flag =
  [ `Rdonly
  | `Wronly
  | `Rdwr
  | `Nonblock
  | `Append
  | `Creat
  | `Trunc
  | `Excl
  | `Noctty
  | `Dsync
  | `Sync
  | `Rsync
  ]

type file_perm = int

val openfile : ?perm:file_perm -> string -> mode:open_flag list -> Fd.t Deferred.t

(** [with_file file ~mode ~perm ~f ?exclusive] opens [file], and applies [f] to the
    resulting file descriptor.  When the result of [f] becomes determined, it closes the
    descriptor and returns the result of [f].  If [exclusive] is supplied, then the file
    descriptor is locked before calling [f] and unlocked after calling [f]. *)
val with_file
  :  ?exclusive:[`Read | `Write]
  -> ?perm:file_perm
  -> string
  -> mode:open_flag list
  -> f:(Fd.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [close fd] closes the file descriptor [fd], and raises an exception if [fd] has
    already been closed.  *)
val close : Fd.t -> unit Deferred.t

val lseek : Fd.t -> int64 -> mode:[ `Set | `Cur | `End ] -> int64 Deferred.t

val truncate : string -> len:int64 -> unit Deferred.t

val ftruncate : Fd.t -> len:int64 -> unit Deferred.t

val fsync : Fd.t -> unit Deferred.t

val fdatasync : Fd.t -> unit Deferred.t

val sync : unit -> unit Deferred.t

(** [lockf fd read_or_write ?len] exclusively locks for reading/writing the section of the
    open file [fd] specified by the current file position and [len] (see man lockf).  It
    returns when the lock has been acquired.  It raises if [fd] is closed. *)
val lockf : ?len:Int64.t -> Fd.t -> [`Read | `Write] -> unit Deferred.t

(** [try_lockf fd read_or_write ?len] attempts to exclusively lock for reading/writing the
    section of the open file [fd] specified by the current file position and [len] (see
    man lockf).  It returns [true] if it acquired the lock.  It raises if [fd] is
    closed. *)
val try_lockf : ?len:Int64.t -> Fd.t -> [`Read | `Write] -> bool

(** [lockf_is_locked fd ?len] checks the lock on section of the open file [fd] specified
    by the current file position and [len] (see man lockf).  If the section is unlocked or
    locked by this process, it returns true, else it returns false.  It raises if [fd] is
    closed. *)
val test_lockf : ?len:Int64.t -> Fd.t -> bool

(** [unlockf fd ?len] unlocks the section of the open file [fd] specified by the current
    file position and [len] (see man lockf).  It raises if [fd] is closed. *)
val unlockf : ?len:Int64.t -> Fd.t -> unit

module File_kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
end

module Stats : sig
  type t =
    { dev : int;
      ino : int;
      kind : File_kind.t;
      perm : file_perm;
      nlink : int;
      uid : int;
      gid : int;
      rdev : int;
      size : int64;
      atime : Time.t;
      mtime : Time.t;
      ctime : Time.t;
    }
  with fields, sexp

  val to_string : t -> string
end

val fstat : Fd.t -> Stats.t Deferred.t

val stat : string -> Stats.t Deferred.t

val lstat : string -> Stats.t Deferred.t

val unlink : string -> unit Deferred.t

val rename : src:string -> dst:string -> unit Deferred.t

val link : ?force : bool -> target:string -> link_name:string -> unit -> unit Deferred.t

val chmod : string -> perm:file_perm -> unit Deferred.t

val fchmod : Fd.t -> perm:file_perm -> unit Deferred.t

val chown : string -> uid:int -> gid:int -> unit Deferred.t

val fchown : Fd.t -> uid:int -> gid:int -> unit Deferred.t

val access
  :  string
  -> [ `Read | `Write | `Exec | `Exists ] list
  -> (unit, exn) Result.t Deferred.t

val access_exn
  :  string
  -> [ `Read | `Write | `Exec | `Exists ] list
  -> unit Deferred.t

val set_close_on_exec : Fd.t -> unit

val clear_close_on_exec : Fd.t -> unit

val mkdir : ?p:unit -> ?perm:file_perm -> string -> unit Deferred.t

val rmdir : string -> unit Deferred.t

val chdir : string -> unit Deferred.t

val getcwd : unit -> string Deferred.t

val chroot : string -> unit Deferred.t

type dir_handle = Unix.dir_handle

val opendir : string -> dir_handle Deferred.t

val readdir : dir_handle -> string Deferred.t

val rewinddir : dir_handle -> unit Deferred.t

val closedir : dir_handle -> unit Deferred.t

val pipe : unit -> ([`Reader of Fd.t] * [`Writer of Fd.t]) Deferred.t

val symlink : src:string -> dst:string -> unit Deferred.t

val readlink : string -> string Deferred.t

val mkstemp : string -> (string * Fd.t) Deferred.t

val mkdtemp : string -> string Deferred.t

(** Time functions. *)
type process_times =
  Unix.process_times =
    { tms_utime : float;          (** User time for the process *)
      tms_stime : float;          (** System time for the process *)
      tms_cutime : float;         (** User time for the children processes *)
      tms_cstime : float;         (** System time for the children processes *)
    }

val times : unit -> process_times

type tm = Unix.tm =
    { tm_sec : int;                       (** Seconds 0..59 *)
      tm_min : int;                       (** Minutes 0..59 *)
      tm_hour : int;                      (** Hours 0..23 *)
      tm_mday : int;                      (** Day of month 1..31 *)
      tm_mon : int;                       (** Month of year 0..11 *)
      tm_year : int;                      (** Year - 1900 *)
      tm_wday : int;                      (** Day of week (Sunday is 0) *)
      tm_yday : int;                      (** Day of year 0..365 *)
      tm_isdst : bool;                    (** Daylight time savings in effect *)
    }
val time : unit -> float
val gettimeofday : unit -> float
val gmtime : float -> tm
val localtime : float -> tm
val mktime : tm -> float * tm
val utimes : string -> access:float -> modif:float -> unit Deferred.t


val environment : unit -> string array
val getenv : string -> string option
val getenv_exn : string -> string
val putenv : key:string -> data:string -> unit

(* val graceful_kill : pid:int -> unit Deferred.t *)

(** [fork_exec ~prog ~args ?path ?env] forks and execs [prog] with [args], and returns the
    child pid.  If [use_path = true] (the default) and [prog] doesn't contain a slash,
    then [fork_exec] searches the PATH environment variable for [prog].  If [env] is
    supplied, it is used as the environment when [prog] is executed. *)
val fork_exec
  :  prog:string
  -> args:string list
  -> ?use_path:bool
  -> ?env:string list
  -> unit
  -> Pid.t Deferred.t

type wait_on =
  [ `Any
  | `Group of Pid.t
  | `My_group
  | `Pid of Pid.t
  ]
with sexp

val wait                 : wait_on -> (Pid.t * Exit_or_signal.t        ) Deferred.t
val wait_nohang          : wait_on -> (Pid.t * Exit_or_signal.t        ) option
val wait_untraced        : wait_on -> (Pid.t * Exit_or_signal_or_stop.t) Deferred.t
val wait_nohang_untraced : wait_on -> (Pid.t * Exit_or_signal_or_stop.t) option

module Inet_addr : sig
  type t = Unix.Inet_addr.t with bin_io, sexp

  (* same as Core.Unix *)
  val of_string : string -> t
  val to_string : t -> string
  val bind_any       : t
  val bind_any_inet6 : t
  val localhost       : t (* [127.0.0.1] *)
  val localhost_inet6 : t (* ([::1]) *)

  (** [of_string_or_getbyname hostname] does a DNS lookup of hostname and returns the
      resulting IP address.  The implemenation sequentializes all calls so that only a
      single call is active at a time.  The is because we've observed thread safety issues
      with certain versions of winbind using "wins" name resolution. *)
  val of_string_or_getbyname : string -> t Deferred.t
end

module Protocol_family : sig
  type t = Unix.Protocol_family.t
end

val socketpair : unit -> Fd.t * Fd.t

module Socket : sig
  type unix = [ `Unix of string ] with sexp, bin_io
  type inet = [ `Inet of Inet_addr.t * int ] with sexp, bin_io
  type 'a addr = 'a constraint 'a = [< unix | inet ] with sexp, bin_io

  val address_string_of_inet : inet -> string
  val string_of_inet : inet -> string
  val string_of_unix : unix -> string

  module Address : sig
    type t = [ unix | inet ] with sexp, bin_io

    val unix : string -> unix
    val inet : Inet_addr.t -> port:int -> inet
    val inet_addr_any : port:int -> inet

    val to_string : 'a addr -> string
    val to_unix : 'a addr -> Core.Std.Unix.sockaddr
  end

  module Family : sig
    type 'a t constraint 'a = 'a addr

              val unix : unix t
              val inet : inet t
  end

  (** Sockets have a phantom type parameter that tracks the state of the socket
      in order to eliminate certain errors in which socket functions are called
      in the wrong order.  Initially, a socket is `Unconnected.  As various
      socket functions are called, they return a socket with a new phantom state.
      Here is a chart of the allowed state transitions.

      Unconnected ---connect--> Active
      | ---bind--> Bound ---listen--> Passive ---accept---> Active
  *)
  type ('a, 'b) t
  constraint 'a = [< `Unconnected | `Bound | `Passive | `Active ]
  constraint 'b = 'b addr

  module Type : sig
    type 'a t constraint 'a = 'a addr

    val tcp : inet t
    val udp : inet t
    val unix : unix t
  end

  val create : 'addr Type.t -> ([ `Unconnected ], 'addr) t

  val connect
    :  ([ `Unconnected ], 'addr) t
    -> 'addr
    -> ([ `Active ], 'addr) t Deferred.t

  val connect_interruptible
    :  ([ `Unconnected ], 'addr) t
    -> 'addr
    -> interrupt:(unit Deferred.t)
    -> [ `Ok of ([ `Active ], 'addr) t
       | `Interrupted
       ] Deferred.t

  (* [bind socket addr] sets close_on_exec for the fd of [socket].  *)
  val bind : ([ `Unconnected ], 'addr) t -> 'addr -> ([ `Bound ], 'addr) t Deferred.t

  val listen
    :  ?max_pending_connections:int  (* default: 10 *)
    -> ([ `Bound ], 'addr) t
    -> ([ `Passive ], 'addr) t

  val accept : ([ `Passive ], 'addr) t -> (([ `Active ], 'addr) t * 'addr) Deferred.t

  val accept_interruptible
    :  ([ `Passive ], 'addr) t
    -> interrupt:(unit Deferred.t)
    -> [ `Ok of (([ `Active ], 'addr) t * 'addr)
       | `Interrupted
       ] Deferred.t

  val shutdown : ('a, 'addr) t -> [ `Receive | `Send | `Both ] -> unit

  val fd : ('a, 'addr) t -> Fd.t

  val of_fd : Fd.t -> 'addr Type.t -> ('a, 'addr) t

  val getsockname : ('a, 'addr) t -> 'addr

  val getpeername : ('a, 'addr) t -> 'addr

  module Opt : sig
    type 'a t

    val debug : bool t
    val broadcast : bool t
    val reuseaddr : bool t
    val keepalive : bool t
    val dontroute : bool t
    val oobinline : bool t
    val acceptconn : bool t
    val nodelay : bool t

    val sndbuf : int t
    val rcvbuf : int t
    val error : int t
    val typ : int t
    val rcvlowat : int t
    val sndlowat : int t

    val linger : int option t

    val rcvtimeo : float t
    val sndtimeo : float t
  end

  val getopt : ('a, 'addr) t -> 'c Opt.t -> 'c

  val setopt : ('a, 'addr) t -> 'c Opt.t -> 'c -> unit
end

module Host : sig
  type t = Unix.Host.t =
    { name : string;
      aliases : string array;
      family : Protocol_family.t;
      addresses : Inet_addr.t array;
    }

  val getbyname     : string      -> t option Deferred.t
  val getbyname_exn : string      -> t        Deferred.t
  val getbyaddr     : Inet_addr.t -> t option Deferred.t
  val getbyaddr_exn : Inet_addr.t -> t        Deferred.t
end

val gethostname : unit -> string

val getuid : unit -> int

val geteuid : unit -> int

val getgid : unit -> int

val getegid : unit -> int

val setuid : int -> unit

type error =
 Unix.error =
| E2BIG | EACCES | EAGAIN | EBADF | EBUSY | ECHILD | EDEADLK | EDOM | EEXIST
| EFAULT | EFBIG | EINTR | EINVAL | EIO | EISDIR | EMFILE | EMLINK
| ENAMETOOLONG | ENFILE | ENODEV | ENOENT | ENOEXEC | ENOLCK | ENOMEM | ENOSPC
| ENOSYS | ENOTDIR | ENOTEMPTY | ENOTTY | ENXIO | EPERM | EPIPE | ERANGE
| EROFS | ESPIPE | ESRCH | EXDEV | EWOULDBLOCK | EINPROGRESS | EALREADY
| ENOTSOCK | EDESTADDRREQ | EMSGSIZE | EPROTOTYPE | ENOPROTOOPT
| EPROTONOSUPPORT | ESOCKTNOSUPPORT | EOPNOTSUPP | EPFNOSUPPORT | EAFNOSUPPORT
| EADDRINUSE | EADDRNOTAVAIL | ENETDOWN | ENETUNREACH | ENETRESET
| ECONNABORTED | ECONNRESET | ENOBUFS | EISCONN | ENOTCONN | ESHUTDOWN
| ETOOMANYREFS | ETIMEDOUT | ECONNREFUSED | EHOSTDOWN | EHOSTUNREACH | ELOOP
| EOVERFLOW | EUNKNOWNERR of int
with sexp

exception Unix_error of error * string * string

module Terminal_io : sig
  type t = Caml.Unix.terminal_io = {
    (* Input modes: *)
    mutable c_ignbrk : bool;  (** Ignore the break condition. *)
    mutable c_brkint : bool;  (** Signal interrupt on break condition. *)
    mutable c_ignpar : bool;  (** Ignore characters with parity errors. *)
    mutable c_parmrk : bool;  (** Mark parity errors. *)
    mutable c_inpck : bool;   (** Enable parity check on input. *)
    mutable c_istrip : bool;  (** Strip 8th bit on input characters. *)
    mutable c_inlcr : bool;   (** Map NL to CR on input. *)
    mutable c_igncr : bool;   (** Ignore CR on input. *)
    mutable c_icrnl : bool;   (** Map CR to NL on input. *)
    mutable c_ixon : bool;    (** Recognize XON/XOFF characters on input. *)
    mutable c_ixoff : bool;   (** Emit XON/XOFF chars to control input flow. *)
    (* Output modes: *)
    mutable c_opost : bool;   (** Enable output processing. *)
    (* Control modes: *)
    mutable c_obaud : int;    (** Output baud rate (0 means close connection).*)
    mutable c_ibaud : int;    (** Input baud rate. *)
    mutable c_csize : int;    (** Number of bits per character (5-8). *)
    mutable c_cstopb : int;   (** Number of stop bits (1-2). *)
    mutable c_cread : bool;   (** Reception is enabled. *)
    mutable c_parenb : bool;  (** Enable parity generation and detection. *)
    mutable c_parodd : bool;  (** Specify odd parity instead of even. *)
    mutable c_hupcl : bool;   (** Hang up on last close. *)
    mutable c_clocal : bool;  (** Ignore modem status lines. *)
    (* Local modes: *)
    mutable c_isig : bool;    (** Generate signal on INTR, QUIT, SUSP. *)
    mutable c_icanon : bool;  (** Enable canonical processing
                                 (line buffering and editing) *)
    mutable c_noflsh : bool;  (** Disable flush after INTR, QUIT, SUSP. *)
    mutable c_echo : bool;    (** Echo input characters. *)
    mutable c_echoe : bool;   (** Echo ERASE (to erase previous character). *)
    mutable c_echok : bool;   (** Echo KILL (to erase the current line). *)
    mutable c_echonl : bool;  (** Echo NL even if c_echo is not set. *)
    (* Control characters: *)
    mutable c_vintr : char;   (** Interrupt character (usually ctrl-C). *)
    mutable c_vquit : char;   (** Quit character (usually ctrl-\). *)
    mutable c_verase : char;  (** Erase character (usually DEL or ctrl-H). *)
    mutable c_vkill : char;   (** Kill line character (usually ctrl-U). *)
    mutable c_veof : char;    (** End-of-file character (usually ctrl-D). *)
    mutable c_veol : char;    (** Alternate end-of-line char. (usually none). *)
    mutable c_vmin : int;     (** Minimum number of characters to read
                                 before the read request is satisfied. *)
    mutable c_vtime : int;    (** Maximum read wait (in 0.1s units). *)
    mutable c_vstart : char;  (** Start character (usually ctrl-Q). *)
    mutable c_vstop : char;   (** Stop character (usually ctrl-S). *)
  }
  type setattr_when = Caml.Unix.setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

  val tcgetattr : Fd.t -> t Deferred.t
  val tcsetattr : t -> Fd.t -> mode:setattr_when -> unit Deferred.t
end

(** Structure of entries in the [passwd] database. *)
module Passwd : sig
  type t =
    { name : string;
      passwd : string;
      uid : int;
      gid : int;
      gecos : string;
      dir : string;
      shell : string;
    }
  with fields, sexp

  val getbyname : string -> t option Deferred.t
  val getbyname_exn : string -> t Deferred.t
  val getbyuid : int -> t option Deferred.t
  val getbyuid_exn : int -> t Deferred.t
end

(** Structure of entries in the [groups] database. *)
module Group : sig
  type t =
    { name : string;
      passwd : string;
      gid : int;
      mem : string array;
    }
  with fields, sexp

  val getbyname : string -> t option Deferred.t
  val getbyname_exn : string -> t Deferred.t
  val getbygid : int -> t option Deferred.t
  val getbygid_exn : int -> t Deferred.t
end

(** Return the login name of the user executing the process.

    This returns a deferred because the username may need to be looked up in
    what is essentially a database elsewhere on the network (winbound user, or
    NIS). *)
val getlogin : unit -> string Deferred.t

val wordexp
  :  ?flags : [ `No_cmd | `Show_err | `Undef ] list
  -> string
  -> string array Deferred.t
