module Async_signal = Signal
open Core.Std
open Import

module U = Unix
module File_descr = Unix.File_descr
module Exit = Unix.Exit
module Exit_or_signal = Unix.Exit_or_signal
module Exit_or_signal_or_stop = Unix.Exit_or_signal_or_stop

type error = Unix.error =
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

exception Unix_error = Unix.Unix_error

let ready_to = Fd.ready_to
let close = Fd.close

let system s = In_thread.syscall_exn (fun () -> Unix.system s)

let system_exn s =
  system s
  >>| fun status ->
  if not (Result.is_ok status) then
    failwiths "system failed" (s, status) <:sexp_of< string * Exit_or_signal.t >>
;;

let getpid      () = Unix.getpid      ()
let getppid     () = Unix.getppid     ()
let getppid_exn () = Unix.getppid_exn ()

let this_process_became_child_of_init ?(poll_delay = sec 1.) () =
  Deferred.create (fun i ->
    Clock.every poll_delay ~stop:(Ivar.read i) (fun () ->
      if getppid_exn () = Pid.init then Ivar.fill i ()))
;;

let nice i = Unix.nice i

let cores = Or_error.map Linux_ext.cores ~f:(fun cores () -> In_thread.syscall_exn cores)

(* basic input/output *)

let convert_open_flag = function
  | `Rdonly -> U.O_RDONLY
  | `Wronly -> U.O_WRONLY
  | `Rdwr -> U.O_RDWR
  | `Nonblock -> U.O_NONBLOCK
  | `Append -> U.O_APPEND
  | `Creat -> U.O_CREAT
  | `Trunc -> U.O_TRUNC
  | `Excl -> U.O_EXCL
  | `Noctty -> U.O_NOCTTY
  | `Dsync -> U.O_DSYNC
  | `Sync -> U.O_SYNC
  | `Rsync -> U.O_RSYNC
;;

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

type file_perm = int with sexp

let openfile ?perm file ~mode =
  let mode = List.map mode ~f:convert_open_flag in
  In_thread.syscall_exn (fun () -> Unix.openfile ?perm file ~mode)
  >>| fun file_descr ->
  Fd.create Fd.Kind.File file_descr ~name:file
;;

let lseek fd pos ~mode =
  let mode =
    match mode with
    | `Set -> U.SEEK_SET
    | `Cur -> U.SEEK_CUR
    | `End -> U.SEEK_END
  in
  Fd.syscall_in_thread_exn fd (fun file_descr -> Unix.lseek file_descr pos ~mode)
;;

let truncate filename ~len = In_thread.syscall_exn (fun () -> Unix.truncate filename ~len)

let ftruncate fd ~len =
  Fd.syscall_in_thread_exn fd (fun file_descr -> Unix.ftruncate file_descr ~len)
;;

let fsync fd = Fd.syscall_in_thread_exn fd Unix.fsync

let fdatasync fd = Fd.syscall_in_thread_exn fd Unix.fdatasync
;;

let sync () = In_thread.syscall_exn Unix.sync

let lockf ?(len = 0L) fd read_or_write =
  let mode =
    match read_or_write with
    | `Read -> U.F_RLOCK
    | `Write -> U.F_LOCK
  in
  Fd.syscall_in_thread_exn fd (fun file_descr -> Unix.lockf file_descr ~mode ~len)
;;

let try_lockf ?(len = 0L) fd read_or_write =
  let mode =
    match read_or_write with
    | `Read -> U.F_TRLOCK
    | `Write -> U.F_TLOCK
  in
  Fd.syscall_exn fd (fun file_descr ->
    try Unix.lockf file_descr ~mode ~len; true
    with Unix_error ((EACCES | EAGAIN), _, _) -> false)
;;

let test_lockf ?(len = 0L) fd =
  Fd.syscall_exn fd (fun file_descr ->
    try Unix.lockf file_descr ~mode:U.F_TEST ~len; true
    with Unix_error ((EACCES | EAGAIN), _, _) -> false)
;;

let unlockf ?(len = 0L) fd =
  Fd.syscall_exn fd (fun file_descr ->
    Unix.lockf file_descr ~mode:U.F_ULOCK ~len)
;;

let with_file ?exclusive ?perm file ~mode ~f =
  let doit f = openfile file ~mode ?perm >>= fun fd -> Fd.with_close fd ~f in
  match exclusive with
  | None -> doit f
  | Some read_or_write ->
    doit (fun fd ->
      lockf fd read_or_write
      >>= fun () ->
      Monitor.protect (fun () -> f fd) ~finally:(fun () -> unlockf fd; Deferred.unit))
;;

(* file status *)

module File_kind = struct
  type t =
    [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ] with sexp

  let of_unix = function
    | U.S_REG -> `File
    | U.S_DIR -> `Directory
    | U.S_CHR -> `Char
    | U.S_BLK -> `Block
    | U.S_LNK -> `Link
    | U.S_FIFO -> `Fifo
    | U.S_SOCK -> `Socket
  ;;

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Stats = struct
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

  let of_unix u = {
    dev = u.U.st_dev;
    ino = u.U.st_ino;
    kind = File_kind.of_unix u.U.st_kind;
    perm = u.U.st_perm;
    nlink = u.U.st_nlink;
    uid = u.U.st_uid;
    gid = u.U.st_gid;
    rdev = u.U.st_rdev;
    size = u.U.st_size;
    atime = Time.of_float u.U.st_atime;
    mtime = Time.of_float u.U.st_mtime;
    ctime = Time.of_float u.U.st_ctime;
  }

  let to_string t = Sexp.to_string (sexp_of_t t)
end

let fstat fd = Fd.syscall_in_thread_exn fd Unix.fstat >>| Stats.of_unix

let stat filename =
  In_thread.syscall_exn (fun () -> Unix.stat filename) >>| Stats.of_unix
;;

let lstat filename =
  In_thread.syscall_exn (fun () -> Unix.lstat filename) >>| Stats.of_unix
;;

(* operations on filenames *)

let unlink filename = In_thread.syscall_exn (fun () -> Unix.unlink filename)
let rename ~src ~dst = In_thread.syscall_exn (fun () -> Unix.rename ~src ~dst)

let link ?force ~target ~link_name () =
  In_thread.syscall_exn (fun () -> Unix.link ?force ~target ~link_name ())

(* file permission and ownership *)

let chmod filename ~perm =
  In_thread.syscall_exn (fun () -> Unix.chmod filename ~perm)
;;

let fchmod fd ~perm =
  Fd.syscall_in_thread_exn fd (fun file_descr -> Unix.fchmod file_descr ~perm)
;;

let chown filename ~uid ~gid =
  In_thread.syscall_exn (fun () -> Unix.chown filename ~uid ~gid)
;;

let fchown fd ~uid ~gid =
  Fd.syscall_in_thread_exn fd (fun file_descr -> Unix.fchown file_descr ~uid ~gid)
;;

let access filename perm =
  Monitor.try_with (fun () -> In_thread.syscall_exn (fun () -> Unix.access filename perm))
  >>| function
    | Ok res -> res
    | Error exn -> Error (Monitor.extract_exn exn)
;;

let access_exn filename perm =
  In_thread.syscall_exn (fun () -> Unix.access_exn filename perm)
;;

(* operations on file descriptors *)

let set_close_on_exec fd = Fd.with_file_descr_exn fd Unix.set_close_on_exec
let clear_close_on_exec fd = Fd.with_file_descr_exn fd Unix.clear_close_on_exec

let mkdir ?p ?(perm = 0o777) dirname =
  let mkdir dirname = In_thread.syscall_exn (fun () -> Unix.mkdir dirname ~perm) in
  match p with
  | None -> mkdir dirname
  | Some () ->
    let rec loop acc dirname =
      match Filename.split dirname with
      | "." as base, "." -> base, acc
      | "/" as base, "/" -> base, acc
      | rest, dir -> loop (dir :: acc) rest
    in
    (* if [dirname = "/a/b/c"], then [base = "/"] and [dirs = ["a"; "b"; "c"]] *)
    let base, dirs = loop [] dirname in
    Deferred.List.fold dirs ~init:base ~f:(fun acc dir ->
      let dir = String.concat [acc; "/"; dir] in
      Monitor.try_with (fun () -> mkdir dir)
      >>| function
      | Ok () -> dir
      | Error e ->
        match Monitor.extract_exn e with
        | Unix_error (EEXIST, _, _) -> dir
        | _ -> raise e)
    >>| fun (_ : string) ->
    ()
;;

let rmdir  dirname = In_thread.syscall_exn (fun () -> Unix.rmdir  dirname)
let chdir  dirname = In_thread.syscall_exn (fun () -> Unix.chdir  dirname)
let chroot dirname = In_thread.syscall_exn (fun () -> Unix.chroot dirname)

let getcwd () = In_thread.syscall_exn (fun () -> Unix.getcwd ())

type dir_handle = Unix.dir_handle
let opendir   dirname = In_thread.syscall_exn (fun () -> Unix.opendir   dirname)
let readdir   handle  = In_thread.syscall_exn (fun () -> Unix.readdir   handle )
let rewinddir handle  = In_thread.syscall_exn (fun () -> Unix.rewinddir handle )
let closedir  handle  = In_thread.syscall_exn (fun () -> Unix.closedir  handle )

let pipe () =
  In_thread.syscall_exn (fun () -> Unix.pipe ())
  >>| fun (reader, writer) ->
  (`Reader (Fd.create Fd.Kind.Fifo reader ~name:"<pipe reader>"),
   `Writer (Fd.create Fd.Kind.Fifo writer ~name:"<pipe writer>"))
;;

(* symlinks *)
let symlink ~src ~dst = In_thread.syscall_exn (fun () -> Unix.symlink ~src ~dst)
let readlink filename = In_thread.syscall_exn (fun () -> Unix.readlink filename)
let mkdtemp filename = In_thread.syscall_exn (fun () -> Unix.mkdtemp filename)

let mkstemp filename =
  In_thread.syscall_exn (fun () ->
    let name, file_descr = Unix.mkstemp filename in
    (name, Fd.create Fd.Kind.File file_descr ~name))
;;

type process_times =
Unix.process_times =
{ tms_utime : float;          (** User time for the process *)
  tms_stime : float;          (** System time for the process *)
  tms_cutime : float;         (** User time for the children processes *)
  tms_cstime : float;         (** System time for the children processes *)
}

let times = Unix.times

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
let time = Unix.time
let gettimeofday = Unix.gettimeofday
let gmtime = Unix.gmtime
let localtime = Unix.localtime
let mktime = Unix.mktime

let utimes name ~access ~modif =
  In_thread.syscall_exn (fun () -> Unix.utimes name ~access ~modif)
;;

(* environment *)

let environment = Unix.environment
let getenv = Sys.getenv
let getenv_exn = Sys.getenv_exn
let putenv = Unix.putenv

(* processes *)

let fork_exec ~prog ~args ?use_path ?env () =
  In_thread.run (fun () -> Unix.fork_exec ~prog ~args ?use_path ?env ())
;;

type wait_on = Unix.wait_on with sexp

let make_wait wait_nohang =
  let waits = ref [] in
  let install_sigchld_handler_the_first_time =
    lazy (
      Async_signal.handle [Signal.chld] ~f:(fun _s ->
        waits :=
          List.fold !waits ~init:[] ~f:(fun ac ((wait_on, result) as wait) ->
            match
              Result.try_with (fun () -> wait_nohang wait_on)
            with
            | Ok None -> wait :: ac
            | Ok (Some x) -> Ivar.fill result (Ok x); ac
            | Error exn -> Ivar.fill result (Error exn); ac)))
  in
  fun wait_on ->
    match wait_nohang wait_on with
    | Some result -> return result
    | None ->
      Deferred.create (fun result ->
        Lazy.force install_sigchld_handler_the_first_time;
        waits := (wait_on, result) :: !waits)
      >>| Result.ok_exn
;;

let wait_nohang          = Unix.wait_nohang
let wait_nohang_untraced = Unix.wait_nohang_untraced

let wait          = make_wait wait_nohang
let wait_untraced = make_wait wait_nohang_untraced

module Inet_addr = struct
  include Unix.Inet_addr

  let of_string_or_getbyname =
    let sequencer = Throttle.Sequencer.create ~continue_on_error:true () in
    fun s ->
      Throttle.Sequencer.enqueue sequencer
        (fun () -> In_thread.run (fun () -> of_string_or_getbyname s))
  ;;
end

module Socket = struct
  type unix = [ `Unix of string ] with sexp, bin_io
  type inet = [ `Inet of Inet_addr.t * int ] with sexp, bin_io

  type 'a addr = 'a constraint 'a = [< unix | inet ] with sexp, bin_io

  let address_string_of_inet (`Inet (a,_p)) = Inet_addr.to_string a
  let string_of_inet (`Inet (a,p)) = sprintf "%s:%d" (Unix.Inet_addr.to_string a) p
  let string_of_unix (`Unix s) = s

  module Address = struct
    type t = [ unix | inet ] with sexp, bin_io

    let unix s = `Unix s
    let inet a ~port = `Inet (a, port)
    let inet_addr_any ~port = `Inet (Inet_addr.of_string "0.0.0.0", port)

    let to_unix = function
      | `Unix s -> U.ADDR_UNIX s
      | `Inet (a, i) -> U.ADDR_INET (a, i)
    ;;

    let unix_to_string = function
      | U.ADDR_INET (a, p) -> sprintf "%s:%d" (Unix.Inet_addr.to_string a) p
      | U.ADDR_UNIX s -> s
    ;;

    let to_string addr = unix_to_string (to_unix addr)
  end

  module Family = struct
    type 'a t = {
      family : Unix.socket_domain;
      addr_of_unix : Unix.sockaddr -> 'a;
    } constraint 'a = 'a addr

    let addr_of_unix t u = t.addr_of_unix u

    let family t = t.family

    let to_string t =
      match t.family with
      | U.PF_INET -> "inet"
      | U.PF_INET6 -> "inet6"
      | U.PF_UNIX -> "unix"
    ;;

    let inet =
      { family = U.PF_INET;
        addr_of_unix = function
          | U.ADDR_INET (a, i) -> `Inet (a, i)
          | _ -> failwith "Socket.Family.inet";
      }
    ;;

    let unix =
      { family = U.PF_UNIX;
        addr_of_unix = function
          | U.ADDR_UNIX s -> `Unix s
          | _ -> failwith "Socket.Family.unix";
      }
    ;;
  end

  module Type = struct
    type 'a t = {
      family : 'a Family.t;
      socket_type : Unix.socket_type;
    }

    let tcp =
      { family = Family.inet;
        socket_type = U.SOCK_STREAM;
      }
    ;;

    let udp =
      { family = Family.inet;
        socket_type = U.SOCK_DGRAM;
      }
    ;;

    let unix =
      { family = Family.unix;
        socket_type = U.SOCK_STREAM;
      }
    ;;
  end

  type ('a, 'b) t = {
    typ : 'b Type.t;
    fd : Fd.t;
  } constraint 'a = [< `Unconnected | `Bound | `Passive | `Active ]

  let fd t = t.fd

  let to_string t =
    let st_str =
      Sexp.to_string (Unix.sexp_of_socket_type t.typ.Type.socket_type)
    in
    sprintf "{ typ = { family = %s; socket_type = %s }; fd = %s; }"
      (Family.to_string t.typ.Type.family) st_str (Fd.to_string t.fd)
  ;;

  let of_fd fd typ = { typ; fd }

  let cast t = t

  let create typ =
    let fd =
      Fd.create (Fd.Kind.Socket `Unconnected)
        (Unix.socket ~domain:typ.Type.family.Family.family
            ~kind:typ.Type.socket_type ~protocol:0)
        ~name:"<socket>"
    in
    { typ; fd }
  ;;

  module Opt = struct
    type 'a t = {
      name : string;
      get : File_descr.t -> 'a;
      set : File_descr.t -> 'a -> unit;
    }

    let to_string t = t.name

    let make getsockopt setsockopt name opt = {
      name;
      get = (fun fd -> getsockopt fd opt);
      set = (fun fd a -> setsockopt fd opt a);
    }

    let bool = make Unix.getsockopt Unix.setsockopt
    let int = make Unix.getsockopt_int Unix.setsockopt_int
    let optint = make Unix.getsockopt_optint Unix.setsockopt_optint
    let float = make Unix.getsockopt_float Unix.setsockopt_float

    let debug = bool "debug" U.SO_DEBUG
    let broadcast = bool "broadcast" U.SO_BROADCAST
    let reuseaddr = bool "reuseaddr" U.SO_REUSEADDR
    let keepalive = bool "keepalive" U.SO_KEEPALIVE
    let dontroute = bool "dontroute" U.SO_DONTROUTE
    let oobinline = bool "oobinline" U.SO_OOBINLINE
    let acceptconn = bool "acceptconn" U.SO_ACCEPTCONN

    let nodelay = bool "nodelay" U.TCP_NODELAY

    let sndbuf = int "sndbuf" U.SO_SNDBUF
    let rcvbuf = int "rcvbuf" U.SO_RCVBUF
    let error = int "error" U.SO_ERROR
    let typ = int "typ" U.SO_TYPE
    let rcvlowat = int "rcvlowat" U.SO_RCVLOWAT
    let sndlowat = int "sndlowat" U.SO_SNDLOWAT

    let linger = optint "linger" U.SO_LINGER

    let rcvtimeo = float "rcvtimeo" U.SO_RCVTIMEO
    let sndtimeo = float "sndtimeo" U.SO_SNDTIMEO
  end

  let getopt t opt = Fd.with_file_descr_exn t.fd opt.Opt.get

  let setopt t opt a =
    Fd.with_file_descr_exn t.fd (fun file_descr -> opt.Opt.set file_descr a)
  ;;

  let bind t addr =
    setopt t Opt.reuseaddr true;
    set_close_on_exec t.fd;
    let addr =
      match addr with
      | `Unix s -> U.ADDR_UNIX s
      | `Inet (addr, port) -> U.ADDR_INET (addr, port)
      | `Inet_addr_any port -> U.ADDR_INET (Unix.Inet_addr.bind_any, port)
    in
    Fd.syscall_in_thread_exn t.fd (fun file_descr -> Unix.bind file_descr ~addr)
    >>| fun () ->
    { fd = Fd.replace t.fd (Fd.Kind.Socket `Bound);
      typ = t.typ;
    }
  ;;

  let listen ?(max_pending_connections = 10) t =
    Fd.syscall_exn t.fd (fun file_descr ->
      Unix.listen file_descr ~max:max_pending_connections);
    { fd = Fd.replace t.fd (Fd.Kind.Socket `Passive);
      typ = t.typ;
    }
  ;;

  let turn_off_nagle addr t =
    match addr, t.typ.Type.socket_type with
    | U.ADDR_INET _ , U.SOCK_STREAM -> setopt t Opt.nodelay true
    | (U.ADDR_UNIX _ | U.ADDR_INET _), _ -> ()
  ;;

  let accept_interruptible t ~interrupt =
    Deferred.create (fun i ->
      let rec loop () =
        match
          (* We call [accept] with [~nonblocking:true] because there is no way to use
             [select] to guarantee that an [accept] will not block (see Stevens' book on
             Unix Network Programming, p422). *)
          Fd.with_file_descr t.fd ~nonblocking:true
            (fun file_descr ->
              U.accept file_descr)
        with
        | `Already_closed -> failwiths "already closed" t.fd <:sexp_of< Fd.t >>
        | `Ok (file_descr, addr) ->
          let s =
            { fd = Fd.create (Fd.Kind.Socket `Active) file_descr ~name:"<socket>";
              typ = t.typ;
            }
          in
          set_close_on_exec s.fd;
          turn_off_nagle addr s;
          Ivar.fill i (`Ok (s, Family.addr_of_unix t.typ.Type.family addr))
        | `Error (Unix_error ((EAGAIN | EWOULDBLOCK | ECONNABORTED | EINTR), _, _)) ->
          (* If [accept] would have blocked (EAGAIN|EWOULDBLOCK) or got interrupted
             (EINTR), we want to wait for select to tell us when to try again.

             If the kernel returns ECONNABORTED, this means that we first got a connection
             and therefore woke up in "select" (ready to read).  But due to slowness
             (e.g. other long async jobs getting to run first) we could not call accept
             quickly enough, and the other side terminated the connection in the
             meanwhile.  Though one could imagine weird client/server applications that
             absolutely need to know that some client aborted the connection before we
             could accept it, this seems quite contrived and unlikely.  In virtually all
             cases people just want to continue waiting for a new connection.

             [Sys_blocked_io] cannot be raised here.  This is a Unix-function, not a
             standard OCaml I/O-function (e.g. for reading from channels).  *)
          begin
            Fd.ready_to_interruptible t.fd `Read ~interrupt
            >>> function
              | `Ready -> loop ()
              | `Interrupted as x -> Ivar.fill i x
              | `Closed | `Bad_fd ->
                failwiths "accept on bad file descriptor" t.fd <:sexp_of< Fd.t >>
          end
        | `Error exn -> raise exn
      in
      loop ())
  ;;

  let accept t =
    accept_interruptible t ~interrupt:(Deferred.never ())
    >>| function
      | `Interrupted -> assert false  (* impossible *)
      | `Ok x -> x
  ;;

  let connect_interruptible t addr ~interrupt =
    Deferred.create (fun i ->
      let unix_addr = Address.to_unix addr in
      turn_off_nagle unix_addr t;
      let success () =
        Ivar.fill i (`Ok { fd = Fd.replace t.fd (Fd.Kind.Socket `Active); typ = t.typ })
      in
      let fail_closed () = failwiths "connect on closed fd" t.fd <:sexp_of< Fd.t >> in
      match
        (* We call [connect] with [~nonblocking:true] to initiate an asynchronous connect
           (see Stevens' book on Unix Network Programming, p413).  Once the connect
           succeeds or fails, [select] on the socket will return it in the writeable
           set. *)
        Fd.with_file_descr t.fd ~nonblocking:true
          (fun file_descr ->
            Unix.connect file_descr ~addr:unix_addr)
      with
      | `Already_closed -> fail_closed ()
      | `Ok () -> success ()
      | `Error (Unix_error ((EINPROGRESS | EINTR), _, _)) -> begin
        Fd.ready_to_interruptible t.fd `Write ~interrupt
        >>> function
          | `Closed -> fail_closed ()
          | `Bad_fd -> failwiths "connect on bad file descriptor" t.fd <:sexp_of< Fd.t >>
          | `Interrupted as interrupt -> Ivar.fill i interrupt
          | `Ready ->
              (* We call [getsockopt] to find out whether the connect has succeed or
                 failed. *)
            match
              Fd.with_file_descr t.fd (fun file_descr ->
                Unix.getsockopt_int file_descr U.SO_ERROR)
            with
            | `Already_closed -> fail_closed ()
            | `Error exn -> raise exn
            | `Ok err ->
              if err = 0 then
                success ()
              else
                Unix.unix_error err "connect" (Address.to_string addr)
      end
      | `Error e -> raise e)
  ;;

  let connect t addr =
    connect_interruptible t addr ~interrupt:(Deferred.never ())
    >>| function
      | `Interrupted -> assert false  (* impossible *)
      | `Ok t -> t
  ;;

  let shutdown t mode =
    let mode =
      match mode with
      | `Receive -> U.SHUTDOWN_RECEIVE
      | `Send -> U.SHUTDOWN_SEND
      | `Both -> U.SHUTDOWN_ALL
    in
    Fd.syscall_exn t.fd (fun file_descr -> Unix.shutdown file_descr ~mode)
  ;;

  let getsockname t =
    Family.addr_of_unix t.typ.Type.family (Unix.getsockname (Fd.file_descr_exn t.fd))
  ;;

  let getpeername t =
    Family.addr_of_unix t.typ.Type.family (Unix.getpeername (Fd.file_descr_exn t.fd))
  ;;
end

let socketpair () =
  let (s1, s2) = Unix.socketpair ~domain:U.PF_UNIX ~kind:U.SOCK_STREAM ~protocol:0 in
  let make_fd s = Fd.create (Fd.Kind.Socket `Active) s ~name:"<socketpair>" in
  (make_fd s1, make_fd s2)
;;

module Protocol_family = Unix.Protocol_family

module Host = struct
  type t = Unix.Host.t =
    { name : string;
      aliases : string array;
      family : Protocol_family.t;
      addresses : Inet_addr.t array;
    }

  let getbyname     n = In_thread.syscall_exn (fun () -> Unix.Host.getbyname     n)
  let getbyname_exn n = In_thread.syscall_exn (fun () -> Unix.Host.getbyname_exn n)
  let getbyaddr     a = In_thread.syscall_exn (fun () -> Unix.Host.getbyaddr     a)
  let getbyaddr_exn a = In_thread.syscall_exn (fun () -> Unix.Host.getbyaddr_exn a)
end

let gethostname () = Unix.gethostname ()

let setuid  uid = Unix.setuid  uid

let getuid  ()  = Unix.getuid  ()
let getgid  ()  = Unix.getgid  ()
let getegid ()  = Unix.getegid ()
let geteuid ()  = Unix.geteuid ()

module Terminal_io = struct
  include Unix.Terminal_io

  let tcgetattr fd =
    Fd.syscall_in_thread_exn fd (fun file_descr -> tcgetattr file_descr)
  ;;

  let tcsetattr t fd ~mode =
    Fd.syscall_in_thread_exn fd (fun file_descr -> tcsetattr t file_descr ~mode)
  ;;
end

module Passwd = struct
  type t = Unix.Passwd.t =
    { name : string;
      passwd : string;
      uid : int;
      gid : int;
      gecos : string;
      dir : string;
      shell : string;
    }
  with fields, sexp

  let getbyname n = In_thread.syscall_exn (fun () -> Unix.Passwd.getbyname n)
  let getbyname_exn n = In_thread.syscall_exn (fun () -> Unix.Passwd.getbyname_exn n)
  let getbyuid uid = In_thread.syscall_exn (fun () -> Unix.Passwd.getbyuid uid)
  let getbyuid_exn uid = In_thread.syscall_exn (fun () -> Unix.Passwd.getbyuid_exn uid)
end

module Group = struct
  type t = Unix.Group.t =
    { name : string;
      passwd : string;
      gid : int;
      mem : string array;
    }
  with fields, sexp

  let getbyname n = In_thread.syscall_exn (fun () -> Unix.Group.getbyname n)
  let getbyname_exn n = In_thread.syscall_exn (fun () -> Unix.Group.getbyname_exn n)
  let getbygid gid = In_thread.syscall_exn (fun () -> Unix.Group.getbygid gid)
  let getbygid_exn gid = In_thread.syscall_exn (fun () -> Unix.Group.getbygid_exn gid)
end

let getlogin () = In_thread.syscall_exn (fun () -> Unix.getlogin ())

let wordexp ?flags glob = In_thread.syscall_exn (fun () -> Unix.wordexp ?flags glob)
