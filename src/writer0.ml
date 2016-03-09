open Core.Std
open Import

module Core_unix = Core.Std.Unix
module Unix      = Unix_syscalls

module IOVec = Core.Std.Unix.IOVec

module Id = Unique_id.Int63 ()

let io_stats = Io_stats.create ()

let debug = Debug.writer

module Check_buffer_age' = struct
  type 'a t =
    { writer               : 'a
    ; maximum_age          : Time_ns.Span.t
    (* [Queue.length bytes_received_queue = Queue.length times_received_queue] *)
    ; bytes_received_queue : Int63.t   Queue.t
    ; times_received_queue : Time_ns.t Queue.t
    (* The buffer-age check is responsible for filling in [too_old] if it detects an age
       violation. *)
    ; mutable too_old      : unit Ivar.t
    }
  [@@deriving sexp_of]
end

module Open_flags = Unix.Open_flags

type open_flags = (Open_flags.t, exn) Result.t [@@deriving sexp_of]

module Scheduled = struct
  type t = (Bigstring.t IOVec.t * [ `Destroy | `Keep ]) Deque.t

  let length (t : t) = Deque.fold t ~init:0 ~f:(fun n (iovec, _) -> n + iovec.len)
end

type t =
  { id                                 : Id.t
  ; mutable fd                         : Fd.t

  (* The writer uses a background job to flush data.  The job runs within [inner_monitor],
     which has a handler that wraps all errors to include [sexp_of_t t], and sends them to
     [monitor]. *)
  ; monitor       : Monitor.t
  ; inner_monitor : Monitor.t
  ; mutable background_writer_state    : [ `Running | `Not_running | `Stopped_permanently ]

  (* [syscall] determines the batching approach that the writer uses to batch data
     together and flush it using the underlying write syscall. *)
  ; syscall                            : [ `Per_cycle | `Periodic of Time.Span.t ]

  (* Counts since the writer was created. *)
  ; mutable bytes_received             : Int63.t
  ; mutable bytes_written              : Int63.t

  (* Bytes that we have received but not yet written are stored in two places: [scheduled]
     and [buf].  The bytes that we need to write are the concatenation of the sequence
     of iovecs in [scheduled] followed by the bytes in [buf] from [scheduled_back] to
     [back].  Note that iovecs in [scheduled] can point to regions in [buf], even the
     current [buf] in the writer. *)

  (* [scheduled] holds iovecs that we plan to write. *)
  ; scheduled                          : Scheduled.t sexp_opaque

  (* [scheduled_bytes] is the sum of the lengths of the iovecs in[scheduled] *)
  ; mutable scheduled_bytes            : int

  (* [buf] has three regions                            :
     [0, scheduled_back)             received and scheduled
     [scheduled_back, back)          received but not scheduled
     [back, Bigstring.length buf)    free space*)
  ; mutable buf                        : Bigstring.t sexp_opaque
  ; mutable scheduled_back             : int
  ; mutable back                       : int

  ; flushes                            : (Time.t Ivar.t * Int63.t) Queue.t sexp_opaque

  (* [closed_state] tracks the state of the writer as it is being closed.  Initially,
     [closed_state] is [`Open].  When [close] is called, [closed_state] transitions to
     [`Closed_and_flushing].  Once the writer is flushed and we're actually going to
     close [fd], it transitions to[`Closed].

     The distinction between [`Closed] and [`Closed_and_flushing] is necessary because
     we want to allow [write]s to happen while [`Closed_and_flushing], but not when
     [`Closed].  This is necessary to allow upstream producers to flush their data
     to the writer when it is closed. *)
  ; mutable close_state                : [ `Open | `Closed_and_flushing | `Closed ]

  (* [close_finished] is filled when the close() system call on [fd] finishes. *)
  ; close_finished                     : unit Ivar.t

  (* [close_started] is filled when [close] is called. *)
  ; close_started                      : unit Ivar.t

  (* [producers_to_flush_at_close] holds all upstream producers feeding data to this
     writer, and thus should be flushed when we close this writer, before flushing
     the writer itself. *)
  ; producers_to_flush_at_close        : (unit -> unit Deferred.t) Bag.t

  (* [flush_at_shutdown_elt] holds the element in [writers_to_flush_at_shutdown] for
     this writer.  Being in that bag is what causes this writer to be automatically
     closed when [shutdown] is called, and for shutdown to wait for the close to finish.
     [flush_at_shutdown_elt] is [Some] for the lifetime of the writer, until the
     close finishes, at which point it transitions to[None]. *)
  ; mutable flush_at_shutdown_elt      : t sexp_opaque Bag.Elt.t option

  ; mutable check_buffer_age           : t sexp_opaque Check_buffer_age'.t Bag.Elt.t option

  (* The "consumer" of a writer is whomever is reading the bytes that the writer
     is writing.  E.g. if the writer's file descriptor is a socket, then it is whomever
     is on the other side of the socket connection.  If the consumer leaves, Unix will
     indicate this by returning EPIPE or ECONNRESET to a write() syscall.  We keep
     track of this with the [consumer_left] ivar, which is exposed in writer.mli.
     We also allow the user to configure what action the writer takes when the
     consumer leaves.  By default, it raises, but that can be disabled. *)
  ; consumer_left                      : unit Ivar.t
  ; mutable raise_when_consumer_leaves : bool (** default is [true] *)
  (* [open_flags] is the open-file-descriptor bits of [fd].  It is created when [t] is
     created, and starts a deferred computation that calls [Unix.fcntl_getfl].
     [open_flags] is used to report an error when [fd] is not writable.  [Fd] treats the
     call to [fcntl_getfl] as an active system call, which prevents [Unix.close fd] from
     completing until [fcntl_getfl] finishes.  This prevents a file-descriptor or thread
     leak even though client code doesn't explicitly wait on [open_flags]. *)
  ; open_flags                         : open_flags Deferred.t
  }
[@@deriving fields, sexp_of]

type writer = t [@@deriving sexp_of]

let set_raise_when_consumer_leaves t bool = t.raise_when_consumer_leaves <- bool

let bytes_to_write t = t.scheduled_bytes + t.back - t.scheduled_back

let is_stopped_permanently t =
  match t.background_writer_state with
  | `Stopped_permanently -> true
  | `Running | `Not_running -> false
;;

let invariant t : unit =
  try
    let check f = fun field -> f (Field.get field t) in
    Fields.iter
      ~id:ignore
      ~fd:ignore
      ~monitor:ignore
      ~inner_monitor:ignore
      ~buf:ignore
      ~background_writer_state:(check (function
        | `Stopped_permanently ->
          assert (bytes_to_write t = 0)
        | `Running | `Not_running ->
          assert (Bigstring.length t.buf > 0);
          assert (Int63.(t.bytes_received - t.bytes_written = of_int (bytes_to_write t)))))
      ~syscall:ignore
      ~bytes_written:(check (fun bytes_written ->
        assert (Int63.(zero <= bytes_written && bytes_written <= t.bytes_received))))
      ~bytes_received:ignore
      ~scheduled:(check (fun (scheduled : Scheduled.t) ->
        Deque.iter scheduled ~f:(fun (iovec, kind) ->
          if phys_equal t.buf iovec.buf then assert (kind = `Keep))))
      ~scheduled_bytes:(check (fun scheduled_bytes ->
        assert (scheduled_bytes = Scheduled.length t.scheduled)))
      ~scheduled_back:(check (fun scheduled_back ->
        assert (0 <= scheduled_back && scheduled_back <= t.back)))
      ~back:(check (fun back -> assert (back <= Bigstring.length t.buf)))
      ~flushes:ignore
      ~close_state:ignore
      ~close_finished:(check (fun close_finished ->
        match t.close_state with
        | `Open | `Closed_and_flushing -> assert (Ivar.is_empty close_finished)
        | `Closed -> ()))
      ~close_started:(check (fun close_started ->
        [%test_result: bool] (Ivar.is_empty close_started)
          ~expect:
            (match t.close_state with
             | `Open -> true
             | `Closed | `Closed_and_flushing -> false)))
      ~producers_to_flush_at_close:ignore
      ~flush_at_shutdown_elt:(check (fun o ->
        assert (is_none o = Ivar.is_full t.close_finished);
        Option.iter o ~f:(fun elt -> assert (phys_equal t (Bag.Elt.value elt)))))
      ~check_buffer_age:ignore
      ~consumer_left:(check (fun consumer_left ->
        if Ivar.is_full consumer_left then assert (is_stopped_permanently t)))
      ~raise_when_consumer_leaves:ignore
      ~open_flags:ignore
  with exn -> failwiths "writer invariant failed" (exn, t) [%sexp_of: exn * t]
;;

module Check_buffer_age : sig
  type t = writer Check_buffer_age'.t Bag.Elt.t option

  val dummy : t
  val create
    :  writer
    -> maximum_age:[ `At_most of Time.Span.t | `Unlimited ]
    -> t
  val destroy : t -> unit
  val too_old : t -> unit Deferred.t
end = struct
  open Check_buffer_age'

  type t = writer Check_buffer_age'.t Bag.Elt.t option

  let dummy = None

  let active_checks = Bag.create ()

  let () =
    let backed_up_writers = ref [] in
    let now = ref Time_ns.epoch in
    let send_backed_up_writer_error e =
      Monitor.send_exn e.writer.monitor
        (Error.to_exn
           (Error.create "writer buffer has data older than" (e.maximum_age, e.writer)
              [%sexp_of: Time_ns.Span.t * writer]));
      Ivar.fill_if_empty e.too_old ();
    in
    let process_active_check e =
      let continue = ref true in
      while !continue do
        if Queue.is_empty e.bytes_received_queue
        then continue := false
        else
          let bytes_received = Queue.peek_exn e.bytes_received_queue in
          let time_received  = Queue.peek_exn e.times_received_queue in
          if Int63.( <= ) bytes_received e.writer.bytes_written
          then (
            ignore (Queue.dequeue_exn e.bytes_received_queue : Int63.t);
            ignore (Queue.dequeue_exn e.times_received_queue : Time_ns.t))
          else (
            continue := false;
            if Time_ns.Span.( > ) (Time_ns.diff !now time_received) e.maximum_age
            then backed_up_writers := e :: !backed_up_writers
            else if Ivar.is_full e.too_old then e.too_old <- Ivar.create ())
      done;
      if e.writer.bytes_received > e.writer.bytes_written
      then (
        Queue.enqueue e.bytes_received_queue e.writer.bytes_received;
        Queue.enqueue e.times_received_queue !now);
    in
    every Time_ns.Span.second ~continue_on_error:false (fun () ->
      assert (List.is_empty !backed_up_writers);
      if not (Bag.is_empty active_checks)
      then (
        now := Kernel_scheduler.(cycle_start (t ()));
        Bag.iter active_checks ~f:process_active_check;
        if not (List.is_empty !backed_up_writers)
        then (
          List.iter !backed_up_writers ~f:send_backed_up_writer_error;
          backed_up_writers := [])));
  ;;

  let create writer ~maximum_age =
    match maximum_age with
    | `Unlimited -> None
    | `At_most maximum_age ->
      Some (
        Bag.add active_checks
          { writer
          ; bytes_received_queue = Queue.create ()
          ; times_received_queue = Queue.create ()
          ; maximum_age          = Time_ns.Span.of_span maximum_age
          ; too_old              = Ivar.create ()
          })
  ;;

  let destroy t =
    match t with
    | None -> ()
    | Some elt -> Bag.remove active_checks elt
  ;;

  let too_old t =
    match t with
    | None -> Deferred.never ()
    | Some elt -> Ivar.read (Bag.Elt.value elt).too_old
  ;;
end

let flushed_time t =
  if t.bytes_written = t.bytes_received
  then return (Time.now ())
  else if Ivar.is_full t.close_finished
  then Deferred.never ()
  else
    Deferred.create (fun ivar ->
      Queue.enqueue t.flushes (ivar, t.bytes_received))
;;

let flushed t =
  if t.bytes_written = t.bytes_received
  then Deferred.unit
  else if Ivar.is_full t.close_finished
  then Deferred.never ()
  else Deferred.ignore (flushed_time t)
;;

let set_fd t fd = flushed t >>| fun () -> t.fd <- fd

let consumer_left t = Ivar.read t.consumer_left

let close_finished t = Ivar.read t.close_finished

let close_started t = Ivar.read t.close_started

let is_closed t =
  match t.close_state with
  | `Open -> false
  | `Closed | `Closed_and_flushing -> true
;;

let is_open t = not (is_closed t)

let writers_to_flush_at_shutdown : t Bag.t = Bag.create ()

let final_flush ?force t =
  let producers_flushed =
    (* Note that each element of [producers_to_flush_at_close] checks that the upstream
       producer is flushed, which includes checking that [t] itself is flushed once the
       producer has written everything to [t].  So, there is no need to call [flushed t]
       after the producer is flushed. *)
    Deferred.List.iter ~how:`Parallel ~f:(fun f -> f ())
      (Bag.to_list t.producers_to_flush_at_close);
  in
  let force =
    match force with
    | Some fc -> fc
    | None ->
      (* We used to use [after (sec 5.)] as the default value for [force] for all kinds
         of underlying fds.  This was problematic, because it silently caused data in
         the writer's buffer to be dropped when it kicked in.  We care about data
         getting out only for the files, when we want to get data to disk.  When we
         close socket writers, we usually just want to drop the connection, so using
         [after (sec 5.)]  makes sense. *)
      match Fd.kind t.fd with
      | File -> Deferred.never ()
      | Char | Fifo | Socket _ -> Clock.after (sec 5.)
  in
  Deferred.any_unit
    [ (* If the consumer leaves, there's no more writing we can do. *)
      consumer_left t;
      Deferred.all_unit [ producers_flushed; flushed t ];
      force;
      (* The buffer-age check might fire while we're waiting. *)
      Check_buffer_age.too_old t.check_buffer_age;
    ]
;;

let close ?force_close t =
  if debug then Debug.log "Writer.close" t [%sexp_of: t];
  begin match t.close_state with
  | `Closed_and_flushing | `Closed -> ()
  | `Open ->
    t.close_state <- `Closed_and_flushing;
    Ivar.fill t.close_started ();
    final_flush t ?force:force_close
    >>> fun () ->
    t.close_state <- `Closed;
    Check_buffer_age.destroy t.check_buffer_age;
    begin match t.flush_at_shutdown_elt with
    | None -> assert false
    | Some elt -> Bag.remove writers_to_flush_at_shutdown elt
    end;
    Unix.close t.fd
    >>> fun () ->
    Ivar.fill t.close_finished ();
  end;
  close_finished t
;;

let () =
  Shutdown.at_shutdown (fun () ->
    if debug then Debug.log_string "Writer.at_shutdown";
    Deferred.List.iter ~how:`Parallel (Bag.to_list writers_to_flush_at_shutdown)
      ~f:(fun t ->
        Deferred.any_unit [ final_flush t;
                            close_finished t;
                          ]))
;;

let fill_flushes { bytes_written; flushes; _ } =
  if not (Queue.is_empty flushes) then begin
    let now = Time.now () in
    let rec loop () =
      match Queue.peek flushes with
      | None -> ()
      | Some (ivar, z) ->
        if Int63.(z <= bytes_written) then begin
          Ivar.fill ivar now;
          ignore (Queue.dequeue flushes);
          loop ()
        end
    in
    loop ()
  end
;;

let stop_permanently t =
  t.background_writer_state <- `Stopped_permanently;
  Deque.clear t.scheduled;
  t.scheduled_bytes <- 0;
  t.buf <- Bigstring.create 0;
  t.scheduled_back <- 0;
  t.back <- 0;
  Queue.clear t.flushes;
;;

let die t error = stop_permanently t; Error.raise error

type buffer_age_limit = [ `At_most of Time.Span.t | `Unlimited ] [@@deriving bin_io, sexp]

let create
      ?buf_len
      ?(syscall = `Per_cycle)
      ?buffer_age_limit
      ?(raise_when_consumer_leaves = true)
      fd =
  let buffer_age_limit =
    match buffer_age_limit with
    | Some z -> z
    | None ->
      match Fd.kind fd with
      | File -> `Unlimited
      | Char | Fifo | Socket _ -> `At_most (Time.Span.of_min 2.)
  in
  let buf_len =
    match buf_len with
    | None -> 65 * 1024 * 2 (* largest observed single write call * 2 *)
    | Some buf_len ->
      if buf_len <= 0
      then invalid_arg "Writer.create: buf_len <= 0"
      else buf_len
  in
  let id = Id.create () in
  let monitor       = Monitor.create () in
  let inner_monitor = Monitor.create () in
  let consumer_left = Ivar.create () in
  let open_flags = try_with (fun () -> Unix.fcntl_getfl fd) in
  let t =
    { id
    ; fd
    ; syscall
    ; monitor
    ; inner_monitor
    ; buf                         = Bigstring.create buf_len
    ; back                        = 0
    ; scheduled_back              = 0
    ; scheduled                   = Deque.create ()
    ; scheduled_bytes             = 0
    ; bytes_received              = Int63.zero
    ; bytes_written               = Int63.zero
    ; flushes                     = Queue.create ()
    ; background_writer_state     = `Not_running
    ; close_state                 = `Open
    ; close_finished              = Ivar.create ()
    ; close_started               = Ivar.create ()
    ; producers_to_flush_at_close = Bag.create ()
    ; flush_at_shutdown_elt       = None
    ; check_buffer_age            = Check_buffer_age.dummy
    ; consumer_left
    ; raise_when_consumer_leaves
    ; open_flags
    }
  in
  Monitor.detach_and_iter_errors inner_monitor ~f:(fun exn ->
    Monitor.send_exn monitor
      (Error.to_exn (Error.create "writer error" (exn, t) [%sexp_of: exn * t])));
  t.check_buffer_age <- Check_buffer_age.create t ~maximum_age:buffer_age_limit;
  t.flush_at_shutdown_elt <- Some (Bag.add writers_to_flush_at_shutdown t);
  t
;;

let set_buffer_age_limit t maximum_age =
  Check_buffer_age.destroy t.check_buffer_age;
  t.check_buffer_age <- Check_buffer_age.create t ~maximum_age;
;;

let of_out_channel oc kind = create (Fd.of_out_channel oc kind)

let ensure_can_write t =
  match t.close_state with
  | `Open | `Closed_and_flushing -> ()
  | `Closed ->
    failwiths "attempt to use closed writer" t [%sexp_of: t]
;;

let open_file ?(append = false) ?(close_on_exec = true) ?(perm = 0o666) file =
  (* Writing to NFS needs the [`Trunc] flag to avoid leaving extra junk at the end of
     a file. *)
  let mode = [ `Wronly; `Creat ] in
  let mode = (if append then `Append else `Trunc) :: mode in
  Unix.openfile file ~mode ~perm ~close_on_exec >>| create
;;

let with_close t ~f = Monitor.protect f ~finally:(fun () -> close t)

let with_writer_exclusive t f =
  Unix.lockf t.fd `Write
  >>= fun () ->
  Monitor.protect f ~finally:(fun () -> flushed t >>| fun () -> Unix.unlockf t.fd)
;;

let with_file ?perm ?append ?(exclusive = false) file ~f =
  open_file ?perm ?append file
  >>= fun t ->
  with_close t ~f:(fun () ->
    if exclusive
    then with_writer_exclusive t (fun () -> f t)
    else f t)
;;

let got_bytes t n = t.bytes_received <- Int63.(t.bytes_received + of_int n)

let add_iovec t kind (iovec : _ IOVec.t) ~count_bytes_as_received =
  assert (t.scheduled_back = t.back);
  if count_bytes_as_received then got_bytes t iovec.len;
  if not (is_stopped_permanently t) then begin
    t.scheduled_bytes <- t.scheduled_bytes + iovec.len;
    Deque.enqueue_back t.scheduled (iovec, kind);
  end;
  assert (t.scheduled_back = t.back);
;;

let schedule_unscheduled t kind =
  let need_to_schedule = t.back - t.scheduled_back in
  assert (need_to_schedule >= 0);
  if need_to_schedule > 0 then begin
    let pos = t.scheduled_back in
    t.scheduled_back <- t.back;
    add_iovec t kind (IOVec.of_bigstring t.buf ~pos ~len:need_to_schedule)
      ~count_bytes_as_received:false; (* they were already counted *)
  end;
;;

let dummy_iovec = IOVec.empty IOVec.bigstring_kind

let mk_iovecs t =
  schedule_unscheduled t `Keep;
  let n_iovecs = Int.min (Deque.length t.scheduled) IOVec.max_iovecs in
  let iovecs = Array.create ~len:n_iovecs dummy_iovec in
  let contains_mmapped_ref = ref false in
  let iovecs_len = ref 0 in
  with_return (fun r ->
    let i = ref 0 in
    Deque.iter t.scheduled ~f:(fun (iovec, _) ->
      if !i >= n_iovecs then r.return ();
      if not !contains_mmapped_ref
      && Bigstring.is_mmapped iovec.buf
      then contains_mmapped_ref := true;
      iovecs_len := !iovecs_len + iovec.len;
      iovecs.(!i) <- iovec;
      incr i;));
  iovecs, !contains_mmapped_ref, !iovecs_len
;;

(* Size of I/O- or blit operation for which a helper thread should be used.  This number
   (a power of two) is somewhat empirically motivated, but there is no reason why it
   should be the best. *)
let thread_io_cutoff = 262_144

(* If whe writer was closed, we should be quiet.  But if it wasn't, then someone was
   monkeying around with the fd behind our back, and we should complain. *)
let fd_closed t =
  if not (is_closed t) then failwiths "writer fd unexpectedly closed " t [%sexp_of: t]
;;

let rec start_write t =
  if debug then Debug.log "Writer.start_write" t [%sexp_of: t];
  assert (t.background_writer_state = `Running);
  let iovecs, contains_mmapped, iovecs_len = mk_iovecs t in
  let handle_write_result = function
    | `Already_closed -> fd_closed t
    | `Ok n ->
      if n >= 0
      then write_finished t n
      else die t (Error.create "write system call returned negative result" (t, n)
                    [%sexp_of: t * int])
    | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
      write_when_ready t
    | `Error (Unix.Unix_error (EBADF, _, _)) ->
      die t (Error.create "write got EBADF" t [%sexp_of: t])
    | `Error ((Unix.Unix_error
                 (( EPIPE
                  | ECONNRESET
                  | ENETDOWN
                  | ENETRESET
                  | ENETUNREACH
                  | ETIMEDOUT
                  ),_, _)) as exn) ->
      (* [t.consumer_left] is empty since once we reach this point, we stop the writer
         permanently, and so will never reach here again. *)
      assert (Ivar.is_empty t.consumer_left);
      Ivar.fill t.consumer_left ();
      stop_permanently t;
      if t.raise_when_consumer_leaves then raise exn;
    | `Error exn -> die t (Error.of_exn exn)
  in
  let should_write_in_thread =
    not (Fd.supports_nonblock t.fd)
    || begin
      (* Though the write will not block in this case, a memory-mapped bigstring in an
         I/O-vector may cause a page fault, which would cause the async scheduler thread
         to block.  So, we write in a separate thread, and the [Bigstring.writev] releases
         the OCaml lock, allowing the async scheduler thread to continue. *)
      iovecs_len > thread_io_cutoff || contains_mmapped
    end
  in
  if should_write_in_thread then begin
    Fd.syscall_in_thread t.fd ~name:"writev"
      (fun file_descr -> Bigstring.writev file_descr iovecs)
    >>> handle_write_result
  end else
    handle_write_result
      (Fd.syscall t.fd ~nonblocking:true
         (fun file_descr ->
            Bigstring.writev_assume_fd_is_nonblocking file_descr iovecs))

and write_when_ready t =
  if debug then Debug.log "Writer.write_when_ready" t [%sexp_of: t];
  assert (t.background_writer_state = `Running);
  Fd.ready_to t.fd `Write
  >>> function
  | `Bad_fd -> die t (Error.create "writer ready_to got Bad_fd" t [%sexp_of: t])
  | `Closed -> fd_closed t
  | `Ready -> start_write t

and write_finished t bytes_written =
  if debug
  then Debug.log "Writer.write_finished" (bytes_written, t) [%sexp_of: int * t];
  assert (t.background_writer_state = `Running);
  let int63_bytes_written = Int63.of_int bytes_written in
  Io_stats.update io_stats ~kind:(Fd.kind t.fd) ~bytes:int63_bytes_written;
  t.bytes_written <- Int63.(int63_bytes_written + t.bytes_written);
  if Int63.(t.bytes_written > t.bytes_received)
  then die t (Error.create "writer wrote more bytes than it received" t [%sexp_of: t]);
  fill_flushes t;
  t.scheduled_bytes <- t.scheduled_bytes - bytes_written;
  (* Remove processed iovecs from t.scheduled. *)
  let rec remove_done bytes_written =
    assert (bytes_written >= 0);
    match Deque.dequeue_front t.scheduled with
    | None ->
      if bytes_written > 0
      then die t (Error.create "writer wrote nonzero amount but IO_queue is empty" t
                    [%sexp_of: t])
    | Some ({ buf; pos; len }, kind) ->
      if bytes_written >= len then begin
        (* Current I/O-vector completely written.  Internally generated buffers get
           destroyed immediately unless they are still in use for buffering.  *)
        begin
          match kind with
          | `Destroy -> Bigstring.unsafe_destroy buf
          | `Keep -> ()
        end;
        remove_done (bytes_written - len)
      end else begin
        (* Partial I/O: update partially written I/O-vector and retry I/O. *)
        let new_iovec =
          IOVec.of_bigstring
            buf ~pos:(pos + bytes_written) ~len:(len - bytes_written)
        in
        Deque.enqueue_front t.scheduled (new_iovec, kind);
      end
  in
  remove_done bytes_written;
  (* See if there's anything else to do. *)
  schedule_unscheduled t `Keep;
  if Deque.is_empty t.scheduled then begin
    t.back <- 0;
    t.scheduled_back <- 0;
    t.background_writer_state <- `Not_running;
  end else begin
    match t.syscall with
    | `Per_cycle -> write_when_ready t
    | `Periodic span ->
      Clock.after span
      >>> fun _ ->
      start_write t
  end
;;

let maybe_start_writer t =
  match t.background_writer_state with
  | `Stopped_permanently | `Running -> ()
  | `Not_running ->
    t.background_writer_state <- `Running;
    (* We schedule the background writer thread to run with low priority, so that it runs
       at the end of the cycle and that all of the calls to Writer.write will usually be
       batched into a single system call. *)
    schedule ~monitor:t.inner_monitor ~priority:Priority.low (fun () ->
      t.open_flags
      >>> fun open_flags ->
      let can_write_fd =
        match open_flags with
        | Error _ -> false
        | Ok flags -> Unix.Open_flags.can_write flags
      in
      if not can_write_fd
      then failwiths "not allowed to write due to file-descriptor flags" (open_flags, t)
             [%sexp_of: open_flags * t];
      start_write t);
;;

let give_buf t desired =
  assert (desired > 0);
  assert (not (is_stopped_permanently t));
  got_bytes t desired;
  let buf_len = Bigstring.length t.buf in
  let available = buf_len - t.back in
  if desired <= available then begin
    (* Data fits into buffer *)
    let pos = t.back in
    t.back <- t.back + desired;
    (t.buf, pos)
  end else begin
    (* Preallocated buffer too small; schedule buffered writes.  We create a new buffer of
       exactly the desired size if the desired size is more than half the buffer length.
       If we only created a new buffer when the desired size was greater than the buffer
       length, then multiple consecutive writes of slightly more than half the buffer
       length would each waste slightly less than half of the buffer.  Although, it is
       still the case that multiple consecutive writes of slightly more than one quarter
       of the buffer length will waste slightly less than one quarter of the buffer. *)
    if desired > buf_len / 2 then begin
      schedule_unscheduled t `Keep;
      (* Preallocation size too small; allocate dedicated buffer *)
      let buf = Bigstring.create desired in
      add_iovec t `Destroy (IOVec.of_bigstring ~len:desired buf)
        ~count_bytes_as_received:false; (* we already counted them above *)
      (buf, 0)
    end else begin
      schedule_unscheduled t `Destroy;
      (* Preallocation size sufficient; preallocate new buffer *)
      let buf = Bigstring.create buf_len in
      t.buf <- buf;
      t.scheduled_back <- 0;
      t.back <- desired;
      (buf, 0)
    end
  end
;;

let write_gen (type a)
      ~(length : a -> int)
      ~(blit_to_bigstring : (a, Bigstring.t) Blit.blit)
      ?pos ?len t src =
  let src_pos, src_len =
    Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(length src)
  in
  if is_stopped_permanently t
  then got_bytes t src_len
  else begin
    let available = Bigstring.length t.buf - t.back in
    if available >= src_len then begin
      got_bytes t src_len;
      blit_to_bigstring ~src ~src_pos ~len:src_len ~dst:t.buf ~dst_pos:t.back;
      t.back <- t.back + src_len;
    end else begin
      got_bytes t available;
      blit_to_bigstring ~src ~src_pos ~len:available ~dst:t.buf ~dst_pos:t.back;
      t.back <- t.back + available;
      let remaining = src_len - available in
      let dst, dst_pos = give_buf t remaining in
      blit_to_bigstring
        ~src ~src_pos:(src_pos + available) ~len:remaining ~dst ~dst_pos;
    end;
    maybe_start_writer t;
  end
;;

let write ?pos ?len t src =
  write_gen
    ~length:String.length
    ~blit_to_bigstring:Bigstring.From_string.blit
    ?pos ?len t src
;;

let write_bigstring ?pos ?len t src =
  write_gen
    ~length:Bigstring.length
    ~blit_to_bigstring:Bigstring.blit
    ?pos ?len t src
;;

let write_iobuf ?pos ?len t iobuf =
  let iobuf = Iobuf.read_only (Iobuf.no_seek iobuf) in
  write_gen
    ~length:Iobuf.length
    ~blit_to_bigstring:Iobuf.Peek.To_bigstring.blit
    ?pos ?len t iobuf;
;;

let write_substring t substring =
  write t (Substring.base substring)
    ~pos:(Substring.pos substring)
    ~len:(Substring.length substring)
;;

let write_bigsubstring t bigsubstring =
  write_bigstring t (Bigsubstring.base bigsubstring)
    ~pos:(Bigsubstring.pos bigsubstring)
    ~len:(Bigsubstring.length bigsubstring)
;;

let writef t = ksprintf (fun s -> write t s)

let to_formatter t =
  Format.make_formatter
    (fun str pos len ->
       ensure_can_write t;
       write_substring t (Substring.create str ~pos ~len))
    ignore
;;

let write_char t c =
  if is_stopped_permanently t
  then got_bytes t 1
  else begin
    (* Check for the common case that the char can simply be put in the buffer. *)
    if Bigstring.length t.buf - t.back >= 1 then begin
      got_bytes t 1;
      t.buf.{ t.back } <- c;
      t.back <- t.back + 1;
    end else begin
      let dst, dst_pos = give_buf t 1 in
      dst.{ dst_pos } <- c;
    end;
    maybe_start_writer t
  end
;;

let newline t = write_char t '\n'

let write_line t s =
  write t s;
  newline t;
;;

let write_byte t i = write_char t (char_of_int (i % 256))

module Terminate_with = struct
  type t = Newline | Space_if_needed [@@deriving sexp_of]
end

let write_sexp_internal =
  let initial_size = 1024 * 1024 in
  let buffer = Buffer.create initial_size in
  let blit_str = ref (String.create initial_size) in
  fun ~(terminate_with : Terminate_with.t) ?(hum = false) t sexp ->
    Buffer.clear buffer;
    if hum
    then Sexp.to_buffer_hum ~buf:buffer ~indent:!Sexp.default_indent sexp
    else Sexp.to_buffer ~buf:buffer sexp;
    let len = Buffer.length buffer in
    let blit_str_len = String.length !blit_str in
    if len > blit_str_len then blit_str := String.create (max len (2 * blit_str_len));
    Buffer.blit buffer 0 !blit_str 0 len;
    write t !blit_str ~len;
    match terminate_with with
    | Newline -> newline t
    | Space_if_needed ->
      (* If the string representation doesn't start/end with paren or double quote, we add
         a space after it to ensure that the parser can recognize the end of the sexp. *)
      let c = !blit_str.[0] in
      if not (c = '(' || c = '"')
      then write_char t ' '
;;

let write_sexp ?hum ?(terminate_with = Terminate_with.Space_if_needed) t sexp =
  write_sexp_internal t sexp ?hum ~terminate_with
;;

let write_bin_prot t (writer : _ Bin_prot.Type_class.writer) v =
  let len = writer.size v in
  assert (len > 0);
  let tot_len = len + Bin_prot.Utils.size_header_length in
  if is_stopped_permanently t
  then got_bytes t tot_len
  else begin
    let buf, start_pos = give_buf t tot_len in
    ignore (Bigstring.write_bin_prot buf ~pos:start_pos writer v : int);
    maybe_start_writer t;
  end
;;

let write_bin_prot_no_size_header t ~size write v =
  if is_stopped_permanently t
  then got_bytes t size
  else begin
    let buf, start_pos = give_buf t size in
    let end_pos = write buf ~pos:start_pos v in
    let written = end_pos - start_pos in
    if written <> size
    then failwiths "Writer.write_bin_prot_no_size_header bug!"
           (`written written, `size size)
           [%sexp_of: [ `written of int ] * [ `size of int ]];
    maybe_start_writer t;
  end
;;

let write_marshal t ~flags v =
  schedule_unscheduled t `Keep;
  let iovec = IOVec.of_bigstring (Bigstring_marshal.marshal ~flags v) in
  add_iovec t `Destroy iovec ~count_bytes_as_received:true;
  maybe_start_writer t
;;

let send t s =
  write t (string_of_int (String.length s) ^ "\n");
  write t s;
;;

let schedule_iovec t iovec =
  schedule_unscheduled t `Keep;
  add_iovec t `Keep iovec ~count_bytes_as_received:true;
  maybe_start_writer t;
;;

let schedule_iovecs t iovecs =
  schedule_unscheduled t `Keep;
  Queue.iter iovecs ~f:(add_iovec t `Keep ~count_bytes_as_received:true);
  Queue.clear iovecs;
  maybe_start_writer t;
;;

let schedule_bigstring t ?pos ?len bstr =
  schedule_iovec t (IOVec.of_bigstring ?pos ?len bstr)
;;

let schedule_bigsubstring t bigsubstring =
  schedule_bigstring t
    (Bigsubstring.base bigsubstring)
    ~pos:(Bigsubstring.pos bigsubstring)
    ~len:(Bigsubstring.length bigsubstring)
;;

let schedule_iobuf_peek t ?pos ?len iobuf =
  schedule_iovec t (Iobuf.Expert.to_iovec_shared ?pos ?len iobuf)
;;

let schedule_iobuf_consume t ?len iobuf =
  let iovec = Iobuf.Expert.to_iovec_shared ?len iobuf in
  let len = iovec.len in
  schedule_iovec t iovec;
  flushed_time t
  >>| fun _ ->
  Iobuf.advance iobuf len
;;

(* The code below ensures that no calls happen on a closed writer. *)

let fsync t = ensure_can_write t; flushed t >>= fun _ -> Unix.fsync t.fd
let fdatasync t =
  ensure_can_write t; flushed t >>= fun _ -> Unix.fdatasync t.fd
;;
let write_bin_prot t sw_arg v       = ensure_can_write t; write_bin_prot t sw_arg v
let send t s                        = ensure_can_write t; send t s
let schedule_iovec t iovec          = ensure_can_write t; schedule_iovec t iovec
let schedule_iovecs t iovecs        = ensure_can_write t; schedule_iovecs t iovecs
let schedule_bigstring t ?pos ?len bstr =
  ensure_can_write t; schedule_bigstring t ?pos ?len bstr
let schedule_bigsubstring t bigsubstring =
  ensure_can_write t; schedule_bigsubstring t bigsubstring
let schedule_iobuf_peek t ?pos ?len iobuf =
  ensure_can_write t; schedule_iobuf_peek t ?pos ?len iobuf
let schedule_iobuf_consume t ?len iobuf =
  ensure_can_write t; schedule_iobuf_consume t ?len iobuf
let write_gen ~length ~blit_to_bigstring ?pos ?len t src =
  ensure_can_write t; write_gen ~length ~blit_to_bigstring ?pos ?len t src
let write ?pos ?len t s             = ensure_can_write t; write ?pos ?len t s
let write_line t s                  = ensure_can_write t; write_line t s
let writef t                        = ensure_can_write t; writef t
let write_marshal t ~flags v        = ensure_can_write t; write_marshal t ~flags v
let write_sexp ?hum ?terminate_with t s =
  ensure_can_write t; write_sexp ?hum ?terminate_with t s
let write_iobuf ?pos ?len t iobuf   = ensure_can_write t; write_iobuf ?pos ?len t iobuf
let write_bigstring ?pos ?len t src = ensure_can_write t; write_bigstring ?pos ?len t src
let write_bigsubstring t s          = ensure_can_write t; write_bigsubstring t s
let write_substring t s             = ensure_can_write t; write_substring t s
let write_byte t b                  = ensure_can_write t; write_byte t b
let write_char t c                  = ensure_can_write t; write_char t c
let newline t                       = ensure_can_write t; newline t

let stdout_and_stderr =
  lazy (
    (* The following code checks to see if stdout and stderr point to the same file, and
       if so, shares a single writer between them.  See the comment in writer.mli for
       details. *)
    let stdout = Fd.stdout () in
    let stderr = Fd.stderr () in
    let t = create stdout in
    let dev_and_ino fd =
      let stats = Core.Std.Unix.fstat (Fd.file_descr_exn fd) in
      (stats.st_dev, stats.st_ino)
    in
    if dev_and_ino stdout = dev_and_ino stderr
    then (t, t)
    else (t, create stderr)
  )
;;

let stdout = lazy (fst (Lazy.force stdout_and_stderr))
let stderr = lazy (snd (Lazy.force stdout_and_stderr))

let behave_nicely_in_pipeline ?writers () =
  let writers =
    match writers with
    | Some z -> z
    | None -> List.map [ stdout; stderr ] ~f:force
  in
  List.iter writers ~f:(fun writer ->
    set_buffer_age_limit writer `Unlimited;
    set_raise_when_consumer_leaves writer false;
    don't_wait_for (consumer_left writer >>| fun () -> Shutdown.shutdown 0))
;;

let apply_umask perm =
  let umask = Core_unix.umask 0 in
  ignore (Core_unix.umask umask);
  perm land (lnot umask)
;;

let with_file_atomic ?temp_file ?perm ?fsync:(do_fsync = false) file ~f =
  (Monitor.try_with (fun () -> Unix.stat file)
   >>| function
   | Ok stats -> Some stats.perm
   | Error _  -> None
  )
  >>= fun current_file_permissions ->
  Unix.mkstemp (Option.value temp_file ~default:file)
  >>= fun (temp_file, fd) ->
  let t = create fd in
  with_close t ~f:(fun () ->
    f t
    >>= fun result ->
    let new_permissions =
      match current_file_permissions with
      | None ->
        (* We are creating a new file; apply the umask. *)
        apply_umask (Option.value perm ~default:0o666)
      | Some p ->
        (* We are overwriting an existing file; use the requested permissions, or whatever
           the file had already if nothing was supplied. *)
        Option.value perm ~default:p
    in
    Unix.fchmod fd ~perm:new_permissions
    >>= fun () ->
    (if do_fsync then fsync t else Deferred.unit)
    >>| fun () ->
    result)
  >>= fun result ->
  Monitor.try_with (fun () -> Unix.rename ~src:temp_file ~dst:file)
  >>= function
  | Ok () -> return result
  | Error exn ->
    let fail v sexp_of_v =
      failwiths "Writer.with_file_atomic could not create file"
        (file, v) [%sexp_of: string * v]
    in
    Monitor.try_with (fun () -> Unix.unlink temp_file)
    >>| function
    | Ok () -> fail exn [%sexp_of: exn]
    | Error exn2 ->
      fail (exn, `Cleanup_failed exn2)
        [%sexp_of: exn * [ `Cleanup_failed of exn ]]
;;

let save ?temp_file ?perm ?fsync file ~contents =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    write t contents;
    Deferred.unit)
;;

let save_lines ?temp_file ?perm ?fsync file lines =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    List.iter lines ~f:(fun line -> write t line; newline t);
    Deferred.unit)
;;

let save_sexp ?temp_file ?perm ?fsync ?(hum=true) file sexp =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    write_sexp_internal t sexp ~hum ~terminate_with:Newline;
    Deferred.unit)
;;

let save_sexps ?temp_file ?perm ?fsync ?(hum=true) file sexps =
  with_file_atomic ?temp_file ?perm ?fsync file ~f:(fun t ->
    List.iter sexps ~f:(fun sexp ->
      write_sexp_internal t sexp ~hum ~terminate_with:Newline);
    Deferred.unit)

let with_flushed_at_close t ~flushed ~f =
  let producers_to_flush_at_close_elt =
    Bag.add t.producers_to_flush_at_close flushed
  in
  Monitor.protect f
    ~finally:(fun () ->
      Bag.remove t.producers_to_flush_at_close producers_to_flush_at_close_elt;
      Deferred.unit)
;;

let make_transfer ?(stop = Deferred.never ()) ?max_num_values_per_read t pipe_r write_f =
  let consumer =
    Pipe.add_consumer pipe_r ~downstream_flushed:(fun () -> flushed t >>| fun () -> `Ok)
  in
  let end_of_pipe_r = Ivar.create () in
  (* The only reason we can't use [Pipe.iter] is because it doesn't accept
     [?max_num_values_per_read]. *)
  let rec iter () =
    if Ivar.is_full t.consumer_left
       || Ivar.is_full t.close_finished
       || Deferred.is_determined stop
    then
      (* The [choose] in [doit] will become determined and [doit] will do the right
         thing. *)
      ()
    else begin
      let read_result =
        match max_num_values_per_read with
        | None                  -> Pipe.read_now' pipe_r ~consumer
        | Some max_queue_length -> Pipe.read_now' pipe_r ~consumer ~max_queue_length
      in
      match read_result with
      | `Eof               -> Ivar.fill end_of_pipe_r ()
      | `Nothing_available -> Pipe.values_available pipe_r >>> fun _ -> iter ()
      | `Ok q              ->
        write_f q ~cont:(fun () ->
          Pipe.Consumer.values_sent_downstream consumer;
          flushed t >>> iter)
    end
  in
  let doit () =
    (* Concurrecy between [iter] and [choose] is essential.  Even if [iter] gets blocked,
       for example on [flushed], the result of [doit] can still be determined by [choice]s
       other than [end_of_pipe_r]. *)
    iter ();
    choose [ choice (Ivar.read end_of_pipe_r) (fun () -> `End_of_pipe_r)
           ; choice stop                      (fun () -> `Stop         )
           ; choice (close_finished t)        (fun () -> `Writer_closed)
           ; choice (consumer_left  t)        (fun () -> `Consumer_left)
           ]
    >>| function
    | `End_of_pipe_r | `Stop -> ()
    | `Writer_closed | `Consumer_left -> Pipe.close_read pipe_r
  in
  with_flushed_at_close t ~f:doit
    ~flushed:(fun () -> Deferred.ignore (Pipe.upstream_flushed pipe_r))
;;

let transfer ?stop ?max_num_values_per_read t pipe_r write_f =
  make_transfer ?stop ?max_num_values_per_read
    t pipe_r (fun q ~cont -> Queue.iter q ~f:write_f; cont ())
;;

let transfer' ?stop ?max_num_values_per_read t pipe_r write_f =
  make_transfer ?stop ?max_num_values_per_read
    t pipe_r (fun q ~cont -> write_f q >>> cont)
;;

let pipe t =
  let pipe_r, pipe_w = Pipe.create () in
  don't_wait_for (transfer t pipe_r (fun s -> write t s));
  pipe_w
;;
