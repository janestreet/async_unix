open Core.Std
open Import

module Core_unix = Core.Std.Unix
module Unix = Unix_syscalls

module IOVec = Core.Std.Unix.IOVec

module Id = Unique_id.Int63 (struct end)

let io_stats = Io_stats.create ()

module Check_buffer_age' = struct
  type 'a t =
    { writer : 'a;
      queue : (Int63.t * Time.t) Queue.t;
      maximum_age : Time.Span.t;
      (* The buffer-age check is responsible for filling in [too_old] if it detects an age
         violation. *)
      mutable too_old : unit Ivar.t;
    }
end

type flush = unit -> unit Deferred.t

module Flush_at_exit = struct
  type t =
    { producers : flush Bag.t;
      writer_flushed : flush;
    }

  let create writer_flushed = { producers = Bag.create (); writer_flushed }

  let add_producer t producer = Bag.add t.producers producer

  let remove_producer t producer_elt = Bag.remove t.producers producer_elt

  let flush t =
    Deferred.all_unit (Bag.fold t.producers ~init:[] ~f:(fun ac f -> (f ()) :: ac))
    >>= t.writer_flushed
  ;;
end

let flush_at_exits : Flush_at_exit.t Bag.t = Bag.create ()

type t =
  { id : Id.t;
    mutable fd : Fd.t;

    (* The writer uses a background thread to flush data.  It runs within [monitor].  *)
    monitor : Monitor.t;
    mutable background_writer_state : [ `Running | `Not_running | `Stopped_permanently ];

    (* [syscall] determines the batching approach that the writer uses to batch data
       together and flush it using the underlying write syscall. *)
    syscall : [ `Per_cycle | `Periodic of Time.Span.t ];

    (* Counts since the writer was created. *)
    mutable bytes_received : Int63.t;
    mutable bytes_written : Int63.t;

    (* Bytes that we have received but not yet written are stored in two places:
       [scheduled] and [buf].  The bytes that we need to write are the concatenation of
       the sequence of iovecs in [scheduled] followed by the bytes in [buf] from
       [scheduled_back] to [back].  Note that iovecs in [scheduled] can point to regions
       in [buf], even the current [buf] in the writer. *)

    (* [scheduled] holds iovecs that we plan to write. *)
    mutable scheduled : (Bigstring.t IOVec.t * [ `Destroy | `Keep ]) Queue.t sexp_opaque;

    (* [scheduled_bytes] is the sum of the lengths of the iovecs in [scheduled] *)
    mutable scheduled_bytes : int;

    (* [buf] has three regions:
       [0, scheduled_back)             received and scheduled
       [scheduled_back, back)          received but not scheduled
       [back, Bigstring.length buf)    free space
    *)
    mutable buf : Bigstring.t sexp_opaque;
    mutable scheduled_back : int;
    mutable back : int;

    flushes : (Time.t Ivar.t * Int63.t) Queue.t sexp_opaque;
    mutable is_closed : bool;
    close_finished : unit Ivar.t;

    mutable flush_at_exit_elt : Flush_at_exit.t Bag.Elt.t option sexp_opaque;

    mutable check_buffer_age : t Check_buffer_age'.t Bag.Elt.t option sexp_opaque;

    got_epipe : unit Ivar.t;
    mutable raise_epipe : bool;
  }
with fields, sexp_of

type writer = t with sexp_of

let set_raise_epipe t bool = t.raise_epipe <- bool

let iovecs_length iovecs =
  Queue.fold iovecs ~init:0 ~f:(fun n (iovec, _) -> n + iovec.IOVec.len)
;;

let bytes_to_write t = t.scheduled_bytes + t.back - t.scheduled_back

let is_stopped_permanently t =
  match t.background_writer_state with
  | `Stopped_permanently -> true
  | `Running | `Not_running -> false
;;

let invariant t =
  assert (Int63.(zero <= t.bytes_written && t.bytes_written <= t.bytes_received));
  assert (0 <= t.scheduled_back
           && t.scheduled_back <= t.back
           && t.back <= Bigstring.length t.buf);
  assert (t.scheduled_bytes = iovecs_length t.scheduled);
  if is_stopped_permanently t then
    assert (bytes_to_write t = 0)
  else begin
    assert (Bigstring.length t.buf > 0);
    assert (Int63.(t.bytes_received - t.bytes_written = of_int (bytes_to_write t)))
  end;
  Queue.iter t.scheduled ~f:(fun (iovec, kind) ->
    if phys_equal t.buf iovec.IOVec.buf then
      assert (kind = `Keep));
;;

module Check_buffer_age : sig
  type t = writer Check_buffer_age'.t Bag.Elt.t option

  val dummy : t
  val create :
    writer
    -> maximum_age:[ `At_most of Time.Span.t | `Unlimited ]
    -> t
  val destroy : t -> unit
  val too_old : t -> unit Deferred.t
end = struct
  open Check_buffer_age'

  type t = writer Check_buffer_age'.t Bag.Elt.t option

  let dummy = None

  exception Writer_buffer_has_data_older_than of Time.Span.t * writer with sexp

  let active_checks = Bag.create ()

  let maybe_start_loop =
    let loop_running = ref false in
    let rec loop () =
      if Bag.is_empty active_checks then
        loop_running := false
      else begin
        after (sec 1.)
        >>> fun () ->
        let now = Scheduler.cycle_start () in
        Bag.iter active_checks ~f:(fun e ->
          Queue.enqueue e.queue (e.writer.bytes_received, now));
        let backed_up_writers =
          Bag.fold active_checks ~init:[] ~f:(fun acc e ->
            let bytes_written = e.writer.bytes_written in
            let rec loop () =
              match Queue.peek e.queue with
              | None -> acc
              | Some (bytes_received, time) ->
                if Int63.(bytes_received <= bytes_written) then begin
                  ignore (Queue.dequeue e.queue);
                  loop ()
                end
                else if Time.Span.(>) (Time.diff now time) e.maximum_age then
                  e::acc
                else begin
                  if Ivar.is_full e.too_old then
                    e.too_old <- Ivar.create ();
                  acc
                end
            in
            loop ())
        in
        List.iter backed_up_writers ~f:(fun e ->
          Monitor.send_exn e.writer.monitor
            (Writer_buffer_has_data_older_than (e.maximum_age, e.writer));
          Ivar.fill_if_empty e.too_old ());
        loop ()
      end
    in
    fun () ->
      if not !loop_running then begin
        loop_running := true;
        loop ();
      end
  ;;

  let create writer ~maximum_age =
    match maximum_age with
    | `Unlimited -> None
    | `At_most maximum_age ->
      let res =
        Bag.add active_checks
          { writer;
            queue = Queue.create ();
            maximum_age;
            too_old = Ivar.create ();
          }
      in
      maybe_start_loop ();
      Some res
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
  if t.bytes_written = t.bytes_received then return (Time.now ())
  else
    Deferred.create (fun ivar ->
      Queue.enqueue t.flushes (ivar, t.bytes_received))
;;

let flushed t = Deferred.ignore (flushed_time t)

let set_fd t fd = flushed t >>| fun () -> t.fd <- fd

let () =
  Shutdown.at_shutdown (fun () ->
    Deferred.List.all_unit
      (Bag.fold flush_at_exits ~init:[]
         ~f:(fun ac flush_at_exit ->
           Flush_at_exit.flush flush_at_exit :: ac)))
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

let close_finished t = Ivar.read t.close_finished

let close_was_started t = t.is_closed

let is_open t = not (close_was_started t)

let got_epipe t = Ivar.read t.got_epipe

let stop_permanently t =
  t.background_writer_state <- `Stopped_permanently;
  Queue.clear t.scheduled;
  t.scheduled_bytes <- 0;
  t.buf <- Bigstring.create 0;
  t.scheduled_back <- 0;
  t.back <- 0;
;;

let die t error = stop_permanently t; Error.raise error

(* We used to use [after (sec 5.)] as the default value for [force_close] for all kinds of
   underlying fds. This was problematic, because it silently caused data in the writer's
   buffer to be dropped when it kicked in.  We care about data getting out only for the
   files, when we want to get data to disk.  When we close socket writers, we usually just
   want to drop the connection, so using [after (sec 5.)] makes sense. *)
let close ?force_close t =
  if not t.is_closed then begin
    t.is_closed <- true;
    let force_close =
      match force_close with
      | Some fc -> fc
      | None ->
        let module K = Fd.Kind in
        match Fd.kind t.fd with
        | K.File -> Deferred.never ()
        | K.Char | K.Fifo | K.Socket _ -> after (sec 5.)
    in
    (* We leave the buffer-age check running because it might fire while we're waiting for
       the flush. *)
    choose [choice (got_epipe t) ignore;
            choice (flushed t) ignore;
            choice force_close ignore;
            choice (Check_buffer_age.too_old t.check_buffer_age) ignore;
           ]
    >>> fun () ->
    begin match t.flush_at_exit_elt with
    | None -> assert false
    | Some elt -> Bag.remove flush_at_exits elt
    end;
    Check_buffer_age.destroy t.check_buffer_age;
    Unix.close t.fd
    >>> fun () ->
    Ivar.fill t.close_finished ()
  end;
  close_finished t
;;

let create
    ?buf_len
    ?(syscall = `Per_cycle)
    ?(buffer_age_limit = `At_most (Time.Span.of_min 2.))
    ?(raise_epipe = true)
    fd =
  let buf_len =
    match buf_len with
    | None -> 65 * 1024 * 2 (* largest observed single write call * 2 *)
    | Some buf_len ->
        if buf_len <= 0 then invalid_arg "Writer.create: buf_len <= 0"
        else buf_len
  in
  let id = Id.create () in
  let monitor =
    Monitor.create
      ~name:(sprintf "writer %s on fd %s" (Id.to_string id) (Fd.to_string fd))
      ()
  in
  let got_epipe = Ivar.create () in
  let t =
  { id;
    fd;
    syscall;
    monitor;
    buf = Bigstring.create buf_len;
    back = 0;
    scheduled_back = 0;
    scheduled = Queue.create ();
    scheduled_bytes = 0;
    bytes_received = Int63.zero;
    bytes_written = Int63.zero;
    flushes = Queue.create ();
    background_writer_state = `Not_running;
    is_closed = false;
    close_finished = Ivar.create ();
    flush_at_exit_elt = None;
    check_buffer_age = Check_buffer_age.dummy;
    got_epipe;
    raise_epipe;
  }
  in
  t.check_buffer_age <- Check_buffer_age.create t ~maximum_age:buffer_age_limit;
  t.flush_at_exit_elt <-
    Some (Bag.add flush_at_exits
            (Flush_at_exit.create
               (fun () ->
                 if Ivar.is_empty t.got_epipe then
                   flushed t
                 else
                   Deferred.unit)));
  t
;;

let of_out_channel oc kind = create (Fd.of_out_channel oc kind)

let to_string t = sprintf "<%s>" (Monitor.name t.monitor)

exception Attempt_to_use_closed_writer of t with sexp

let ensure_not_closed t = if t.is_closed then raise (Attempt_to_use_closed_writer t)

let open_file ?(append = false) file =
  (* Writing to /dropoff needs the [`Trunc] flag to avoid leaving extra junk at the end of
     a file. *)
  let mode = [ `Wronly; `Creat ] in
  let mode = (if append then `Append else `Trunc) :: mode in
  Unix.openfile file ~mode ~perm:0o666 >>| create
;;

let with_close t f = Monitor.protect f ~finally:(fun () -> close t)

let with_writer_exclusive t f =
  Unix.lockf t.fd `Write
  >>= fun () ->
  Monitor.protect f ~finally:(fun () -> flushed t >>| fun () -> Unix.unlockf t.fd)
;;

let with_file ?append ?(exclusive = false) file ~f =
  open_file ?append file
  >>= fun t ->
  with_close t (fun () ->
    if exclusive then
      with_writer_exclusive t (fun () -> f t)
    else
      f t)
;;

let got_bytes t n = t.bytes_received <- Int63.(t.bytes_received + of_int n)

let add_iovec t kind iovec ~count_bytes_as_received =
  assert (t.scheduled_back = t.back);
  if count_bytes_as_received then
    got_bytes t (iovec.IOVec.len - iovec.IOVec.pos);
  if not (is_stopped_permanently t) then begin
    t.scheduled_bytes <- t.scheduled_bytes + iovec.IOVec.len;
    Queue.enqueue t.scheduled (iovec, kind);
  end
;;

let schedule_unscheduled t kind =
  let need_to_schedule = t.back - t.scheduled_back in
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
  let n_iovecs = Int.min (Queue.length t.scheduled) IOVec.max_iovecs in
  let iovecs = Array.create n_iovecs dummy_iovec in
  let contains_mmapped_ref = ref false in
  let iovecs_len = ref 0 in
  with_return (fun {return} ->
    let i = ref 0 in
    Queue.iter t.scheduled ~f:(fun (iovec, _) ->
      if !i >= n_iovecs then return ();
      if not !contains_mmapped_ref
        && Bigstring.is_mmapped iovec.IOVec.buf
      then contains_mmapped_ref := true;
      iovecs_len := !iovecs_len + iovec.IOVec.len;
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
  if not t.is_closed then failwiths "writer fd unexpectedly closed " t <:sexp_of< t >>
;;

let rec start_write t =
  assert (t.background_writer_state = `Running);
  let module U = Unix in
  let iovecs, contains_mmapped, iovecs_len = mk_iovecs t in
  let handle_write_result = function
    | `Already_closed -> fd_closed t
    | `Ok n ->
      if n >= 0 then
        write_finished t n
      else
        die t (Error.create "write system call returned negative result" (t, n)
                 (<:sexp_of< t * int >>))
    | `Error (U.Unix_error ((U.EWOULDBLOCK | U.EAGAIN), _, _)) ->
      write_when_ready t
    | `Error (U.Unix_error (U.EBADF, _, _)) ->
      die t (Error.create "write got EBADF" t <:sexp_of< t >>)
    | `Error ((U.Unix_error ((U.EPIPE | U.ECONNRESET), _, _)) as exn) ->
      (* [t.got_epipe] is empty since once we reach this point, we stop the writer
         permanently, and so will never reach here again. *)
      assert (Ivar.is_empty t.got_epipe);
      Ivar.fill t.got_epipe ();
      stop_permanently t;
      if t.raise_epipe then raise exn;
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
    Fd.syscall_in_thread t.fd (fun file_descr -> Bigstring.writev file_descr iovecs)
    >>> handle_write_result
  end else
    handle_write_result
      (Fd.syscall t.fd ~nonblocking:true
         (fun file_descr ->
           Bigstring.writev_assume_fd_is_nonblocking file_descr iovecs))

and write_when_ready t =
  assert (t.background_writer_state = `Running);
  Fd.ready_to t.fd `Write
  >>> function
    | `Bad_fd -> die t (Error.create "writer ready_to got Bad_fd" t <:sexp_of< t >>)
    | `Closed -> fd_closed t
    | `Ready -> start_write t

and write_finished t bytes_written =
  assert (t.background_writer_state = `Running);
  let int63_bytes_written = Int63.of_int bytes_written in
  Io_stats.update io_stats ~kind:(Fd.kind t.fd) ~bytes:int63_bytes_written;
  t.bytes_written <- Int63.(int63_bytes_written + t.bytes_written);
  if Int63.(t.bytes_written > t.bytes_received) then
    die t (Error.create "writer wrote more bytes than it received" t <:sexp_of< t >>);
  fill_flushes t;
  t.scheduled_bytes <- t.scheduled_bytes - bytes_written;
  (* Remove processed iovecs from t.scheduled. *)
  let rec remove_done bytes_written =
    assert (bytes_written >= 0);
    match Queue.dequeue t.scheduled with
    | None ->
      if bytes_written > 0 then
        die t (Error.create "writer wrote nonzero amount but IO_queue is empty" t
                 (<:sexp_of< t >>))
    | Some ({ IOVec. buf; pos; len }, kind) ->
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
        let new_scheduled = Queue.create () in
        Queue.enqueue new_scheduled (new_iovec, kind);
        Queue.transfer ~src:t.scheduled ~dst:new_scheduled;
        t.scheduled <- new_scheduled;
      end
  in
  remove_done bytes_written;
  (* See if there's anything else to do. *)
  schedule_unscheduled t `Keep;
  if Queue.is_empty t.scheduled then begin
    t.back <- 0;
    t.scheduled_back <- 0;
    t.background_writer_state <- `Not_running;
  end else begin
    match t.syscall with
    | `Per_cycle -> write_when_ready t
    | `Periodic span ->
      after span
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
    schedule ~monitor:t.monitor ~priority:Priority.low (fun () -> start_write t);
;;

let give_buf t desired =
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
    (* Preallocated buffer too small; schedule buffered writes *)
    (* We have to divide the buffer length by two in the following comparison
       to avoid wasting buffer space on subsequent writes of slightly more
       than half the buffer length. *)
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

module Write_substring (S : Substring_intf.S) = struct
  let write t src =
    let len = S.length src in
    if is_stopped_permanently t then
      got_bytes t len
    else begin
      let available = Bigstring.length t.buf - t.back in
      if available >= len then begin
        got_bytes t len;
        S.blit_to_bigstring src ~dst:t.buf ~dst_pos:t.back;
        t.back <- t.back + len;
      end else begin
        got_bytes t available;
        S.blit_to_bigstring (S.prefix src available) ~dst:t.buf ~dst_pos:t.back;
        t.back <- t.back + available;
        let dst, dst_pos = give_buf t (len - available) in
        S.blit_to_bigstring (S.drop_prefix src available) ~dst ~dst_pos;
      end;
      maybe_start_writer t;
    end
  ;;
end

let write_substring =
  let module W = Write_substring (Substring) in
  W.write
;;

let write_bigsubstring =
  let module W = Write_substring (Bigsubstring) in
  W.write
;;

let write ?pos ?len t string = write_substring t (Substring.create ?pos ?len string)

let writef t = ksprintf (fun s -> write t s)

let to_formatter t =
  Format.make_formatter
    (fun str pos len ->
      ensure_not_closed t;
      write_substring t (Substring.create str ~pos ~len))
    ignore
;;

let write_char t c =
  if is_stopped_permanently t then
    got_bytes t 1
  else begin
    (* Check for the common case that the char can simply be put in the buffer. *)
    if Bigstring.length t.buf - t.back >= 1 then begin
      got_bytes t 1;
      t.buf.{t.back} <- c;
      t.back <- t.back + 1;
    end else begin
      let dst, dst_pos = give_buf t 1 in
      dst.{dst_pos} <- c;
    end;
    maybe_start_writer t
  end
;;

let newline t = write_char t '\n'

let write_byte t i = write_char t (char_of_int (i % 256))

let write_sexp =
  let initial_size = 1024 * 1024 in
  let buffer = Buffer.create initial_size in
  let blit_str = ref (String.create initial_size) in
  fun ?(hum = false) t sexp ->
    Buffer.clear buffer;
    if hum then
      Sexp.to_buffer_hum ~buf:buffer ~indent:!Sexp.default_indent sexp
    else
      Sexp.to_buffer ~buf:buffer sexp;
    let len = Buffer.length buffer in
    let blit_str_len = String.length !blit_str in
    if len > blit_str_len then blit_str := String.create (max len (2 * blit_str_len));
    Buffer.blit buffer 0 !blit_str 0 len;
    write t !blit_str ~len;
    (* If the string representation doesn't start/end with paren or double quote, we add a
       space after it to ensure that the parser can recognize the end of the sexp. *)
    let c = !blit_str.[0] in
    if not (c = '(' || c = '"') then
      write_char t ' ';
;;

include (struct
  let len_len = 8

  module Write_bin_prot_bug = struct
    type t =
      { pos : int;
        start_pos : int;
        tot_len : int;
        len : int;
        len_len : int;
        pos_len : int;
      }
    with sexp
  end

  let write_bin_prot t writer v =
    let len            = writer.Bin_prot.Type_class.size v in
    let tot_len        = len + len_len in
    if is_stopped_permanently t then
      got_bytes t tot_len
    else begin
      let buf, start_pos = give_buf t tot_len in
      let pos_len        = Bin_prot.Write_ml.bin_write_int_64bit buf ~pos:start_pos len in
      let pos            = writer.Bin_prot.Type_class.write buf ~pos:pos_len v in
      if pos - start_pos <> tot_len then begin
        failwiths "write_bin_prot"
          { Write_bin_prot_bug. pos; start_pos; tot_len; len; len_len; pos_len }
          (<:sexp_of< Write_bin_prot_bug.t >>)
      end;
      maybe_start_writer t;
    end
end : sig
  val write_bin_prot : t -> 'a Bin_prot.Type_class.writer -> 'a -> unit
end)

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

(* The code below ensures that no calls happen on a closed writer. *)

let flushed_time t = ensure_not_closed t; flushed_time t
let flushed t = ensure_not_closed t; flushed t
let fsync t   = ensure_not_closed t; flushed t >>= fun _ -> Unix.fsync t.fd
let fdatasync t =
  ensure_not_closed t; flushed t >>= fun _ -> Unix.fdatasync t.fd
;;
let write_bin_prot t sw_arg v = ensure_not_closed t; write_bin_prot t sw_arg v
let send t s                  = ensure_not_closed t; send t s
let schedule_iovec t iovec    = ensure_not_closed t; schedule_iovec t iovec
let schedule_iovecs t iovecs  = ensure_not_closed t; schedule_iovecs t iovecs
let schedule_bigstring t ?pos ?len bstr =
  ensure_not_closed t; schedule_bigstring t ?pos ?len bstr
let write ?pos ?len t s       = ensure_not_closed t; write ?pos ?len t s
let writef t                  = ensure_not_closed t; writef t
let write_marshal t ~flags v  = ensure_not_closed t; write_marshal t ~flags v
let write_sexp ?hum t s       = ensure_not_closed t; write_sexp ?hum t s
let write_bigsubstring t s    = ensure_not_closed t; write_bigsubstring t s
let write_substring t s       = ensure_not_closed t; write_substring t s
let write_byte t b            = ensure_not_closed t; write_byte t b
let write_char t c            = ensure_not_closed t; write_char t c
let newline t                 = ensure_not_closed t; newline t

let stdout_and_stderr =
  lazy (
    (* The following code checks to see if stdout and stderr point to the same file, and
       if so, shares a single writer between them. *)
    let stdout = Fd.stdout () in
    let stderr = Fd.stderr () in
    let t = create stdout in
    let module U = Core.Std.Unix in
    let dev_and_ino fd =
      let stats = U.fstat (Fd.file_descr_exn fd) in
      (stats.U.st_dev, stats.U.st_ino)
    in
    if dev_and_ino stdout = dev_and_ino stderr then
      (t, t)
    else
      (t, create stderr)
  )
;;

let stdout = lazy (fst (Lazy.force stdout_and_stderr))
let stderr = lazy (snd (Lazy.force stdout_and_stderr))

let apply_umask perm =
  let umask = Core_unix.umask 0 in
  ignore (Core_unix.umask umask);
  perm land (lnot umask)
;;

let save ?temp_prefix ?perm ?fsync:(do_fsync = false) file ~contents =
  Async_sys.file_exists file
  >>= fun file_exists ->
  (match file_exists with
  | `Yes -> (Unix.stat file >>| fun stats -> Some stats.Unix.Stats.perm)
  | `No | `Unknown -> return None)
  >>= fun current_file_permissions ->
  let prefixed_temp_file =
    match temp_prefix with
    | None -> file
    | Some temp_prefix -> temp_prefix ^ file
  in
  Unix.mkstemp prefixed_temp_file
  >>= fun (temp_file, fd) ->
  let t = create fd in
  with_close t (fun () ->
    write t contents;
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
    if do_fsync then Unix.fsync fd else Deferred.unit)
  >>= fun () ->
  Monitor.try_with (fun () -> Unix.rename ~src:temp_file ~dst:file)
  >>| function
    | Ok () -> ()
    | Error exn ->
      whenever (Unix.unlink temp_file);
      failwiths "Writer.save could not create file" (file, exn) <:sexp_of< string * exn >>
;;

let sexp_to_buffer ?(hum = true) ~buf sexp =
  if hum then
    Sexp.to_buffer_hum ~buf sexp
  else
    Sexp.to_buffer_mach ~buf sexp
;;

let save_sexp ?temp_prefix ?perm ?fsync ?hum file sexp =
  let buf = Buffer.create 1 in
  sexp_to_buffer ?hum ~buf sexp;
  Buffer.add_char buf '\n';
  save ?temp_prefix ?perm ?fsync file ~contents:(Buffer.contents buf);
;;

let attach_pipe ?(close_on_eof = true) t pipe_r write_f =
  match t.flush_at_exit_elt with
  | None -> assert false
  | Some flush_at_exit_elt ->
    let flush_at_exit = Bag.Elt.value flush_at_exit_elt in
    let producer_elt =
      Flush_at_exit.add_producer flush_at_exit (fun () ->
        Deferred.ignore (Pipe.flushed pipe_r))
    in
    let rec loop () =
      Pipe.read' pipe_r
      >>> function
        | `Eof ->
          Flush_at_exit.remove_producer flush_at_exit producer_elt;
          if close_on_eof then whenever (close t)
        | `Ok data ->
          Queue.iter data ~f:write_f;
          flushed t
          >>> fun () ->
          loop ()
    in
    loop ()
;;

let pipe t =
  let pipe_r, pipe_w = Pipe.create () in
  attach_pipe t pipe_r (fun s -> write t s);
  pipe_w
;;
