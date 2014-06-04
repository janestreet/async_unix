open Core.Std
open Import

module Read = Bin_prot.Read
module Type_class = Bin_prot.Type_class
module Unix = Unix_syscalls
module Id = Unique_id.Int63 (struct end)

module Read_result = struct
  module Z = struct
    type 'a t = [ `Ok of 'a | `Eof ] with bin_io, sexp

    let bind a f =
      match a with
      | `Ok a -> f a
      | `Eof -> `Eof
    ;;

    let map a ~f =
      match a with
      | `Ok a -> `Ok (f a)
      | `Eof -> `Eof
    ;;

    let map = `Custom map

    let return a = `Ok a
  end
  include Z
  include Monad.Make (Z)
end

(* We put everything in module [Internal] and then expose just the functions we want
   later.  This reminds us to wrap functions with [do_read], which we do to prevent
   multiple simultaneous active uses of a reader. *)
module Internal = struct

  module State = struct
    type t = [ `Not_in_use | `In_use | `Closed ]
    with sexp
  end

  module Open_flags = Unix.Open_flags

  type open_flags = (Open_flags.t, exn) Result.t with sexp_of

  type t =
    { fd : Fd.t;
      id : Id.t;
      (* [buf] holds data read by the reader from the OS, but not yet read by user code.
         When [t] is closed, [buf] is set to the empty buffer.  So, we must make sure in
         any code that accesses [buf] that [t] has not been closed.  In particular, after
         any deferred operation, we must check whether [t] has been closed while we were
         waiting. *)
      mutable buf : Bigstring.t sexp_opaque;
      (* [close_may_destroy_buf] indicates whether a call to [close] can immediately
         destroy [buf].  [close_may_destroy_buf] is usually [`Yes], except when we're in
         the middle of a system call in another thread that refers to [buf], in which case
         it is [`Not_now] and [close] can't destroy [buf], and we must wait until that
         system call finishes before doing so.

         [`Not_ever] is used for [read_one_chunk_at_a_time], which exposes [buf]
         to client code, which may in turn hold on to it (e.g. via
         [Bigstring.sub_shared]), and thus it is not safe to ever destroy it. *)
      mutable close_may_destroy_buf : [ `Yes | `Not_now | `Not_ever ];
      (* [pos] is the first byte of data in [buf] to b be read by user code. *)
      mutable pos : int;
      (* [available] is how many bytes in [buf] are available to be read by user code. *)
      mutable available : int;
      (* [bin_prot_len_buf] and [bin_prot_buf] are used by [read_bin_Perot]. *)
      mutable bin_prot_len_buf : Bigstring.t sexp_opaque;
      mutable bin_prot_buf : Bigstring.t sexp_opaque;
      (* [`Closed] means that [close t] has been called.  [`In_use] means there is some
         user call extant that is waiting for data from the reader. *)
      mutable state : State.t;
      close_finished : unit Ivar.t;
      mutable last_read_time : Time.t;
      (* [open_flags] is the open-file-descriptor bits of the reader's fd. *)
      open_flags : open_flags Deferred.t;
    }
  with fields, sexp_of

  let io_stats = Io_stats.create ()

  let invariant t : unit =
    assert (0 <= t.pos);
    assert (0 <= t.available);
    assert (t.pos + t.available <= Bigstring.length t.buf);
  ;;

  let create ?buf_len fd =
    let buf_len =
      match buf_len with
      | None ->
        let module K = Fd.Kind in
        begin match Fd.kind fd with
        | K.Char
        | K.File -> (32 * 1024)
        | K.Fifo
        | K.Socket _ -> (128 * 1024)
        end
      | Some buf_len ->
        if buf_len > 0 then
          buf_len
        else
          failwiths "Reader.create got non positive buf_len" (buf_len, fd)
            (<:sexp_of< int * Fd.t >>)
    in
    let open_flags = try_with (fun () -> Unix.fcntl_getfl fd) in
    { fd;
      id = Id.create ();
      buf = Bigstring.create buf_len;
      close_may_destroy_buf = `Yes;
      pos = 0;
      available = 0;
      bin_prot_len_buf = Bigstring.create 8;
      bin_prot_buf = Bigstring.create 4096;
      state = `Not_in_use;
      close_finished = Ivar.create ();
      last_read_time = Scheduler.cycle_start ();
      open_flags;
    };
  ;;

  let of_in_channel ic kind = create (Fd.of_in_channel ic kind)

  let open_file ?(close_on_exec = true) ?buf_len file =
    Unix.openfile file ~mode:[`Rdonly] ~perm:0o000 ~close_on_exec
    >>| fun fd ->
    create fd ?buf_len
  ;;

  let stdin = lazy (create (Fd.stdin ()))

  let close_finished t = Ivar.read t.close_finished

  let is_closed t =
    match t.state with
    | `Closed -> true
    | `Not_in_use | `In_use -> false
  ;;

  let empty_buf = Bigstring.create 0

  let destroy t =
    (* Calling [unsafe_destroy] on [t]'s bigstrings rather than waiting for finalizers to
       free them makes their space immediately available for reuse by C's malloc. *)
    Bigstring.unsafe_destroy t.buf;
    t.buf <- empty_buf;
    Bigstring.unsafe_destroy t.bin_prot_len_buf;
    t.bin_prot_len_buf <- empty_buf;
    Bigstring.unsafe_destroy t.bin_prot_buf;
    t.bin_prot_buf <- empty_buf;
  ;;

  let close t =
    begin match t.state with
    | `Closed -> ()
    | `Not_in_use | `In_use ->
      t.state <- `Closed;
      upon (Unix.close t.fd) (fun () -> Ivar.fill t.close_finished ());
      t.pos <- 0;
      t.available <- 0;
      match t.close_may_destroy_buf with
      | `Yes -> destroy t
      | `Not_now | `Not_ever -> ()
    end;
    close_finished t;
  ;;

  let with_close t ~f = Monitor.protect f ~finally:(fun () -> close t)

  let with_reader_exclusive t f =
    Unix.lockf t.fd `Read
    >>= fun () ->
    Monitor.protect f ~finally:(fun () ->
      if not (Fd.is_closed t.fd) then Unix.unlockf t.fd;
      Deferred.unit)
  ;;

  let with_file ?buf_len ?(exclusive = false) file ~f =
    open_file ?buf_len file >>= fun t ->
    with_close t ~f:(fun () ->
      if exclusive then
        with_reader_exclusive t (fun () -> f t)
      else
        f t)
  ;;

  (* [get_data t] attempts to read data into [t.buf].  If the read gets data, [get_data]
     returns [`Ok], otherwise it returns [`Eof]. *)
  let get_data t : [ `Ok | `Eof ] Deferred.t  =
    Deferred.create (fun result ->
      t.open_flags
      >>> fun open_flags ->
      let eof () = Ivar.fill result `Eof in
      match t.state with
      | `Not_in_use -> assert false
      | `Closed -> eof ()
      | `In_use ->
        let can_read_fd =
          match open_flags with
          | Error _ -> false
          | Ok open_flags -> Unix.Open_flags.can_read open_flags
        in
        if not can_read_fd then
          failwiths "not allowed to read due to file-descriptor flags" (open_flags, t)
            (<:sexp_of< open_flags * t >>);
        let ebadf () =
          (* If the file descriptior has been closed, we will get EBADF from a syscall.
             If someone closed the [Fd.t] using [Fd.close], then that is fine.  But if the
             underlying file descriptor got closed in some other way, then something is
             likely wrong, so we raise. *)
          failwiths "reader file descriptor was unexpectedly closed" t <:sexp_of< t >>
        in
        let finish res handle =
          match res with
          | `Already_closed -> eof ()
          | `Error exn ->
            begin match exn with
            | Bigstring.IOError (0, End_of_file)
            | Unix.Unix_error
                (( Unix.ECONNRESET
                 | Unix.ENETDOWN
                 | Unix.ENETRESET
                 | Unix.ENETUNREACH
                 | Unix.ETIMEDOUT
                 ), _, _)
              -> eof ()
            | Unix.Unix_error (Unix.EBADF, _, _) -> ebadf ()
            | _ -> handle exn
            end
          | `Ok (bytes_read, read_time) ->
            Io_stats.update io_stats ~kind:(Fd.kind t.fd)
              ~bytes:(Int63.of_int bytes_read);
            if bytes_read = 0 then
              eof ()
            else begin
              t.pos <- 0;
              t.available <- t.available + bytes_read;
              t.last_read_time <- read_time;
              Ivar.fill result `Ok
            end
        in
        let buf = t.buf in
        if t.available > 0 && t.pos > 0 then begin
          Bigstring.blit ~src:buf ~src_pos:t.pos ~dst:buf ~dst_pos:0
            ~len:t.available;
          t.pos <- 0;
        end;
        let pos = t.available in
        let len = Bigstring.length buf - pos in
        let module K = Fd.Kind in
        if not (Fd.supports_nonblock t.fd) then begin
          begin match t.close_may_destroy_buf with
          | `Yes -> t.close_may_destroy_buf <- `Not_now
          | `Not_now | `Not_ever -> ()
          end;
          Fd.syscall_in_thread t.fd ~name:"read"
            (fun file_descr ->
              let res = Bigstring.read file_descr buf ~pos ~len in
              res, Time.now ())
          >>> fun res ->
          begin match t.close_may_destroy_buf with
          | `Not_now -> t.close_may_destroy_buf <- `Yes
          | `Yes | `Not_ever -> ()
          end;
          match t.state with
          | `Not_in_use -> assert false
          | `In_use -> finish res raise
          | `Closed ->
            (* If we're here, somebody [close]d the reader while we were making the system
               call.  [close] couldn't [destroy], so we need to. *)
            destroy t; eof ()
        end else begin
          let rec loop () =
            (* Force the async cycle to end between reads, allowing others to run. *)
            Fd.ready_to t.fd `Read
            >>> function
              | `Bad_fd -> ebadf ()
              | `Closed -> eof ()
              | `Ready ->
                (* There is a race between the [ready_to] becoming determined and someone
                   [close]ing [t].  It is possible to get [`Ready] and then by the time we
                   get here, [t] is closed. *)
                match t.state with
                | `Not_in_use -> assert false
                | `Closed -> eof ()
                | `In_use ->
                  finish
                    (Fd.syscall t.fd ~nonblocking:true
                       (fun file_descr ->
                         let res =
                           Bigstring.read_assume_fd_is_nonblocking
                             file_descr buf ~pos ~len
                         in
                         res, Scheduler.cycle_start ()))
                    (let module U = Unix in
                     function
                     (* Since [t.fd] is ready, we should never see EWOULDBLOCK or EAGAIN.
                        But we don't trust the OS.  So, in case it does, we just try
                        again. *)
                     | U.Unix_error ((U.EWOULDBLOCK | U.EAGAIN), _, _) -> loop ()
                     | exn -> raise exn)
          in
          loop ()
        end)
  ;;

  (* [with_nonempty_buffer t f] waits for [t.buf] to have data, and then returns [f `Ok].
     If no data can be read, then [with_nonempty_buffer] returns [f `Eof].
     [with_nonempty_buffer] must be called with [t.state] as [`Closed] or [`In_use].  It
     guarantees that if [f `Ok] is called, that [t.state = `In_use]. *)
  let with_nonempty_buffer (type a) t (f : [ `Ok | `Eof ] -> a) : a Deferred.t =
    match t.state with
    | `Not_in_use -> assert false
    | `Closed -> return (f `Eof)
    | `In_use ->
      if t.available > 0 then
        return (f `Ok)
      else
        get_data t
        >>| fun ok_or_eof ->
        match t.state with
        | `Not_in_use -> assert false
        | `Closed -> f `Eof
        | `In_use -> f ok_or_eof
  ;;

  (* [with_nonempty_buffer' t f] is an optimized version of
     [don't_wait_for (with_nonempty_buffer t f)].

     With [force_refill = true], [with_nonempty_buffer'] will do a read, whether or not
     there is already data available in [t.buf]. *)
  let with_nonempty_buffer' ?(force_refill = false) t (f : [ `Ok | `Eof ] -> unit)
    : unit =
    match t.state with
    | `Not_in_use -> assert false
    | `Closed -> f `Eof
    | `In_use ->
      if not force_refill && t.available > 0 then
        f `Ok
      else
        get_data t
        >>> fun ok_or_eof ->
        match t.state with
        | `Not_in_use -> assert false
        | `Closed -> f `Eof
        | `In_use -> f ok_or_eof
  ;;

  let consume t amount =
    assert (0 <= amount && amount <= t.available);
    t.pos <- t.pos + amount;
    t.available <- t.available - amount;
  ;;

  type 'a read_one_chunk_at_a_time_result =
    [ `Eof
    | `Stopped of 'a
    | `Eof_with_unconsumed_data of string
    ]
  with sexp_of

  type consumed = [ `Consumed of int * [ `Need of int | `Need_unknown ] ] with sexp_of

  let read_one_chunk_at_a_time t ~handle_chunk =
    t.close_may_destroy_buf <- `Not_ever;
    Deferred.create (fun final_result ->
      let rec loop ~force_refill =
        with_nonempty_buffer' t ~force_refill (function
          | `Eof ->
            let result =
              if t.available > 0 then
                `Eof_with_unconsumed_data
                  (Bigstring.to_string t.buf ~pos:t.pos ~len:t.available)
              else
                `Eof
            in
            Ivar.fill final_result result
          | `Ok ->
            let len = t.available in
            let continue z =
              match t.state with
              | `Not_in_use -> assert false
              | `Closed -> Ivar.fill final_result `Eof
              | `In_use ->
                match z with
                | `Stop a -> consume t len; Ivar.fill final_result (`Stopped a)
                | `Stop_consumed (a, consumed) ->
                  consume t consumed; Ivar.fill final_result (`Stopped a)
                | `Continue -> consume t len; loop ~force_refill:true
                | `Consumed (consumed, need) as c ->
                  if consumed < 0 || consumed > len
                     || (match need with
                         | `Need_unknown -> false
                         | `Need need -> need < 0 || consumed + need <= len)
                  then
                    failwiths "handle_chunk returned invalid `Consumed" (c, `len len, t)
                      (<:sexp_of< consumed * [ `len of int ] * t >>);
                  consume t consumed;
                  let buf_len = Bigstring.length t.buf in
                  let new_len =
                    match need with
                    | `Need_unknown ->
                      if t.available = buf_len then
                        (* The buffer is full and the client doesn't know how much to
                           expect: double the buffer size. *)
                        buf_len * 2
                      else
                        buf_len
                    | `Need need ->
                      if need > buf_len then
                        Int.max need (buf_len * 2)
                      else
                        buf_len
                  in
                  if new_len < 0 then
                    failwiths
                      "read_one_chunk_at_a_time got overflow in buffer len" t
                      (<:sexp_of< t >>);
                  (* Grow the internal buffer if needed. *)
                  if new_len > buf_len then begin
                    let new_buf = Bigstring.create new_len in
                    if t.available > 0 then
                      Bigstring.blit ~src:t.buf ~src_pos:t.pos ~len:t.available
                        ~dst:new_buf ~dst_pos:0;
                    t.buf <- new_buf;
                    t.pos <- 0;
                  end;
                  loop ~force_refill:true
            in
            let deferred = handle_chunk t.buf ~pos:t.pos ~len in
            match Deferred.peek deferred with
            | None -> deferred >>> continue
            | Some result -> continue result)
      in
      loop ~force_refill:false)
  ;;

  module Read (S : Substring_intf.S) (Name : sig val name : string end) = struct

    let read_available t s =
      let len = Int.min t.available (S.length s) in
      S.blit_from_bigstring s ~src:t.buf ~src_pos:t.pos ~len;
      consume t len;
      `Ok len
    ;;

    let read t s =
      if S.length s = 0 then
        invalid_argf "Reader.read_%s with empty string" Name.name ();
      with_nonempty_buffer t (function
        | `Ok -> read_available t s
        | `Eof -> `Eof)
    ;;

    let really_read t s =
      Deferred.create (fun result ->
        let rec loop s amount_read =
          if S.length s = 0 then
            Ivar.fill result `Ok
          else begin
            read t s
            >>> function
              | `Eof -> Ivar.fill result (`Eof amount_read)
              | `Ok len -> loop (S.drop_prefix s len) (amount_read + len)
          end
        in
        loop s 0)
    ;;

  end

  module Read_substring = Read (Substring) (struct let name = "substring" end)

  let read_substring        = Read_substring.read
  let really_read_substring = Read_substring.really_read

  module Read_bigsubstring = Read (Bigsubstring) (struct let name = "bigsubstring" end)

  let read_bigsubstring        = Read_bigsubstring.read
  let really_read_bigsubstring = Read_bigsubstring.really_read

  let really_read_bigstring t bigstring =
    really_read_bigsubstring t (Bigsubstring.of_bigstring bigstring)
  ;;

  let read t ?pos ?len s = read_substring t (Substring.create s ?pos ?len)

  let really_read t ?pos ?len s = really_read_substring t (Substring.create s ?pos ?len)

  let read_char t =
    with_nonempty_buffer t (function
      | `Eof -> `Eof
      | `Ok ->
        let c = t.buf.{t.pos} in
        consume t 1;
        `Ok c)
  ;;

  let first_char t p =
    let limit = t.pos + t.available in
    let buf = t.buf in
    match p with
    | `Pred p ->
      let rec loop pos =
        if pos = limit then
          None
        else if p buf.{pos} then
          Some pos
        else
          loop (pos + 1)
      in
      (* [p] is supplied by the user and may raise, so we wrap [loop] in a [try_with].  We
         put the [try_with] here rather than around the call to [p] to avoid per-character
         try-with overhead. *)
      Or_error.try_with (fun () -> loop t.pos)
    | `Char ch ->
      let rec loop pos =
        if pos = limit then
          None
        else if ch = buf.{pos} then
          Some pos
        else
          loop (pos + 1)
      in
      Ok (loop t.pos)
  ;;

  let read_until_gen t p ~keep_delim ~max k =
    let rec loop ac total =
      with_nonempty_buffer' t (function
        | `Eof ->
          k (Ok (if ac = [] then `Eof
                 else `Eof_without_delim (Bigsubstring.concat_string (List.rev ac))))
        | `Ok ->
          let concat_helper ss lst =
            Bigsubstring.concat_string (List.rev_append lst [ss])
          in
          match first_char t p with
          | Error _ as e -> k e
          | Ok None ->
            let len = t.available in
            let total = total + len in
            let ss = Bigsubstring.create t.buf ~pos:t.pos ~len in
            t.buf <- Bigstring.create (Bigstring.length t.buf);
            t.pos <- 0;
            t.available <- 0;
            begin match max with
            | Some max when total > max ->
              let s = concat_helper ss ac in
              k (Ok (`Max_exceeded s))
            | Some _ | None -> loop (ss :: ac) total
            end
          | Ok (Some pos) ->
            let amount_consumed = pos + 1 - t.pos in
            let len =
              if keep_delim then amount_consumed else amount_consumed - 1
            in
            let ss = Bigsubstring.create t.buf ~pos:t.pos ~len in
            consume t amount_consumed;
            let res = concat_helper ss ac in
            k (Ok (`Ok res)))
    in
    loop [] 0
  ;;

  let read_until t pred ~keep_delim k =
    read_until_gen t pred ~keep_delim ~max:None (function
      | Error _ as x -> k x
      | Ok `Max_exceeded _ -> assert false  (* impossible - no maximum set *)
      | Ok (`Eof
           | `Eof_without_delim _
           | `Ok _) as x -> k x)
  ;;

  let line_delimiter_pred = `Char '\n'
  let read_line_gen t k =
    read_until t line_delimiter_pred ~keep_delim:false (function
      | Error _ ->
        (* Impossible, since we supplied a [`Char] predicate. *)
        assert false
      | Ok (`Eof | `Eof_without_delim _ as x) -> k x
      | Ok (`Ok line) ->
        k (`Ok (let len = String.length line in
                if len >= 1 && line.[len - 1] = '\r' then
                  String.sub line ~pos:0 ~len:(len - 1)
                else
                  line)))
  ;;

  let read_line t =
    Deferred.create (fun result ->
      read_line_gen t (fun z ->
        Ivar.fill result
          (match z with
           | `Eof_without_delim str -> `Ok str
           | `Ok _ | `Eof as x -> x)))
  ;;

  let really_read_line ~wait_time t =
    Deferred.create (fun result ->
      let fill_result = function
        | [] -> Ivar.fill result None
        | ac -> Ivar.fill result (Some (String.concat (List.rev ac))) in
      let rec continue ac =
        match t.state with
        | `Not_in_use -> assert false
        | `Closed -> fill_result ac
        | `In_use -> Clock.after wait_time >>> fun () -> loop ac
      and loop ac =
        read_line_gen t (function
          | `Eof -> continue ac
          | `Eof_without_delim str -> continue (str :: ac)
          | `Ok line -> fill_result (line :: ac))
      in
      loop []
    )
  ;;

  let space = Bigstring.of_string " "

  let gen_read_sexp ?parse_pos t parse k =
    let rec loop parse_fun =
      with_nonempty_buffer' t (function
        | `Eof ->
          (* The sexp parser doesn't know that a token ends at EOF, so we add a space to
             be sure. *)
          let module S = Sexp.Cont_state in
          begin match Or_error.try_with (fun () -> parse_fun ~pos:0 ~len:1 space) with
          | Error _ as e -> k e
          | Ok (Sexp.Done (sexp, parse_pos)) -> k (Ok (`Ok (sexp, parse_pos)))
          | Ok (Sexp.Cont (S.Parsing_whitespace, _)) -> k (Ok `Eof)
          | Ok (Sexp.Cont ((S.Parsing_atom
                           | S.Parsing_list
                           | S.Parsing_sexp_comment
                           | S.Parsing_block_comment), _))
            -> failwiths "Reader.read_sexp got unexpected eof" t <:sexp_of< t >>
          end;
        | `Ok ->
          match
            Or_error.try_with (fun () -> parse_fun ~pos:t.pos ~len:t.available t.buf)
          with
          | Error _ as e -> k e
          | Ok (Sexp.Done (sexp, parse_pos)) ->
            consume t (parse_pos.Sexp.Parse_pos.buf_pos - t.pos);
            k (Ok (`Ok (sexp, parse_pos)));
          | Ok (Sexp.Cont (_, parse_fun)) ->
            t.available <- 0;
            loop parse_fun)
    in
    let parse ~pos ~len buf =
      (* [parse_pos] will be threaded through the entire reading process by the sexplib
         code.  Every occurrence of [parse_pos] above will be identical to the [parse_pos]
         defined here. *)
      let parse_pos =
        match parse_pos with
        | None -> Sexp.Parse_pos.create ~buf_pos:pos ()
        | Some parse_pos -> Sexp.Parse_pos.with_buf_pos parse_pos pos
      in
      parse ?parse_pos:(Some parse_pos) ?len:(Some len) buf
    in
    loop parse
  ;;

  type 'a read = ?parse_pos : Sexp.Parse_pos.t -> 'a

  let gen_read_sexps ?parse_pos t parse =
    let pipe_r, pipe_w = Pipe.create () in
    let finished =
      Deferred.create (fun result ->
        let rec loop parse_pos =
          gen_read_sexp t parse ?parse_pos (function
            | Error error -> Error.raise error
            | Ok `Eof -> Ivar.fill result ()
            | Ok `Ok (sexp, parse_pos) ->
              if Pipe.is_closed pipe_w then
                Ivar.fill result ()
              else begin
                Pipe.write pipe_w sexp
                >>> fun () ->
                loop (Some parse_pos)
              end)
        in
        loop parse_pos)
    in
    upon finished (fun () ->
      close t
      >>> fun () ->
      Pipe.close pipe_w);
    pipe_r;
  ;;

  let read_sexps ?parse_pos t = gen_read_sexps t Sexp.parse_bigstring ?parse_pos

  let read_annotated_sexps ?parse_pos t =
    gen_read_sexps t Sexp.Annotated.parse_bigstring ?parse_pos
  ;;

  let read_bin_prot ?(max_len = 100 * 1024 * 1024) t bin_prot_reader k =
    let error f =
      ksprintf (fun msg () ->
        k (Or_error.error "Reader.read_bin_prot" (msg, t) <:sexp_of< string * t >>))
        f
    in
    let handle_eof n =
      if n = 0 then
        k (Ok `Eof)
      else
        error "got Eof with %d bytes left over" n ()
    in
    really_read_bigstring t t.bin_prot_len_buf
    >>> function
      | `Eof n -> handle_eof n
      | `Ok ->
        match t.state with
        | `Not_in_use -> assert false
        | `Closed -> error "Reader.read_bin_prot got closed reader" ()
        | `In_use ->
          let pos_ref = ref 0 in
          let expected_len = Bigstring.length t.bin_prot_len_buf in
          match
            Or_error.try_with (fun () ->
              Bin_prot.Read.bin_read_int_64bit t.bin_prot_len_buf ~pos_ref)
          with
          | Error _ as e -> k e
          | Ok len ->
            if !pos_ref <> expected_len then
              error "pos_ref <> len, (%d <> %d)" !pos_ref len ();
            if len > max_len then
              error "max read length exceeded: %d > %d" len max_len ();
            if len < 0 then
              error "negative length %d" len ();
            let prev_len = Bigstring.length t.bin_prot_buf in
            if len > prev_len then
              t.bin_prot_buf <-
                Bigstring.create (Int.min max_len (Int.max len (prev_len * 2)));
            let ss = Bigsubstring.create t.bin_prot_buf ~pos:0 ~len in
            really_read_bigsubstring t ss
            >>> function
              | `Eof n -> handle_eof n
              | `Ok ->
                match t.state with
                | `Not_in_use -> assert false
                | `Closed -> error "Reader.read_bin_prot got closed reader" ()
                | `In_use ->
                  let pos_ref = ref 0 in
                  match
                    Or_error.try_with (fun () ->
                      bin_prot_reader.Type_class.read t.bin_prot_buf ~pos_ref)
                  with
                  | Error _ as e -> k e
                  | Ok v ->
                    if !pos_ref <> len then
                      error "pos_ref <> len, (%d <> %d)" !pos_ref len ();
                    k (Ok (`Ok v))
  ;;

  let read_marshal_raw t =
    let eofn n =
      if n = 0 then `Eof
      else failwiths "Reader.read_marshal got EOF with bytes remaining" n <:sexp_of< int >>
    in
    let header = String.create Marshal.header_size in
    really_read t header >>= function
      | `Eof n -> return (eofn n)
      | `Ok ->
        let len = Marshal.data_size header 0 in
        let buf = String.create (len + Marshal.header_size) in
        String.blit ~src:header ~dst:buf ~src_pos:0 ~dst_pos:0
          ~len:Marshal.header_size;
        let sub = Substring.create buf ~pos:Marshal.header_size ~len in
        really_read_substring t sub >>| function
          | `Eof n -> eofn n
          | `Ok -> `Ok buf
  ;;

  let read_marshal t =
    read_marshal_raw t >>| function
      | `Eof -> `Eof
      | `Ok buf -> `Ok (Marshal.from_string buf 0)
  ;;

  let read_all t read_one =
    let pipe_r, pipe_w = Pipe.create () in
    let finished =
      Deferred.repeat_until_finished ()
        (fun () ->
          read_one t
          >>= function
            | `Eof -> return (`Finished ())
            | `Ok one ->
              if Pipe.is_closed pipe_w then
                return (`Finished ())
              else
                Pipe.write pipe_w one >>| fun () -> `Repeat ())
    in
    upon finished (fun () ->
      close t
      >>> fun () ->
      Pipe.close pipe_w);
    pipe_r
  ;;

  let lines t = read_all t read_line

  let contents t =
    let buf = Buffer.create 1024 in
    let sbuf = String.create 1024 in
    Deferred.repeat_until_finished () (fun () ->
      read t sbuf
      >>| function
        | `Eof -> `Finished ()
        | `Ok l -> Buffer.add_substring buf sbuf 0 l; `Repeat ())
    >>= fun () ->
    close t
    >>| fun () ->
    Buffer.contents buf
  ;;

  let recv t =
    Deferred.create (fun i ->
      read_line t
      >>> function
        | `Eof -> Ivar.fill i `Eof
        | `Ok length_str ->
          match try Ok (int_of_string length_str) with _ -> Error () with
          | Error () ->
            failwiths "Reader.recv got strange length" (length_str, t)
              (<:sexp_of< string * t >>)
          | Ok length ->
            let buf = String.create length in
            really_read t buf
            >>> function
              | `Eof _ -> failwith "Reader.recv got unexpected EOF"
              | `Ok -> Ivar.fill i (`Ok buf))
  ;;

  let transfer t pipe_w =
    Deferred.create
      (fun finished ->
        let rec loop () =
          with_nonempty_buffer' t (function
            | `Eof -> Ivar.fill finished ()
            | `Ok ->
              if Pipe.is_closed pipe_w then
                Ivar.fill finished ()
              else begin
                let pos = t.pos in
                let len = t.available in
                consume t len;
                Pipe.write pipe_w (Bigstring.to_string t.buf ~pos ~len)
                >>> loop
              end)
        in
        loop ())
  ;;
end

open Internal

(* We now expose all the functions in the mli.  For functions that access a reader in a
   deferred manner, we enclude code to dynamically ensure that there aren't simultaneous
   reads. *)

type nonrec t = t with sexp_of

type nonrec 'a read_one_chunk_at_a_time_result =
   'a read_one_chunk_at_a_time_result
with sexp_of

type nonrec 'a read = 'a read

let close          = close
let close_finished = close_finished
let create         = create
let fd             = fd
let id             = id
let invariant      = invariant
let io_stats       = io_stats
let is_closed      = is_closed
let last_read_time = last_read_time
let of_in_channel  = of_in_channel
let open_file      = open_file
let stdin          = stdin
let with_close     = with_close
let with_file      = with_file

let use t =
  let error s = failwiths "can not read from reader" (s, t) <:sexp_of< string * t >> in
  match t.state with
  | `Closed -> error "closed"
  | `In_use -> error "in use"
  | `Not_in_use -> t.state <- `In_use
;;

let finished_read t =
  match t.state with
  | `Closed -> () (* [f ()] closed it.  Leave it closed. *)
  | `Not_in_use -> assert false   (* we're using it *)
  | `In_use -> t.state <- `Not_in_use
;;

let do_read t f =
  use t;
  f ()
  >>| fun x ->
  finished_read t;
  x
;;

let read t ?pos ?len s    = do_read t (fun () -> read t ?pos ?len s)
let read_char t           = do_read t (fun () -> read_char t)
let read_substring t s    = do_read t (fun () -> read_substring t s)
let read_bigsubstring t s = do_read t (fun () -> read_bigsubstring t s)

let read_one_chunk_at_a_time t ~handle_chunk =
  do_read t (fun () -> read_one_chunk_at_a_time t ~handle_chunk)
;;

let really_read t ?pos ?len s =
  do_read t (fun () -> really_read t ?pos ?len s)
;;

let really_read_substring t s =
  do_read t (fun () -> really_read_substring t s)
;;

let really_read_bigsubstring t s =
  do_read t (fun () -> really_read_bigsubstring t s)
;;

let read_line t = do_read t (fun () -> read_line t)

let really_read_line ~wait_time t = do_read t (fun () -> really_read_line ~wait_time t)

(* [do_read_k] takes a [read_k] function that takes a continuation expecting an
   [Or_error.t].  It uses this to do a read returning a deferred.  This allows it to call
   [finished_read] before continuing, in the event that the result is an error. *)
let do_read_k
      (type r) (type r')
      t
      (read_k : (r Or_error.t -> unit) -> unit)
      (make_result : r -> r') : r' Deferred.t =
  use t;
  Deferred.create (fun result ->
    read_k (fun r ->
      finished_read t;
      Ivar.fill result (make_result (ok_exn r))));
;;

let read_until t p ~keep_delim =
  do_read_k t (read_until t p ~keep_delim) Fn.id
;;

let read_until_max t p ~keep_delim ~max =
  do_read_k t (read_until_gen t p ~keep_delim ~max:(Some max)) Fn.id
;;

let read_sexp ?parse_pos t =
  do_read_k t (gen_read_sexp t Sexp.parse_bigstring ?parse_pos)
    (function
     | `Eof -> `Eof
     | `Ok (sexp, _) -> `Ok sexp)
;;

let read_sexps ?parse_pos t =
  use t;
  read_sexps ?parse_pos t
;;

let read_annotated_sexps ?parse_pos t =
  use t;
  read_annotated_sexps ?parse_pos t
;;

let read_bin_prot ?max_len t reader =
  do_read_k t (read_bin_prot ?max_len t reader) Fn.id
;;

let read_marshal_raw t = do_read t (fun () -> read_marshal_raw t)
let read_marshal t     = do_read t (fun () -> read_marshal t)
let recv t             = do_read t (fun () -> recv t)

(* [read_all] does not call [use t], because [read_one] will do so each time it is using
   [t]. *)
let read_all t read_one = read_all t read_one

let lines t = use t; lines t

let contents t = do_read t (fun () -> contents t)

let file_contents file = with_file file ~f:contents

let file_lines file = open_file file >>= fun t -> Pipe.to_list (lines t)

let transfer t = use t; transfer t

let lseek t offset ~mode =
  do_read t (fun () ->
    t.pos <- 0;
    t.available <- 0;
    Unix_syscalls.lseek t.fd offset ~mode)
;;

let get_error file f annot_sexp =
  try
    match Sexp.Annotated.conv f annot_sexp with
    | `Result _ -> Ok ()
    | `Error (exc, bad_annot_sexp) ->
      Error (Error.of_exn (Sexp.Annotated.get_conv_exn ~file ~exc bad_annot_sexp))
  with exn ->
    error "Reader.load_sexp(s) error" (file, exn) <:sexp_of< string * exn >>
;;

let gen_load_exn ?exclusive file
      (type a)
      (convert : Sexp.t list -> a)
      (get_error : Sexp.Annotated.t list -> Error.t) : a Deferred.t =
  let fail exn =
    failwiths "Reader.load_sexp(s) error" (file, exn) <:sexp_of< string * exn >>
  in
  let load parse f =
    Monitor.try_with
      (fun () ->
         with_file ?exclusive file ~f:(fun t ->
           use t;
           Pipe.to_list (gen_read_sexps t parse)))
    >>= function
    | Ok sexps -> f sexps
    | Error exn -> fail exn
  in
  load Sexp.parse_bigstring (fun sexps ->
    try
      return (convert sexps)
    with
    | Of_sexp_error _ ->
      load Sexp.Annotated.parse_bigstring
        (fun sexps -> fail (Error.to_exn (get_error sexps)))
    | exn -> fail exn)
;;

type ('a, 'b) load =
  ?exclusive:bool
  -> ?expand_macros:bool
  -> string
  -> (Sexp.t -> 'a)
  -> 'b Deferred.t

module Macro_loader = Sexplib.Macro.Loader (struct

  module Monad = struct
    type 'a t = 'a Deferred.t
    let return = return
    module Monad_infix = Deferred.Monad_infix
    module List = struct
      let iter xs ~f = Deferred.List.iter xs ~f
      let map  xs ~f = Deferred.List.map  xs ~f
    end
  end

  let load_sexps file =
    Monitor.try_with ~extract_exn:true (fun () ->
      with_file file ~f:(fun t -> Pipe.to_list (read_sexps t)))
    >>| function
    | Ok sexps -> sexps
    | Error e -> raise (Sexplib.Macro.add_error_location file e)
  ;;

  let load_annotated_sexps file =
    Monitor.try_with ~extract_exn:true (fun () ->
      with_file file ~f:(fun t -> Pipe.to_list (read_annotated_sexps t)))
    >>| function
    | Ok sexps -> sexps
    | Error e -> raise (Sexplib.Macro.add_error_location file e)
  ;;
end)

let get_load_result_exn = function
  | `Result x -> x
  | `Error (exn, _sexp) -> raise exn
;;

let load_sexp_exn ?exclusive ?(expand_macros = false) file f =
  if expand_macros
  then Macro_loader.load_sexp_conv file f >>| get_load_result_exn
  else
    let multiple sexps =
      Error.create "Reader.load_sexp requires one sexp but got" (List.length sexps, file)
        <:sexp_of< int * string >>
    in
    gen_load_exn ?exclusive file
      (fun sexps ->
         match sexps with
         | [sexp] -> f sexp
         | _ -> Error.raise (multiple sexps))
      (fun annot_sexps ->
         match annot_sexps with
         | [annot_sexp] ->
           begin match get_error file f annot_sexp with
           | Error e -> e
           | Ok () ->
             Error.create "conversion of annotated sexp unexpectedly succeeded"
               (Sexp.Annotated.get_sexp annot_sexp) <:sexp_of< Sexp.t >>
           end
         | _ -> multiple annot_sexps)
;;

let load_sexp ?exclusive ?expand_macros file f =
  Deferred.Or_error.try_with (fun () -> load_sexp_exn ?exclusive ?expand_macros file f)
;;

let load_sexps_exn ?exclusive ?(expand_macros = false) file f =
  if expand_macros
  then Macro_loader.load_sexps_conv file f >>| List.map ~f:get_load_result_exn
  else
    gen_load_exn ?exclusive file
      (fun sexps -> List.map sexps ~f)
      (fun annot_sexps ->
        let errors =
          List.filter_map annot_sexps ~f:(fun annot_sexp ->
            match get_error file f annot_sexp with
            | Ok _ -> None
            | Error error -> Some error)
        in
        Error.create "Reader.load_sexps error" (file, errors)
          <:sexp_of< string * Error.t list >>)
;;

let load_sexps ?exclusive ?expand_macros file f =
  Deferred.Or_error.try_with (fun () -> load_sexps_exn ?exclusive ?expand_macros file f)
;;

let pipe t =
  let pipe_r, pipe_w = Pipe.create () in
  upon (transfer t pipe_w) (fun () ->
    close t
    >>> fun () ->
    Pipe.close pipe_w);
  pipe_r
;;

let drain t =
  read_one_chunk_at_a_time t
    ~handle_chunk:(fun _bigstring ~pos:_ ~len:_ -> return `Continue)
  >>= function
  | `Stopped _
  | `Eof_with_unconsumed_data _
    -> assert false
  | `Eof ->
    close t
;;
