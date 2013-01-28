open Core.Std
open Import

module Read_ml = Bin_prot.Read_ml
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

    let return a = `Ok a
  end
  include Z
  include (Monad.Make (Z) : Monad.S with type 'a t := 'a t)
end

module State = struct
  type t = [ `Not_in_use | `In_use | `Closed ]
  with sexp

  let to_string t = Sexp.to_string (sexp_of_t t)
end

type t = {
  fd : Fd.t;
  id : Id.t;
  mutable buf : Bigstring.t sexp_opaque;
  mutable pos : int;
  mutable available : int;
  bin_prot_len_buf : Bigstring.t sexp_opaque;
  mutable bin_prot_buf : Bigstring.t sexp_opaque;
  mutable state : State.t;
  close_finished : unit Ivar.t;
  mutable last_read_time : Time.t;
} with fields, sexp_of

let io_stats = Io_stats.create ()

let invariant t =
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
  { fd = fd;
    id = Id.create ();
    buf = Bigstring.create buf_len;
    pos = 0;
    available = 0;
    bin_prot_len_buf = Bigstring.create 8;
    bin_prot_buf = Bigstring.create 4096;
    state = `Not_in_use;
    close_finished = Ivar.create ();
    last_read_time = Scheduler.cycle_start ();
  }
;;

let of_in_channel ic kind = create (Fd.of_in_channel ic kind)

let open_file ?buf_len file =
  Unix.openfile file ~mode:[`Rdonly] ~perm:0o000
  >>| fun fd ->
  create fd ?buf_len
;;

let stdin = lazy (create (Fd.stdin ()))

let closed t = Ivar.read t.close_finished

let close_was_started t =
  match t.state with
  | `Closed -> true
  | `Not_in_use | `In_use -> false

let close t =
  begin match t.state with
  | `Closed -> ()
  | `Not_in_use | `In_use ->
      t.state <- `Closed;
      whenever (
        Unix.close t.fd >>| fun () ->
        Ivar.fill t.close_finished ());
  end;
  closed t
;;

let with_close t f =
  Monitor.protect f ~finally:(fun () -> close t)
;;

let with_reader_exclusive t f =
  Unix.lockf t.fd `Read
  >>= fun () ->
  Monitor.protect f ~finally:(fun () -> Unix.unlockf t.fd; Deferred.unit)
;;

let with_file ?buf_len ?(exclusive = false) file ~f =
  open_file ?buf_len file >>= fun t ->
    with_close t (fun () ->
      if exclusive then
        with_reader_exclusive t (fun () -> f t)
      else
        f t)
;;

let get_data t =
  if t.available > 0 then
    `Available
  else
    `Wait
      (Deferred.create (fun result ->
        let eof () = Ivar.fill result `Eof in
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
            | Bigstring.IOError (0, End_of_file) -> eof ()
            | Unix.Unix_error (Unix.ECONNRESET, _, _) -> eof ()
            | Unix.Unix_error (Unix.EBADF, _, _) -> ebadf ()
            | _ -> handle exn
            end
          | `Ok (bytes_read, read_time) ->
            Io_stats.update io_stats ~kind:(Fd.kind t.fd) ~bytes:(Int63.of_int bytes_read);
            if bytes_read = 0 then
              eof ()
            else begin
              t.pos <- 0;
              t.available <- bytes_read;
              t.last_read_time <- read_time;
              Ivar.fill result `Ok
            end
        in
        let buf = t.buf in
        let pos = 0 in
        let len = Bigstring.length buf in
        let module K = Fd.Kind in
        if not (Fd.supports_nonblock t.fd) then begin
          Fd.syscall_in_thread t.fd
            (fun file_descr ->
              let res = Bigstring.read file_descr buf ~pos ~len in
              res, Time.now ())
          >>> fun res ->
          finish res raise
        end else begin
          let rec loop () =
            (* Force the async cycle to end between reads, allowing others to run. *)
            Fd.ready_to t.fd `Read
            >>> function
              | `Bad_fd -> ebadf ()
              | `Closed -> eof ()
              | `Ready ->
                finish
                  (Fd.syscall t.fd ~nonblocking:true
                     (fun file_descr ->
                       let res =
                         Bigstring.read_assume_fd_is_nonblocking file_descr buf ~pos ~len
                       in
                       res, Scheduler.cycle_start ()))
                  (let module U = Unix in
                   function
                     (* Since we just did a select, we should never see EWOULDBLOCK or
                        EAGAIN.  But we don't trust the OS.  So, in case it does, we'd
                        rather select again. *)
                     | U.Unix_error ((U.EWOULDBLOCK | U.EAGAIN), _, _) -> loop ()
                     | exn -> raise exn)
          in
          loop ()
        end))
;;

let nonempty_buffer t f =
  match get_data t with
  | `Available -> f `Ok
  | `Wait d -> d >>> f
;;

let get_data t f =
  match get_data t with
  | `Available -> return (f `Ok)
  | `Wait d -> d >>| f
;;

let consume t amount =
  assert (0 <= amount && amount <= t.available);
  t.pos <- t.pos + amount;
  t.available <- t.available - amount;
;;

let read_one_chunk_at_a_time_until_eof t ~handle_chunk =
  Deferred.create (fun final_result ->
    let rec loop () =
      nonempty_buffer t
        (function
          | `Eof -> Ivar.fill final_result `Eof
          | `Ok ->
            handle_chunk t.buf ~pos:t.pos ~len:t.available
            >>> fun result ->
            consume t t.available;
            match result with
            | `Stop a -> Ivar.fill final_result (`Stopped a)
            | `Continue -> loop ())
    in
    loop ())
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
    get_data t (function
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
let read_substring = Read_substring.read
let really_read_substring = Read_substring.really_read

module Read_bigsubstring =
  Read (Bigsubstring) (struct let name = "bigsubstring" end)

let read_bigsubstring = Read_bigsubstring.read
let really_read_bigsubstring = Read_bigsubstring.really_read

let really_read_bigstring t bigstring =
  really_read_bigsubstring t (Bigsubstring.of_bigstring bigstring)
;;

let read t ?pos ?len s = read_substring t (Substring.create s ?pos ?len)

let really_read t ?pos ?len s =
  really_read_substring t (Substring.create s ?pos ?len)
;;

let read_char t =
  get_data t (function
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
    loop t.pos
  | `Char ch ->
    let rec loop pos =
      if pos = limit then
        None
      else if ch = buf.{pos} then
        Some pos
      else
        loop (pos + 1)
    in
    loop t.pos
;;

let read_until_gen t p ~keep_delim ~max k =
  Deferred.create (fun result ->
    let rec loop ac total =
      nonempty_buffer t (function
        | `Eof ->
          Ivar.fill result
            (k (if ac = [] then `Eof
              else `Eof_without_delim (Bigsubstring.concat_string (List.rev ac))))
        | `Ok ->
          let concat_helper ss lst =
            Bigsubstring.concat_string (List.rev_append lst [ss])
          in
          match first_char t p with
          | None ->
            let len = t.available in
            let total = total + len in
            let ss = Bigsubstring.create t.buf ~pos:t.pos ~len in
            t.buf <- Bigstring.create (Bigstring.length t.buf);
            t.pos <- 0;
            t.available <- 0;
            begin match max with
            | Some max when total > max ->
              let s = concat_helper ss ac in
              Ivar.fill result (k (`Max_exceeded s))
            | Some _ | None -> loop (ss :: ac) total
            end
          | Some pos ->
            let amount_consumed = pos + 1 - t.pos in
            let len =
              if keep_delim then amount_consumed else amount_consumed - 1
            in
            let ss = Bigsubstring.create t.buf ~pos:t.pos ~len in
            consume t amount_consumed;
            let res = concat_helper ss ac in
            Ivar.fill result (k (`Ok res)))
    in
    loop [] 0)
;;

let read_until_max t pred ~keep_delim ~max =
  read_until_gen t pred ~keep_delim ~max:(Some max) Fn.id
;;

let read_until t pred ~keep_delim k =
  read_until_gen t pred ~keep_delim ~max:None (function
    | `Max_exceeded _ -> assert false  (* impossible - no maximum set *)
    | `Eof
    | `Eof_without_delim _
    | `Ok _ as x -> k x)

let line_delimiter_pred = `Char '\n'
let read_line_gen t k =
  read_until t line_delimiter_pred ~keep_delim:false (function
    | `Eof | `Eof_without_delim _ as x -> k x
    | `Ok line ->
        k (`Ok (let len = String.length line in
                if len >= 1 && line.[len - 1] = '\r' then
                  String.sub line ~pos:0 ~len:(len - 1)
                else
                  line)))
;;

let read_line t =
  read_line_gen t (function
    | `Eof_without_delim str -> `Ok str
    | `Ok _ | `Eof as x -> x)
;;

let really_read_line ~wait_time t =
  Deferred.create (fun result ->
    let fill_result = function
      | [] -> Ivar.fill result None
      | ac -> Ivar.fill result (Some (String.concat (List.rev ac))) in
    let rec loop ac =
      let continue ac =
        if t.state = `Closed then
          fill_result ac
        else begin
          Clock.after wait_time
          >>> fun () -> loop ac
        end
      in
      read_line_gen t Fn.id >>> function
        | `Eof -> continue ac
        | `Eof_without_delim str -> continue (str :: ac)
        | `Ok line -> fill_result (line :: ac)
    in
    loop []
  )
;;

let space = Bigstring.of_string " "

let gen_read_sexp ?parse_pos t parse =
  Deferred.create (fun result ->
    let rec loop parse_fun =
      nonempty_buffer t (function
        | `Eof ->
          begin
            (* The sexp parser doesn't know that a token ends at EOF, so we
               add a space to be sure. *)
            match parse_fun ~pos:0 ~len:1 space with
            | Sexp.Done (sexp, parse_pos) ->
                Ivar.fill result (`Ok (sexp, parse_pos))
            | Sexp.Cont (Sexp.Cont_state.Parsing_whitespace, _) ->
                Ivar.fill result `Eof
            | Sexp.Cont _ ->
                failwiths "Reader.read_sexp got unexpected eof"
                  t <:sexp_of< t >>
          end
        | `Ok ->
          match parse_fun ~pos:t.pos ~len:t.available t.buf with
          | Sexp.Done (sexp, parse_pos) ->
              consume t (parse_pos.Sexp.Parse_pos.buf_pos - t.pos);
              Ivar.fill result (`Ok (sexp, parse_pos));
          | Sexp.Cont (_, parse_fun) ->
              t.available <- 0;
              loop parse_fun)
    in
    let parse ~pos ~len buf =
      (* [parse_pos] will be threaded through the entire reading process by
         the sexplib code.  Every occurrence of [parse_pos] above will be
         identical to the [parse_pos] defined here. *)
      let parse_pos =
        match parse_pos with
        | None -> Sexp.Parse_pos.create ~buf_pos:pos ()
        | Some parse_pos -> Sexp.Parse_pos.with_buf_pos parse_pos pos
      in
      parse ?parse_pos:(Some parse_pos) ?len:(Some len) buf
    in
    loop parse)
;;

type 'a read = ?parse_pos : Sexp.Parse_pos.t -> 'a

let read_sexp ?parse_pos t =
  gen_read_sexp t Sexp.parse_bigstring ?parse_pos
  >>| function
    | `Eof -> `Eof
    | `Ok (sexp, _) -> `Ok sexp
;;

let transfer_sexps ?parse_pos t parse pipe_writer =
  (* Read sexps from [t] and write them to [pipe_writer], stopping when we reach Eof or
     the pipe is closed. *)
  let rec loop parse_pos =
    gen_read_sexp t parse ?parse_pos >>> function
      | `Eof -> Pipe.close pipe_writer
      | `Ok (sexp, parse_pos) ->
        if not (Pipe.is_closed pipe_writer) then begin
          Pipe.write pipe_writer sexp
          >>> fun () ->
          loop (Some parse_pos)
        end
  in
  loop parse_pos;
;;

let gen_read_sexps ?parse_pos t parse =
  let pipe_r, pipe_w = Pipe.create () in
  transfer_sexps t parse pipe_w ?parse_pos;
  pipe_r
;;

let read_sexps ?parse_pos t = gen_read_sexps t Sexp.parse_bigstring ?parse_pos

let read_bin_prot ?(max_len = 100 * 1024 * 1024) t bin_prot_reader =
  let error f =
    ksprintf (fun msg () ->
      failwiths "Reader.read_bin_prot" (msg, t) <:sexp_of< string * t >>)
      f
  in
  Deferred.create (fun result ->
    let handle_eof n =
      if n = 0 then
        Ivar.fill result `Eof
      else
        error "got Eof with %d bytes left over" n ()
    in
    really_read_bigstring t t.bin_prot_len_buf
    >>> function
      | `Eof n -> handle_eof n
      | `Ok ->
        let pos_ref = ref 0 in
        let expected_len = Bigstring.length t.bin_prot_len_buf in
        let len = Read_ml.bin_read_int_64bit t.bin_prot_len_buf ~pos_ref in
        if !pos_ref <> expected_len then
          error "pos_ref <> len, (%d <> %d)" !pos_ref len ();
        if len > max_len then
          error "max read length exceeded: %d > %d" len max_len ();
        if len < 0 then
          error "negative length %d" len ();
        let prev_len = Bigstring.length t.bin_prot_buf in
        if len > prev_len then
          t.bin_prot_buf <- Bigstring.create
            (Int.min max_len (Int.max len (prev_len * 2)));
        let ss = Bigsubstring.create t.bin_prot_buf ~pos:0 ~len in
        really_read_bigsubstring t ss
        >>> function
          | `Eof n -> handle_eof n
          | `Ok ->
            let pos_ref = ref 0 in
            let v =
              bin_prot_reader.Type_class.read t.bin_prot_buf ~pos_ref
            in
            if !pos_ref <> len then
              error "pos_ref <> len, (%d <> %d)" !pos_ref len ();
            Ivar.fill result (`Ok v))
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
  let rec loop () =
    read_one t
    >>> function
      | `Eof -> Pipe.close pipe_w
      | `Ok one ->
        if not (Pipe.is_closed pipe_w) then begin
          Pipe.write pipe_w one
          >>> fun () ->
          loop ()
        end
  in
  loop ();
  pipe_r
;;

let lines t = read_all t read_line

let contents t =
  let buf = Buffer.create 1024 in
  let sbuf = String.create 1024 in
  let res = Ivar.create () in
  let rec loop () =
    read t sbuf
    >>> function
      | `Ok l ->
        Buffer.add_substring buf sbuf 0 l;
        loop ()
      | `Eof -> Ivar.fill res (Buffer.contents buf)
  in
  loop ();
  Ivar.read res;
;;

let file_contents file = with_file file ~f:contents

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
        nonempty_buffer t (function
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

let use t =
  let error s = failwiths "can not read from reader" (s, t) <:sexp_of< string * t >> in
  match t.state with
  | `Closed -> error "closed"
  | `In_use -> error "in use"
  | `Not_in_use -> t.state <- `In_use
;;

let do_read t f =
  use t;
  f ()
  >>| fun x ->
  begin match t.state with
  | `Closed -> () (* [f ()] closed it.  Leave it closed. *)
  | `Not_in_use -> assert false  (* we're using it *)
  | `In_use -> t.state <- `Not_in_use
  end;
  x;
;;

(* The code below dynamically checks to make sure that there aren't simultaneous
   reads. *)
let read t ?pos ?len s = do_read t (fun () -> read t ?pos ?len s)
let read_char t = do_read t (fun () -> read_char t)
let read_substring t s = do_read t (fun () -> read_substring t s)
let read_bigsubstring t s = do_read t (fun () -> read_bigsubstring t s)
let really_read t ?pos ?len s = do_read t (fun () -> really_read t ?pos ?len s)
let really_read_substring t s = do_read t (fun () -> really_read_substring t s)
let really_read_bigsubstring t s =
  do_read t (fun () -> really_read_bigsubstring t s)
let read_until t f ~keep_delim =
  do_read t (fun () -> read_until t f ~keep_delim Fn.id)
let read_until_max t f ~keep_delim ~max =
  do_read t (fun () -> read_until_max t f ~keep_delim ~max)
let read_line t = do_read t (fun () -> read_line t)

let read_sexp ?parse_pos t = do_read t (fun () -> read_sexp ?parse_pos t)
let read_sexps ?parse_pos t = use t; read_sexps ?parse_pos t

let read_bin_prot ?max_len t reader =
  do_read t (fun () -> read_bin_prot ?max_len t reader)
let read_marshal_raw t = do_read t (fun () -> read_marshal_raw t)
let read_marshal t = do_read t (fun () -> read_marshal t)
let recv t = do_read t (fun () -> recv t)
let lines t = use t; lines t
let contents t = do_read t (fun () -> contents t)
let transfer t = use t; transfer t

let convert file f annot_sexp =
  try
    match Sexp.Annotated.conv f annot_sexp with
    | `Result a -> Ok a
    | `Error (exc, bad_annot_sexp) ->
      Error (Error.of_exn (Sexp.Annotated.get_conv_exn ~file ~exc bad_annot_sexp))
  with Of_sexp_error _ as exn ->
    Or_error.error "Reader.load_sexp(s) error" (file, exn) (<:sexp_of< string * exn >>)
;;

let gen_load ?exclusive file
    (good : Sexp.t list -> 'a Or_error.t)
    (bad : Sexp.Annotated.t list -> 'a Or_error.t) =
  let load parse f =
    Monitor.try_with
      (fun () ->
        with_file ?exclusive file ~f:(fun t ->
          Pipe.to_list (gen_read_sexps t parse)))
    >>= function
      | Ok sexps -> f sexps
      | Error exn ->
        return (Or_error.error "Reader.load_sexp(s) error" (file, exn)
                  (<:sexp_of< string * exn >>))
  in
  load Sexp.parse_bigstring (fun sexps ->
    try return (good sexps)
    with Of_sexp_error _ ->
      load Sexp.Annotated.parse_bigstring (fun sexps -> return (bad sexps)))
;;

let load_sexp ?exclusive file f =
  let multiple sexps =
    Or_error.error "Reader.load_sexp requires one sexp but got" (List.length sexps, file)
      (<:sexp_of< int * string >>)
  in
  gen_load ?exclusive file
    (fun sexps ->
      match sexps with
      | [sexp] -> Ok (f sexp)
      | _ -> multiple sexps)
    (fun annot_sexps ->
      match annot_sexps with
      | [annot_sexp] -> convert file f annot_sexp
      | _ -> multiple annot_sexps)
;;

let load_sexp_exn ?exclusive file f = load_sexp ?exclusive file f >>| Or_error.ok_exn

let load_sexps ?exclusive file f =
  gen_load ?exclusive file
    (fun sexps -> Ok (List.map sexps ~f))
    (fun annot_sexps ->
      let errors =
        List.filter_map annot_sexps ~f:(fun annot_sexp ->
          match convert file f annot_sexp with
          | Ok _ -> None
          | Error error -> Some error)
      in
      Or_error.error "Reader.load_sexps error" (file, errors)
        (<:sexp_of< string * Error.t list >>))
;;

let load_sexps_exn ?exclusive file f = load_sexps ?exclusive file f >>| Or_error.ok_exn

let pipe t =
  let pipe_r, pipe_w = Pipe.create () in
  upon (transfer t pipe_w) (fun () ->
    whenever (close t);
    Pipe.close pipe_w);
  pipe_r
;;
