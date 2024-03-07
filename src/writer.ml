open Core
open Import
include Writer0
module Unix = Unix_syscalls

let of_pipe ?time_source info pipe_w =
  let%map `Reader reader_fd, `Writer writer_fd = Unix.pipe info in
  let reader = Reader.create reader_fd in
  let writer = create ?time_source writer_fd in
  if Debug.writer
  then
    Debug.log
      "Writer.of_pipe"
      (pipe_w, reader, writer)
      [%sexp_of: string Pipe.Writer.t * Reader.t * t];
  (* Shuttle bytes from [reader] to [pipe_w].  If the user calls [close writer],
     then [reader] will see EOF, which will cause [transfer] to complete.  If [pipe_w]
     is closed, then [transfer] will complete. *)
  let closed_and_flushed_downstream =
    let%bind () = Reader.transfer reader pipe_w in
    if raise_when_consumer_leaves writer && not (is_closed writer)
    then
      Monitor.send_exn
        (monitor writer)
        (Unix.Unix_error (EPIPE, "Writer.of_pipe", Sexp.to_string (Info.sexp_of_t info)));
    let%map (), () = Deferred.both (Reader.close reader) (close writer) in
    if not (Pipe.is_closed pipe_w) then Pipe.close pipe_w
  in
  writer, `Closed_and_flushed_downstream closed_and_flushed_downstream
;;

let splice_result t ~from =
  match%map
    Reader.read_one_chunk_at_a_time from ~handle_chunk:(fun buffer ~pos ~len ->
      schedule_bigstring t ~pos ~len buffer;
      match%map flushed_or_failed_with_result t with
      | Flushed (_ : Time_ns.t) -> `Continue
      | Error -> `Stop `Error
      | Consumer_left | Force_closed -> `Stop `Consumer_left)
  with
  | `Eof_with_unconsumed_data (_ : string) -> assert false
  (* unreachable because [handle_chunk] only returns [`Stop|`Continue] *)
  | `Eof -> `Ok
  | `Stopped result -> result
;;

let splice t ~from = splice_result t ~from |> Deferred.ignore_m
