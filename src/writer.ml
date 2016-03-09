open Core.Std
open Import

include Writer0

let of_pipe info pipe_w =
  Unix.pipe info
  >>| fun (`Reader reader_fd, `Writer writer_fd) ->
  let reader = Reader.create reader_fd in
  let writer = create writer_fd in
  if Debug.writer
  then Debug.log "Writer.of_pipe" (pipe_w, reader, writer)
         [%sexp_of: string Pipe.Writer.t * Reader.t * t];
  (* Shuttle bytes from [reader] to [pipe_w].  If the user calls [close writer],
     then [reader] will see EOF, which will cause [transfer] to complete.  If [pipe_w]
     is closed, then [transfer] will complete. *)
  let closed_and_flushed_downstream =
    Reader.transfer reader pipe_w
    >>= fun () ->
    if raise_when_consumer_leaves writer && not (is_closed writer)
    then Monitor.send_exn (monitor writer)
           (Unix.Unix_error (EPIPE, "Writer.of_pipe", ""));
    Deferred.both
      (Reader.close reader)
      (close writer)
    >>| fun ((), ()) ->
    if not (Pipe.is_closed pipe_w) then Pipe.close pipe_w
  in
  writer, `Closed_and_flushed_downstream closed_and_flushed_downstream
;;
