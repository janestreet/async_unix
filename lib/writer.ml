open Core.Std
open Import

include Writer0

let of_pipe info pipe_w =
  Unix.pipe info
  >>| fun (`Reader reader_fd, `Writer writer_fd) ->
  let reader = Reader.create reader_fd in
  let writer = create writer_fd in
  if Debug.writer then
    Debug.log "Writer.of_pipe" (pipe_w, reader, writer)
      <:sexp_of< string Pipe.Writer.t * Reader.t * t >>;
  (* Shuttle bytes from [reader] to [pipe_w].  If the user calls [close writer],
     then [reader] will see EOF, which will cause [transfer] to complete.  If [pipe_w]
     is closed, then [transfer] will complete. *)
  let closed_and_flushed_downstream =
    Reader.transfer reader pipe_w
    >>= fun () ->
    if raise_when_consumer_leaves writer && not (is_closed writer) then
      Monitor.send_exn (monitor writer)
        (Unix.Unix_error (Unix.EPIPE, "Writer.of_pipe", ""));
    Deferred.both
      (Reader.close reader)
      (close writer)
    >>| fun ((), ()) ->
    if not (Pipe.is_closed pipe_w) then Pipe.close pipe_w
  in
  writer, `Closed_and_flushed_downstream closed_and_flushed_downstream
;;

TEST_MODULE = struct

  let test_async f =
    Thread_safe.block_on_async_exn (fun () ->
      Clock.with_timeout (sec 1.) (f ())
      >>| function `Result x -> x | `Timeout -> failwith "Timeout.")
  ;;

  TEST_UNIT "transfer" = test_async (fun () ->
    let pipe_r, pipe_w = Pipe.create () in
    let pipe_r = Pipe.map ~f:String.uppercase pipe_r in
    Deferred.both
      (Reader.of_pipe (Info.of_string "read end of pipe")  pipe_r)
      (of_pipe (Info.of_string "write end of pipe") pipe_w)
    >>= fun (reader, (writer, `Closed_and_flushed_downstream _upstream_closed)) ->
    write writer "abc";
    close writer
    >>= fun () ->
    Reader.contents reader
    >>| function
    | "ABC"  -> ()
    | output -> failwiths "Expected \"ABC\", got" output <:sexp_of< string >>)
  ;;

  TEST_UNIT "reader_pipe_closed" = test_async (fun () ->
    let pipe_r, pipe_w = Pipe.create () in
    Reader.of_pipe (Info.of_string "read end of pipe") pipe_r
    >>= fun reader ->
    Pipe.write_without_pushback pipe_w "ABC";
    Pipe.close pipe_w;
    Reader.contents reader
    >>| function
    | "ABC"  -> ()
    | output -> failwiths "Expected \"ABC\", got" output <:sexp_of< string >>)
  ;;

  TEST_UNIT "reader_fd_closed" = test_async (fun () ->
    let pipe_r, pipe_w = Pipe.create () in
    Reader.of_pipe (Info.of_string "read end of pipe") pipe_r
    >>= fun reader ->
    let buf = String.make 100_000 'A' in
    Pipe.write_without_pushback pipe_w buf;
    Reader.read reader buf
    >>= function
    | `Eof -> failwith "Couldn't read from the reader"
    | `Ok len when len = String.length buf ->
      failwith "Test failed, increase buffer size"
    | `Ok _ ->
      Reader.close reader
      >>= fun () ->
      (* The writer should get an EPIPE at this point, but that's ignored. *)
      Clock.after (sec 0.5))
  ;;

  TEST_UNIT "writer_pipe_closed" = test_async (fun () ->
    let pipe_r, pipe_w = Pipe.create () in
    of_pipe (Info.of_string "write end of pipe") pipe_w
    >>= fun (writer, `Closed_and_flushed_downstream _upstream_closed) ->
    Pipe.close pipe_w;
    Monitor.try_with (fun () ->
      Stream.iter (Monitor.detach_and_get_error_stream (monitor writer)) ~f:raise;
      write writer "ABC";
      Clock.after (sec 0.1)
      >>= fun () ->
      Pipe.read_all pipe_r
    )
    >>= function
    | Ok read ->
      failwiths "writer should have failed with EPIPE, but read" read
        <:sexp_of< string Queue.t >>
    | Error exn ->
      match Monitor.extract_exn exn with
      | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> begin
        close_finished writer
        >>= fun () ->
        Pipe.read_all pipe_r
        >>= fun read ->
        if Queue.is_empty read then
          return ()
        else
          failwiths "nothing should have been read, but got" read
            <:sexp_of< string Queue.t >>
      end
      | exn -> raise exn)
  ;;
end
