open! Core.Std
open! Async_kernel.Std
open! Import
open! Std

open Writer

let%test_module _ =
  (module struct

    let test_async f =
      Thread_safe.block_on_async_exn (fun () ->
        Clock.with_timeout (sec 1.) (f ())
        >>| function `Result x -> x | `Timeout -> failwith "Timeout.")
    ;;

    let%test_unit "transfer" = test_async (fun () ->
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
      | output -> failwiths "Expected \"ABC\", got" output [%sexp_of: string])
    ;;

    let%test_unit "transfer closes pipe when consumer leaves" = test_async (fun () ->
      let pipe_r, _pipe_w = Pipe.create () in
      Unix.pipe (Info.of_string "test pipe")
      >>= fun (`Reader reader_fd, `Writer writer_fd) ->
      let writer = create ~raise_when_consumer_leaves:false writer_fd in
      let pending_transfer = transfer writer pipe_r ignore in
      Fd.close reader_fd
      >>= fun () ->
      write writer "foo";
      consumer_left writer
      >>= fun () ->
      pending_transfer
      >>= fun () ->
      [%test_result: bool] ~expect:true (Pipe.is_closed pipe_r);
      close writer)
    ;;

    let%test_unit "transfer closes pipe when writer is closed" = test_async (fun () ->
      let pipe_r, _pipe_w = Pipe.create () in
      Unix.pipe (Info.of_string "test pipe")
      >>= fun (`Reader reader_fd, `Writer writer_fd) ->
      let writer = create ~raise_when_consumer_leaves:false writer_fd in
      let pending_transfer = transfer writer pipe_r ignore in
      Fd.close reader_fd
      >>= fun () ->
      close writer
      >>= fun () ->
      pending_transfer
      >>= fun () ->
      [%test_result: bool] ~expect:true (Pipe.is_closed pipe_r);
      close writer)
    ;;

    let%test_unit "reader_pipe_closed" = test_async (fun () ->
      let pipe_r, pipe_w = Pipe.create () in
      Reader.of_pipe (Info.of_string "read end of pipe") pipe_r
      >>= fun reader ->
      Pipe.write_without_pushback pipe_w "ABC";
      Pipe.close pipe_w;
      Reader.contents reader
      >>| function
      | "ABC"  -> ()
      | output -> failwiths "Expected \"ABC\", got" output [%sexp_of: string])
    ;;

    let%test_unit "reader_fd_closed" = test_async (fun () ->
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

    let%test_unit "writer_pipe_closed" = test_async (fun () ->
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
          [%sexp_of: string Queue.t]
      | Error exn ->
        match Monitor.extract_exn exn with
        | Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> begin
            close_finished writer
            >>= fun () ->
            Pipe.read_all pipe_r
            >>= fun read ->
            if Queue.is_empty read
            then return ()
            else failwiths "nothing should have been read, but got" read
                   [%sexp_of: string Queue.t]
          end
        | exn -> raise exn)
    ;;

    let%test_unit "schedule_iobuf_peek" = test_async (fun () ->
      let pipe_r, pipe_w = Pipe.create () in
      of_pipe (Info.of_string "write end of pipe") pipe_w
      >>= fun (writer, `Closed_and_flushed_downstream _upstream_closed) ->
      let iobuf = Iobuf.of_string "0123456789" in
      schedule_iobuf_peek writer iobuf ~pos:3 ~len:4;
      close writer
      >>= fun () ->
      Pipe.to_list pipe_r
      >>| fun strs ->
      [%test_result: string] (String.concat strs) ~expect:"3456";
      [%test_result: string] (Iobuf.to_string iobuf) ~expect:"0123456789")
    ;;

    let%test_unit "schedule_iobuf_consume" = test_async (fun () ->
      let pipe_r, pipe_w = Pipe.create () in
      of_pipe (Info.of_string "write end of pipe") pipe_w
      >>= fun (writer, `Closed_and_flushed_downstream _upstream_closed) ->
      let iobuf = Iobuf.of_string "0123456789" in
      schedule_iobuf_consume writer iobuf ~len:7
      >>= fun () ->
      close writer
      >>= fun () ->
      Pipe.to_list pipe_r
      >>| fun strs ->
      [%test_result: string] (String.concat strs) ~expect:"0123456";
      [%test_result: string] (Iobuf.to_string iobuf) ~expect:"789")
    ;;

    let%test_unit "consumer_left after partial write" = test_async (fun () ->
      Unix.pipe (Info.of_string "test pipe")
      >>= fun (`Reader fd_r, `Writer fd_w) ->
      let r = Reader.create fd_r in
      let w = create fd_w ~raise_when_consumer_leaves:false in
      write w (String.make 1_000_000 '_');
      let read_loop =
        Reader.read_one_iobuf_at_a_time r ~handle_chunk:(fun _ ->
          Deferred.return (`Stop ()))
      in
      read_loop
      >>= function
      | `Eof | `Eof_with_unconsumed_data _ ->
        assert false
      | `Stopped () ->
        Reader.close r
        >>= fun () ->
        consumer_left w)
    ;;
  end)
;;
