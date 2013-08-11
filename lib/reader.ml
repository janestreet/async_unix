open Core.Std
open Import

include Reader0

module Writer = Writer0

let of_pipe info pipe_r =
  Unix.pipe info
  >>| fun (`Reader reader_fd, `Writer writer_fd) ->
  let reader = create reader_fd in
  let writer =
    Writer.create
      ~buffer_age_limit:`Unlimited
      ~raise_when_consumer_leaves:false
      writer_fd
  in
  if false then
    Debug.log "Reader.of_pipe" (pipe_r, reader, writer)
      <:sexp_of< string Pipe.Reader.t * t * Writer.t >>;
  don't_wait_for (
    Writer.transfer writer pipe_r ~stop:(close_finished reader)
      (fun s -> Writer.write writer s)
    >>= fun () ->
    Writer.close writer);
  reader
;;
