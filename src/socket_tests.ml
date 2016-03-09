open! Core.Std
open! Async_kernel.Std
open! Import
open! Std

open Socket

let%test_unit "accept interrupted by Fd.close" =
  Thread_safe.block_on_async_exn (fun () ->
    let test socket_type address =
      let t = create socket_type in
      bind t address
      >>= fun t ->
      let t = listen t in
      don't_wait_for (Clock.after (sec 0.1) >>= fun () -> Fd.close (fd t));
      Clock.with_timeout (sec 0.2) (accept t)
      >>| function
      | `Result (`Ok _) -> failwith "accepted an unexpected connection"
      | `Result `Socket_closed -> ()
      | `Timeout -> failwith "timed out despite closure of listening socket"
    in
    test Type.tcp (Address.Inet.create_bind_any ~port:0))
;;
