(* Unit tests are in ../../lib_test/thread_safe_test.ml. *)

open Core.Std  let _ = _squelch_unused_module_warning_
open Import

type 'a t = 'a Pipe.Writer.t with sexp_of

let in_async ?wakeup_scheduler f = Thread_safe.run_in_async_exn ?wakeup_scheduler f

let in_async_wait f = Thread_safe.run_in_async_wait_exn f

let create () =
  if Thread_safe.am_holding_async_lock () then
    Pipe.create ()
  else
    in_async Pipe.create
;;

let pushback t = in_async_wait (fun () -> Pipe.pushback t)

let write' t q = in_async_wait (fun () -> Pipe.write' t q)
let write  t a = in_async_wait (fun () -> Pipe.write  t a)

let write_without_pushback' ?wakeup_scheduler t q =
  in_async ?wakeup_scheduler (fun () -> Pipe.write_without_pushback' t q)
;;

let write_without_pushback ?wakeup_scheduler t a =
  in_async ?wakeup_scheduler (fun () -> Pipe.write_without_pushback  t a)
;;

let close     t = in_async      (fun () -> Pipe.close     t)
let is_closed t = in_async      (fun () -> Pipe.is_closed t)
let closed    t = in_async_wait (fun () -> Pipe.closed    t)
