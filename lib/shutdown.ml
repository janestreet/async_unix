(* Unit tests are in ../../lib_test/shutdown_tests.ml *)

open Core.Std
open Import

let debug = Debug.shutdown

let todo = ref []

let at_shutdown f =
  let backtrace = Backtrace.get_opt () in
  if debug then Debug.log "at_shutdown" backtrace <:sexp_of< Backtrace.t option >>;
  todo := (backtrace, f) :: !todo;
;;

let shutting_down_ref = ref `No

let shutting_down () = !shutting_down_ref

let shutdown ?(force = after (sec 10.)) status =
  if debug then Debug.log "shutdown" status <:sexp_of< int >>;
  match !shutting_down_ref with
  | `Yes status' ->
    if status <> 0 && status' <> 0 && status <> status' then
      failwiths "shutdown with inconsistent status" (status, status')
        (<:sexp_of< int * int >>)
    else if status' = 0 && status <> 0 then
      shutting_down_ref := `Yes status
  | `No ->
    shutting_down_ref := `Yes status;
    upon (Deferred.all
            (List.map !todo ~f:(fun (backtrace, f) ->
               f ()
               >>| fun () ->
               if debug then
                 Debug.log "one at_shutdown function finished" backtrace
                   <:sexp_of< Backtrace.t option >>)))
      (fun _ ->
         match shutting_down () with
         | `No -> assert false
         | `Yes status -> exit status);
    upon force (fun () ->
      Debug.log_string "Shutdown forced.";
      exit 1);
;;

let exit ?force status = shutdown ?force status; Deferred.never ()

let don't_finish_before =
  let proceed_with_shutdown = Ivar.create () in
  let num_waiting = ref 0 in
  let check () = if !num_waiting = 0 then Ivar.fill proceed_with_shutdown () in
  at_shutdown (fun () -> check (); Ivar.read proceed_with_shutdown);
  fun d ->
    incr num_waiting;
    upon d (fun () ->
      decr num_waiting;
      match shutting_down () with
      | `No -> ()
      | `Yes _ -> check ())
;;
