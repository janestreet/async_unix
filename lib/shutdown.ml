open Core.Std
open Import

let todo = ref []

let at_shutdown f = todo := f :: !todo

let shutting_down_ref = ref `No

let shutting_down () = !shutting_down_ref

let shutdown ?(force = after (sec 10.)) status =
  match !shutting_down_ref with
  | `Yes status' ->
      if status <> 0 && status' <> 0 && status <> status' then
        failwiths "shutdown with inconsistent status" (status, status')
          (<:sexp_of< int * int >>)
      else if status' = 0 && status <> 0 then
        shutting_down_ref := `Yes status
  | `No ->
    shutting_down_ref := `Yes status;
    upon (Deferred.all (List.map !todo ~f:(fun f -> f ()))) (fun _ ->
      match shutting_down () with
      | `No -> assert false
      | `Yes status -> exit status);
    upon force (fun () ->
      Core.Std.eprintf "Shutdown forced.\n";
      exit 1);
;;

let shutdown_and_raise ?force status =
  shutdown ?force status;
  raise Async_core.Raw_monitor.Shutdown;
;;
