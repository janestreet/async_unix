(* Unit tests are in ../../lib_test/shutdown_tests.ml *)

open Core
open Import
module Signal = Core.Signal

module Status_compatibility = struct
  type t =
    | Incompatible
    | Compatible_and_replace
    | Compatible_and_do_not_replace
end

module Status = struct
  type t =
    | Exit of int
    | Signal of Signal.t
  [@@deriving equal ~localize, sexp_of]

  let compatibility t ~prior : Status_compatibility.t =
    if equal t prior
    then Compatible_and_do_not_replace
    else (
      match prior, t with
      | _, Exit 0 -> Compatible_and_do_not_replace
      | Exit 0, _ -> Compatible_and_replace
      | _, _ -> Incompatible)
  ;;
end

module Maybe_status = struct
  type t =
    | No
    | Yes of Status.t
  [@@deriving sexp_of]
end

(* Be careful to ensure [shutdown] doesn't raise just because
   stderr is closed *)
let ignore_exn f =
  try f () with
  | _ -> ()
;;

let debug = Debug.shutdown

module Handler = struct
  module State = struct
    type t =
      | Waiting of (unit -> unit Deferred.t)
      | Ran of unit Or_error.t Deferred.t
  end

  type t =
    { call_pos : Source_code_position.t
    ; mutable state : State.t
    ; mutable bag_elt : t Bag.Elt.t option
    }

  let todo = Bag.create ()
  let create ~call_pos ~f = { call_pos; state = Waiting f; bag_elt = None }

  let forward_result deferred ivar =
    match Deferred.peek deferred with
    | Some res -> Ivar.fill_exn ivar res
    | None -> upon deferred (fun res -> Ivar.fill_exn ivar res)
  ;;

  let run t =
    let call_pos = t.call_pos in
    match t.state with
    | Ran _ -> raise_s [%sexp "at-shutdown handler forced twice"]
    | Waiting f ->
      let result_ivar = Ivar.create () in
      t.state <- Ran (Ivar.read result_ivar);
      let result = Monitor.try_with_or_error ~rest:`Log f in
      forward_result result result_ivar;
      let%map result = Ivar.read result_ivar in
      (match result with
       | Ok () -> ()
       | Error error ->
         ignore_exn (fun () ->
           Core.Debug.eprints
             "at_shutdown function raised"
             (error, call_pos)
             [%sexp_of: Error.t * Source_code_position.t]));
      if debug
      then
        ignore_exn (fun () ->
          Debug.log
            "one at_shutdown function finished"
            call_pos
            [%sexp_of: Source_code_position.t]);
      result
  ;;

  let remove t =
    Option.iter t.bag_elt ~f:(fun elt ->
      Bag.remove todo elt;
      t.bag_elt <- None);
    match t.state with
    | Waiting _ -> `Ok
    | Ran _ -> `Ran
  ;;
end

let todo = Handler.todo

let at_shutdown_removable ~(here : [%call_pos]) f =
  if debug then Debug.log "at_shutdown" here [%sexp_of: Source_code_position.t];
  let handler = Handler.create ~call_pos:here ~f in
  let bag_elt = Bag.add todo handler in
  handler.bag_elt <- Some bag_elt;
  handler
;;

let at_shutdown ~(here : [%call_pos]) f =
  let _ : Handler.t = at_shutdown_removable ~here f in
  ()
;;

let shutting_down_ref = ref Maybe_status.No
let default_force_ref = ref (fun () -> Clock.after (sec 10.))
let default_force () = !default_force_ref
let set_default_force force = default_force_ref := force
let shutting_down () = !shutting_down_ref

let is_shutting_down () =
  match shutting_down () with
  | No -> false
  | Yes _ -> true
;;

let exit_reliably status =
  match (status : Status.t) with
  | Exit code ->
    (match (exit code : Nothing.t) with
     | exception exn ->
       ignore_exn (fun () -> Core.Debug.eprints "Caml.exit raised" exn [%sexp_of: Exn.t]);
       Core_unix.exit_immediately (if code = 0 then 1 else code)
     | _ -> .)
  | Signal signal ->
    (match Stdlib.do_at_exit () with
     | exception exn ->
       ignore_exn (fun () -> Core.Debug.eprints "Caml.exit raised" exn [%sexp_of: Exn.t])
     | () -> ());
    Signal.Expert.set signal `Default;
    Signal_unix.send_exn signal (`Pid (Core_unix.getpid ()));
    ignore_exn (fun () ->
      Core.Debug.eprints
        "Signal_unix.send_exn failed to kill process"
        signal
        [%sexp_of: Signal.t]);
    Core_unix.exit_immediately 1
;;

let shutdown_with_status ?force status =
  if debug then ignore_exn (fun () -> Debug.log "shutdown" status [%sexp_of: Status.t]);
  match !shutting_down_ref with
  | Yes prior ->
    (match Status.compatibility status ~prior with
     | Incompatible ->
       raise_s
         [%message
           "shutdown with inconsistent status" (status : Status.t) (prior : Status.t)]
     | Compatible_and_replace -> shutting_down_ref := Yes status
     | Compatible_and_do_not_replace -> ())
  | No ->
    shutting_down_ref := Yes status;
    upon
      (Deferred.all (Bag.to_list todo |> List.map ~f:Handler.run))
      (fun results ->
        match shutting_down () with
        | No -> assert false
        | Yes status ->
          let status =
            match Or_error.combine_errors_unit results with
            | Ok () -> status
            | Error _ ->
              (match status with
               | Exit 0 -> Exit 1
               | _ -> status)
          in
          exit_reliably status);
    let force =
      match force with
      | None -> !default_force_ref ()
      | Some f -> f
    in
    upon force (fun () ->
      ignore_exn (fun () ->
        Debug.log
          "Shutdown forced."
          [%sexp
            { unfinished_handlers =
                (List.filter_map (Bag.to_list todo) ~f:(fun handler ->
                   match handler.state with
                   | Waiting _ ->
                     (* added after shutdown started, so actually won't be waited for *)
                     None
                   | Ran deferred ->
                     if Deferred.is_determined deferred
                     then None
                     else Some handler.call_pos)
                 : Source_code_position.t list)
            }]
          (fun s -> s));
      exit_reliably (Exit 1))
;;

let shutdown ?force exit_code = shutdown_with_status ?force (Exit exit_code)

let shutdown_with_signal_exn ?force signal =
  match Signal.default_sys_behavior signal with
  | `Terminate | `Dump_core -> shutdown_with_status ?force (Signal signal)
  | (`Stop | `Continue | `Ignore) as default_sys_behavior ->
    raise_s
      [%message
        "Shutdown.shutdown_with_signal_exn: not a terminating signal"
          (signal : Signal.t)
          (default_sys_behavior : [ `Stop | `Continue | `Ignore ])]
;;

let shutdown_on_unhandled_exn () =
  Monitor.detach_and_iter_errors Monitor.main ~f:(fun exn ->
    ignore_exn (fun () ->
      Debug.log "shutting down due to unhandled exception" exn [%sexp_of: exn]);
    try shutdown 1 with
    | _ ->
      (* The above [shutdown] call raises if we have already called shutdown with a
         different non-zero status. *)
      ())
;;

let exit ?force status =
  shutdown ?force status;
  Deferred.never ()
;;

let don't_finish_before ~(here : [%call_pos]) d =
  let handler = at_shutdown_removable ~here (fun _ -> d) in
  upon d (fun () ->
    let _ : [ `Ok | `Ran ] = Handler.remove handler in
    ())
;;
