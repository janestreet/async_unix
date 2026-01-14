(* Unit tests are in ../test/src/test_shutdown.ml *)

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

(* Be careful to ensure [shutdown] doesn't raise just because stderr is closed *)
let ignore_exn f =
  try f () with
  | _ -> ()
;;

let debug = Debug.shutdown

module Handler : sig
  type t

  val create : call_pos:Source_code_position.t -> f:(unit -> unit Deferred.t) -> t
  val run : t -> unit Or_error.t Deferred.t
  val state : t -> [ `Waiting | `Ran of unit Or_error.t Deferred.t ]
  val call_pos : t -> Source_code_position.t
end = struct
  module State = struct
    type t =
      | Waiting of (unit -> unit Deferred.t)
      | Ran of unit Or_error.t Deferred.t
  end

  type t =
    { call_pos : Source_code_position.t
    ; mutable state : State.t
    }

  let create ~call_pos ~f = { call_pos; state = Waiting f }
  let call_pos t = t.call_pos

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

  let state t =
    match t.state with
    | Waiting _ -> `Waiting
    | Ran result -> `Ran result
  ;;
end

let normal_handlers = Bag.create ()
let late_handlers = Bag.create ()

let at_shutdown_removable ~(here : [%call_pos]) bag f =
  if debug then Debug.log "at_shutdown" here [%sexp_of: Source_code_position.t];
  let handler = Handler.create ~call_pos:here ~f in
  Bag.add bag handler
;;

let at_shutdown ~(here : [%call_pos]) f =
  let _ : Handler.t Bag.Elt.t = at_shutdown_removable ~here normal_handlers f in
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
    Signal.Expert.set signal Default;
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
    don't_wait_for
      (let%bind normal_results =
         Deferred.all (Bag.to_list normal_handlers |> List.map ~f:Handler.run)
       in
       let%bind late_results =
         Deferred.all (Bag.to_list late_handlers |> List.map ~f:Handler.run)
       in
       match shutting_down () with
       | No -> assert false
       | Yes status ->
         let status =
           match Or_error.combine_errors_unit (normal_results @ late_results) with
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
                (List.filter_map
                   (Bag.to_list normal_handlers @ Bag.to_list late_handlers)
                   ~f:(fun handler ->
                     match Handler.state handler with
                     | `Waiting ->
                       (* added after shutdown started, so actually won't be waited for *)
                       None
                     | `Ran deferred ->
                       if Deferred.is_determined deferred
                       then None
                       else Some (Handler.call_pos handler))
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

let shutdown_on_unhandled_exn_logger = ref (fun ~msg:(_ : string) (_ : Exn.t) -> ())

let shutdown_on_unhandled_exn () =
  Monitor.detach_and_iter_errors Monitor.main ~f:(fun exn ->
    ignore_exn (fun () ->
      Debug.log "shutting down due to unhandled exception" exn [%sexp_of: exn]);
    ignore_exn (fun () ->
      !shutdown_on_unhandled_exn_logger
        ~msg:"shutting down due to unhandled exception"
        exn);
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
  let bag_elt = at_shutdown_removable ~here normal_handlers (fun () -> d) in
  upon d (fun () -> Bag.remove normal_handlers bag_elt)
;;

module Private = struct
  let run_after_other_shutdown_handlers ~(here : [%call_pos]) f =
    let _ : Handler.t Bag.Elt.t = at_shutdown_removable ~here late_handlers f in
    ()
  ;;

  let set_shutdown_on_unhandled_exn_logger f = shutdown_on_unhandled_exn_logger := f
end
