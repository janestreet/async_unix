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
  [@@deriving equal, sexp_of]

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

let debug = Debug.shutdown
let todo = ref []

let at_shutdown f =
  let backtrace = Backtrace.get () in
  if debug then Debug.log "at_shutdown" backtrace [%sexp_of: Backtrace.t];
  todo := (backtrace, f) :: !todo
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

(* Be careful to ensure [shutdown] doesn't raise just because
   stderr is closed *)
let ignore_exn f =
  try f () with
  | _ -> ()
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
      (Deferred.all
         (List.map !todo ~f:(fun (backtrace, f) ->
            let%map result = Monitor.try_with_or_error ~rest:`Log f in
            (match result with
             | Ok () -> ()
             | Error error ->
               ignore_exn (fun () ->
                 Core.Debug.eprints
                   "at_shutdown function raised"
                   (error, backtrace)
                   [%sexp_of: Error.t * Backtrace.t]));
            if debug
            then
              ignore_exn (fun () ->
                Debug.log
                  "one at_shutdown function finished"
                  backtrace
                  [%sexp_of: Backtrace.t]);
            result)))
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
      ignore_exn (fun () -> Debug.log_string "Shutdown forced.");
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

let don't_finish_before =
  let proceed_with_shutdown = Ivar.create () in
  let num_waiting = ref 0 in
  let check () = if !num_waiting = 0 then Ivar.fill_exn proceed_with_shutdown () in
  at_shutdown (fun () ->
    check ();
    Ivar.read proceed_with_shutdown);
  fun d ->
    match shutting_down () with
    | Yes _ -> ()
    | No ->
      incr num_waiting;
      upon d (fun () ->
        decr num_waiting;
        match shutting_down () with
        | No -> ()
        | Yes _ -> check ())
;;
