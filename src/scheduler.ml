(* [Raw_scheduler] is distinct from [Scheduler], because the former exposes some things
   that are used internally within Async that are not exposed in scheduler.mli.
   Also, it breaks a cyclic dependency [Raw_scheduler -> Log -> Scheduler]. *)

open! Core.Std
open! Import

include Raw_scheduler

(* We want to get a log of errors sent to [Monitor.try_with] after the initial return, so
   on initialization we redirect them to [Log.Global.error].  However, logging errors
   isn't cheap and there are issues with thread fairness when outputting to stderr (which
   is the default in many cases for [Global.error]), so, to prevent the [Log] [Writer.t]
   buffer from growing without bound, we limit the number of currently unflushed error
   messages created by [try_with_log_exn]. *)
let try_with_log_exn =
  let max_unflushed_errors     = 10 in
  let current_unflushed_errors = ref 0 in
  fun exn ->
    if !current_unflushed_errors < max_unflushed_errors
    then begin
      incr current_unflushed_errors;
      Log.Global.error !"\
Exception raised to Monitor.try_with that already returned
  (this error was captured by a default handler in Async.Log):
  %{Exn}"
        exn;
      if !current_unflushed_errors = max_unflushed_errors
      then Log.Global.error "\
Stopped logging exceptions raised to Monitor.try_with that already returned
  until error log can be flushed.";
      upon (Log.Global.flushed ()) (fun () -> decr current_unflushed_errors);
    end
;;

let go      = unstage (go      ~try_with_log_exn)
let go_main = unstage (go_main ~try_with_log_exn)

let time_spent_waiting_for_io () =
  let t = t () in
  t.time_spent_waiting_for_io
  |> Tsc.Span.to_ns
  |> Time_ns.Span.of_int63_ns
;;

let set_min_inter_cycle_timeout min_inter_cycle_timeout =
  let t = t () in
  if Time_ns.Span.( > )
       min_inter_cycle_timeout
       (t.max_inter_cycle_timeout :> Time_ns.Span.t)
  then Error.raise
         ([%message "min_inter_cycle_timeout too large"
                   (min_inter_cycle_timeout : Time_ns.Span.t)
                   (t.max_inter_cycle_timeout : Max_inter_cycle_timeout.t)]
          |> [%of_sexp: Error.t]);
  t.min_inter_cycle_timeout <- Min_inter_cycle_timeout.create_exn min_inter_cycle_timeout;
;;

let max_num_threads () = max_num_threads (t ())
