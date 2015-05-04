open! Core.Std
open! Import

(* This variant mirrors an enum in the C, so order in this declaration matters. *)
type dump_type =
  | Call_abort
  | Call_gcore

external watch : float -> dump_type -> unit = "dump_core_on_job_delay_watch"
external tick  : unit -> unit               = "dump_core_on_job_delay_tick"

let choose_dump_type how_to_dump =
  match how_to_dump with
  | `Call_abort -> Call_abort
  | `Call_gcore -> Call_gcore
  | `Default ->
    match Core.Std.Sys.file_exists "/usr/bin/gcore" with
    | `Yes           -> Call_gcore
    | `No | `Unknown -> Call_abort
;;

let start_watching ~dump_if_delayed_by ~how_to_dump =
  let dump_type              = choose_dump_type how_to_dump in
  let dump_if_delayed_by_sec = Time.Span.to_sec dump_if_delayed_by in
  let tick_interval          = sec (dump_if_delayed_by_sec /. 10.) in
  ignore (Thread.create (fun () -> watch dump_if_delayed_by_sec dump_type) ());
  Clock.every tick_interval tick;
;;
