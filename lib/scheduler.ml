(* [Raw_scheduler] is distinct from [Scheduler], because the former exposes some things
   that are used internally within Async that are not exposed in scheduler.mli. *)
include Raw_scheduler
