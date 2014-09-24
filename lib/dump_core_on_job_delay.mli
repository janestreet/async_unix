(** Dump core if jobs are delayed, to get additional debug information when running on
    UNIX systems that support core dumps.

    It is not normally enabled, but may be enabled for any program by setting the
    appropriate field, [dump_core_on_job_delay], in the [ASYNC_CONFIG] environment
    variable.
*)

open Core.Std
open Import

(** [start_watching] starts a regular async job (via [Clock.every]) that increments a
    counter, and a C thread to make sure that the counter is incremented in a timely
    manner. *)
val start_watching
  :  dump_if_delayed_by : Time.Span.t
  -> how_to_dump        : [ `Default | `Call_abort | `Call_gcore ]
  -> unit
