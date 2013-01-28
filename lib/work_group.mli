open Core.Std
open Import

type t = Async_core.Work_group.t

val create
  :  ?min_assignable_threads:int
  -> ?max_assigned_threads:int
  -> unit
  -> t Or_error.t
