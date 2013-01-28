open Core.Std
open Import

type t = Async_core.Block_group.t

val create :
  ?min_reserved_threads:int
  -> ?max_reserved_threads:int
  -> unit
  -> [ `Ok of t
     | `Out_of_threads
     ]
