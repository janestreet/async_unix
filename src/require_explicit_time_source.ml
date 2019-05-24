open! Core
open! Import
include Require_explicit_time_source_intf

include (
  From_kernel :
    module type of struct
    include From_kernel
  end
  with module Time_ns := From_kernel.Time_ns
   and module Time := From_kernel.Time
   and module Date := From_kernel.Date)

module Scheduler = Scheduler
module Time = Time
module Time_ns = Time_ns
module Clock = Clock
module Date = Date
