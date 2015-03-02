open Core.Std

include Async_kernel.Import
include Async_kernel.Std

let sec = Core.Std.sec (* shadow [Async_kernel.Import.sec] *)

module Time_ns = Core.Std.Time_ns

module Kernel_scheduler = Async_kernel.Scheduler
module File_descr       = Unix.File_descr
