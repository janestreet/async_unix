open Core.Std

include Async_kernel.Import
include Async_kernel.Std

module Config           = Async_config
module Kernel_scheduler = Async_kernel.Scheduler
module File_descr       = Unix.File_descr
