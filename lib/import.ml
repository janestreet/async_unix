open Core.Std

include Async_core.Import
include Async_core.Std

module Config         = Async_config
module Core_scheduler = Async_core.Scheduler
module Debug          = Async_core.Debug
module File_descr     = Unix.File_descr
