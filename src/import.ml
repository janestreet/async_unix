open Core.Std

include Async_kernel.Std

module Debug = Async_kernel.Debug
module Job   = Async_kernel.Job

module Epoll_max_ready_events   = Config.Epoll_max_ready_events
module Max_inter_cycle_timeout  = Config.Max_inter_cycle_timeout
module Max_num_open_file_descrs = Config.Max_num_open_file_descrs
module Max_num_threads          = Config.Max_num_threads
module Min_inter_cycle_timeout  = Config.Min_inter_cycle_timeout

module Kernel_scheduler = Async_kernel.Scheduler
module File_descr       = Unix.File_descr
