module Block_group = Block_group
module Fd          = Fd
module In_thread   = In_thread
module Io_stats    = Io_stats
module Print       = Async_print
module Reader      = Reader
module Scheduler   = Scheduler
module Shutdown    = Shutdown
module Socket      = Unix_syscalls.Socket
module Signal      = Signal
module Sys         = Async_sys
module Thread_safe = Thread_safe
module Writer      = Writer

module Unix = struct
  module Fd = Fd
  include Unix_syscalls
end

let schedule = Scheduler.schedule
let schedule' = Scheduler.schedule'
let shutdown = Shutdown.shutdown
let within = Scheduler.within
let within' = Scheduler.within'

(* We rebind all pervasive funtions that deal with I/O so that one doesn't
   actually do blocking stuff in an Async program. *)

include struct
  open Core.Std

  module Overwrite_ = struct
    let overwrite1 (`This_is_async__Think_about_blocking as x) = x
    let wrap f `This_is_async__Think_about_blocking = f
    let overwrite2 = wrap overwrite1
    let overwrite3 = wrap overwrite2
    let overwrite4 = wrap overwrite3
    let overwritef f = ksprintf (fun _ -> `This_is_async__Think_about_blocking) f
  end
  open Overwrite_

  let close_in_noerr = overwrite1
  let close_in = overwrite1
  let close_out_noerr = overwrite1
  let close_out = overwrite1
  let eprintf = Print.eprintf
  let flush_all = overwrite1
  let flush = overwrite1
  let fprintf = Print.fprintf
  let in_channel_length = overwrite1
  let input_binary_int = overwrite1
  let input_byte = overwrite1
  let input_char = overwrite1
  let input_line = overwrite1
  let input_lines ?fix_win_eol:_ = overwrite1
  let input = overwrite4
  let input_value = overwrite1
  let open_in_bin = overwrite1
  let open_in_gen = overwrite3
  let open_in = overwrite1
  let open_out_bin = overwrite1
  let open_out_gen = overwrite3
  let open_out = overwrite1
  let out_channel_length = overwrite1
  let output_binary_int = overwrite2
  let output_byte = overwrite2
  let output_char = overwrite2
  let output = overwrite4
  let output_string = overwrite2
  let output_value = overwrite2
  let pos_in = overwrite1
  let pos_out = overwrite1
  let prerr_char = Print.prerr_char
  let prerr_endline = Print.prerr_endline
  let prerr_float = Print.prerr_float
  let prerr_int = Print.prerr_int
  let prerr_newline = Print.prerr_newline
  let prerr_string = Print.prerr_string
  let print_char = Print.print_char
  let print_endline = Print.print_endline
  let print_float = Print.print_float
  let printf = Print.printf
  let print_int = Print.print_int
  let print_newline = Print.print_newline
  let print_string = Print.print_string
  let read_float = overwrite1
  let read_int = overwrite1
  let read_line = overwrite1
  let read_lines = overwrite1
  let read_wrap ?binary:_ ~f:_ = overwrite1
  let really_input = overwrite4
  let seek_in = overwrite2
  let seek_out = overwrite1
  let set_binary_mode_in = overwrite2
  let set_binary_mode_out = overwrite2
  let write_lines = overwrite2
  let write_wrap ?binary:_ ~f:_ = overwrite1

  module LargeFile = struct
    let seek_out = overwrite1
    let pos_out = overwrite1
    let out_channel_length = overwrite1
    let seek_in = overwrite1
    let pos_in = overwrite1
    let in_channel_length = overwrite1
  end
end

(* The standard [exit] function is problematic in Async programs, because it doesn't give
   us a chance to run Async at_exit handlers.  So, we break [exit] because people need to
   think about whether they want to use it or Async's [shutdown] function. *)
let exit (`This_is_async__Think_about_exit as x) = x
