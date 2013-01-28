(* This file exists because 3.12.0 camlp4 doesn't support "module type of".  So, we
   put all the places we would like to use that in here, and don't preprocess this file.
   Once we switch to 3.12.1, we can delete this file and put the uses where they belong. *)


open Core.Std

module type Exit                   = module type of Unix.Exit
module type Exit_or_signal         = module type of Unix.Exit_or_signal
module type Exit_or_signal_or_stop = module type of Unix.Exit_or_signal_or_stop
module type Signal                 = module type of Core.Std.Signal
