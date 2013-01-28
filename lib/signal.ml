open Core.Std
open Import

include Core.Std.Signal

module Scheduler = Raw_scheduler

let the_one_and_only = Scheduler.the_one_and_only

let handle ?stop ts ~f =
  let scheduler = the_one_and_only ~should_lock:true in
  let signal_manager = scheduler.Scheduler.signal_manager in
  let handler = Raw_signal_manager.install_handler signal_manager ts f in
  Option.iter stop ~f:(fun stop ->
    upon stop (fun () ->
      Raw_signal_manager.remove_handler signal_manager handler));
;;

let standard =
  (* Can't do [kill, stop] because it's not allowed to handle them.
     Don't do [segv, vtalrm] because they already have a handler.
     Don't do [fpe] because we want to hear about it.
     Don't do [prof] so that we can profile things with -p. *)
  [
    abrt; alrm; chld; cont; hup; int; quit; term; tstp; ttin; ttou;
    usr1; usr2;
  ]
;;
