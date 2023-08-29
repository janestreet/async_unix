open Core
open Import
include Core.Signal

let handle_default `Do_not_use_with_async = assert false
let ignore `Do_not_use_with_async = assert false

module Scheduler = Raw_scheduler

let the_one_and_only = Scheduler.the_one_and_only

let handle ?stop ts ~f =
  let scheduler = the_one_and_only () in
  let signal_manager = scheduler.signal_manager in
  let handler =
    Signal_manager.install_handler
      signal_manager
      ts
      (unstage (Scheduler.preserve_execution_context f))
  in
  Option.iter stop ~f:(fun stop ->
    upon stop (fun () -> Signal_manager.remove_handler signal_manager handler))
;;

let terminating = [ alrm; hup; int; term; usr1; usr2 ]

let manage_by_async ts =
  let scheduler = the_one_and_only () in
  let signal_manager = scheduler.signal_manager in
  List.iter ts ~f:(fun t ->
    Signal_manager.manage_but_keep_default_behavior signal_manager t)
;;

let is_managed_by_async t =
  let scheduler = the_one_and_only () in
  let signal_manager = scheduler.signal_manager in
  Signal_manager.is_managing signal_manager t
;;
