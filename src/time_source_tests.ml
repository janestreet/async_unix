open! Core.Std
open! Import

module Time_source = Async_kernel.Time_source
module Scheduler = Async_kernel.Scheduler

let run_cycles_until_no_jobs_remain = Scheduler.run_cycles_until_no_jobs_remain

let%bench_module "Clock.every" =
  (module struct

    let scheduler = Scheduler.t ()
    let time_source = scheduler.time_source

    let%bench "~continue-on-error:false" =
      let iv = Ivar.create () in
      let n = ref 0 in
      Time_source.run_repeatedly time_source
        ~stop:(Ivar.read iv)
        ~continue_on_error:false
        ~f:(fun () ->
          if !n >= 1_000
          then (Ivar.fill iv ())
          else (incr n);
          Deferred.unit)
        ~continue:Time_source.Continue.immediately;
      run_cycles_until_no_jobs_remain ();
    ;;

    let%bench "~continue_on_error:true" =
      let iv = Ivar.create () in
      let n = ref 0 in
      Time_source.run_repeatedly time_source
        ~stop:(Ivar.read iv)
        ~continue_on_error:true
        ~f:(fun () ->
          if !n >= 1_000
          then (Ivar.fill iv ())
          else (incr n);
          Deferred.unit)
        ~continue:Time_source.Continue.immediately;
        ();
      run_cycles_until_no_jobs_remain ();
    ;;
  end)
;;
