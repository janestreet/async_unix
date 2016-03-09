open! Core.Std
open! Import

module Time_source = Async_kernel.Time_source

let sec = Time_ns.Span.of_sec

let%test_module "" =
  (module struct

    let start_time = Time_ns.epoch
    let at_ = Time_ns.add start_time (sec 5.)

    let advance_by t span =
      Time_source.advance t ~to_:(Time_ns.add (Time_source.now t) span)
    ;;

    let%test_unit "start time" =
      let t = Time_source.create ~now:start_time () in
      assert (Time_ns.equal start_time (Time_source.now t))
    ;;

    let%test_unit "advance time" =
      Thread_safe.block_on_async_exn (fun () ->
        let t = Time_source.create ~now:start_time () in
        let span = sec 33. in
        advance_by t span;
        assert (Time_ns.equal (Time_ns.add start_time span) (Time_source.now t));
        return ())
    ;;

    let%test_unit "run_at" =
      Thread_safe.block_on_async_exn (fun () ->
        let t = Time_source.create ~now:start_time () in
        let ran = Ivar.create () in
        Time_source.run_at t at_ (fun () -> Ivar.fill ran ()) ();
        assert (Ivar.is_empty ran);
        advance_by t (sec 5.1);
        Ivar.read ran)
    ;;

    let%test_unit "at" =
      Thread_safe.block_on_async_exn (fun () ->
        let t = Time_source.create ~now:start_time () in
        (* add a small offset to [at_] to guarantee timers are triggered *)
        let span = sec 5.1 in
        let d = Time_source.at t at_ in
        advance_by t span;
        Clock_ns.with_timeout (sec 1.) d
        >>= function
        | `Timeout -> assert false
        | `Result () ->
          assert (Time_ns.equal (Time_ns.add start_time span) (Time_source.now t));
          return ())
    ;;

    let%test_unit "every" =
      Thread_safe.block_on_async_exn (fun () ->
        let t = Time_source.create ~now:start_time () in
        let span = sec 1. in
        let ivar = ref (Ivar.create ()) in
        Time_source.every t span (fun () ->
          Ivar.fill !ivar ();
          ivar := Ivar.create ());
        (* add a small offset to [span] to guarantee timers are triggered *)
        let span = Time_ns.Span.( + ) span (sec 0.1) in
        let rec run remaining =
          if remaining = 0
          then return ()
          else (
            let fired = Ivar.read !ivar in
            advance_by t span;
            Clock_ns.with_timeout (sec 1.) fired
            >>= function
            | `Timeout -> assert false
            | `Result () -> run (remaining - 1))
        in
        run 25)
    ;;

    let%test_unit "with_timeout doesn't clutter the async timing wheel" =
      Thread_safe.block_on_async_exn (fun () ->
        let scheduler = Async_kernel.Scheduler.t () in
        let time_source = scheduler.time_source in
        let events = time_source.events in
        let timing_wheel_length () = Timing_wheel_ns.length events in
        let length_before = timing_wheel_length () in
        let ivar = Ivar.create () in
        let d =
          Deferred.ignore
            (Time_source.with_timeout time_source Time_ns.Span.day (Ivar.read ivar))
        in
        assert (timing_wheel_length () > length_before);
        Ivar.fill ivar ();
        d >>| fun () ->
        assert (timing_wheel_length () <= length_before))
    ;;
  end)
;;

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
          then Ivar.fill iv ()
          else incr n;
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
          then Ivar.fill iv ()
          else incr n;
          Deferred.unit)
        ~continue:Time_source.Continue.immediately;
        ();
      run_cycles_until_no_jobs_remain ();
    ;;
  end)
;;
