let%test_unit "spurious wakeups" =
  (* Arrange for an almost-guaranteed spurious wakeup by making the thread that waits on
     the ivar also responsible for handling the signal.

     There's still a small race here: if the signal is delivered before
     [Thread_safe_ivar.read] has a chance to run, then the signal will be processed
     without causing a spurious Condition.wait wakeup. Hence the tiny sleep to make
     that unlikely.
  *)
  let signal_handled = Thread_safe_ivar.create () in
  let main_ivar = Thread_safe_ivar.create () in
  let t = Caml_threads.Thread.create (fun () -> Thread_safe_ivar.read main_ivar) () in
  let _prev =
    Sys.signal
      Sys.sigint
      (Sys.Signal_handle
         (fun _s ->
           assert (Thread.self () == t);
           Thread_safe_ivar.fill signal_handled ()))
  in
  let _previously_blocked = Caml_unix.sigprocmask SIG_BLOCK [ Sys.sigint ] in
  Unix.sleepf 0.001;
  UnixLabels.kill ~pid:(Caml_unix.getpid ()) ~signal:Sys.sigint;
  Thread_safe_ivar.read signal_handled;
  (* This sleep is to make sure that the wakeup is indeed spurious.
     (not sure it's necessary in practice, but seems necessary in theory) *)
  Unix.sleepf 0.001;
  Thread_safe_ivar.fill main_ivar ();
  Caml_threads.Thread.join t
;;
