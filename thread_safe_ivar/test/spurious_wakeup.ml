let%test_unit "spurious wakeups" =
  (* Arrange for an almost-guaranteed spurious wakeup by making the thread that waits on
     the ivar also responsible for handling the signal.

     There's still a small race here: if the signal is delivered before
     [Thread_safe_ivar.read] has a chance to run, then the signal will be processed
     without causing a spurious Condition.wait wakeup. Hence the tiny sleep to make
     that unlikely.

     There's another race where the signal handler can get scheduled to run in such a way
     that it doesn't wake up the [futex] call, but happens after the thread [t] checks for
     pending signals, so it never gets run. To resolve that, just send the signal in a
     loop until it works.
  *)
  let signal_handled = Thread_safe_ivar.create () in
  let main_ivar = Thread_safe_ivar.create () in
  let t = Caml_threads.Thread.create (fun () -> Thread_safe_ivar.read main_ivar) () in
  let _prev =
    (Sys.signal [@ocaml.alert "-unsafe_multidomain"])
      Sys.sigint
      (Sys.Signal_handle
         (fun _s ->
           assert (Thread.self () == t);
           match Thread_safe_ivar.peek signal_handled with
           | None -> Thread_safe_ivar.fill signal_handled ()
           | Some () -> ()))
  in
  let _previously_blocked = Caml_unix.sigprocmask SIG_BLOCK [ Sys.sigint ] in
  let rec signal_until_signal_handled () =
    match Thread_safe_ivar.peek signal_handled with
    | Some () -> ()
    | None ->
      UnixLabels.kill ~pid:(Caml_unix.getpid ()) ~signal:Sys.sigint;
      Unix.sleepf 1.0;
      signal_until_signal_handled ()
  in
  (* This sleep is to let the thread [t] get blocked on [Thread_safe_ivar.read],
     so there's something to wake up. *)
  Unix.sleepf 0.001;
  signal_until_signal_handled ();
  (* This sleep is to make sure that the wakeup is indeed spurious.
     (not sure it's necessary in practice, but seems necessary in theory) *)
  Unix.sleepf 0.001;
  Thread_safe_ivar.fill main_ivar ();
  Caml_threads.Thread.join t
;;
