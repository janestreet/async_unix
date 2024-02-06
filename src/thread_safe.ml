open Core
open Import
open Raw_scheduler

let debug = Debug.thread_safe

let run_holding_async_lock
  (type a b)
  ?(wakeup_scheduler = true)
  t
  (f : unit -> a)
  ~(finish : (a, exn) Result.t -> b)
  : b
  =
  if debug then Debug.log "run_holding_async_lock" t [%sexp_of: t];
  if not (am_holding_lock t) then lock t;
  protect
    ~finally:(fun () ->
      if wakeup_scheduler then thread_safe_wakeup_scheduler t;
      unlock t)
    ~f:(fun () ->
      (* We run [f] within the [main_execution_context] so that any errors are sent to its
         monitor, rather than whatever random monitor happened to be in effect. *)
      finish
        (with_execution_context t Kernel_scheduler.main_execution_context ~f:(fun () ->
           Result.try_with f)))
;;

let ensure_in_a_thread t function_ =
  if is_main_thread ()
  then raise_s [%message "cannot call from the main thread" (function_ : string)];
  if am_holding_lock t
  then raise_s [%message "cannot call while holding the async lock" (function_ : string)]
;;

let run_in_async_with_optional_cycle ?wakeup_scheduler t f =
  if debug then Debug.log "run_in_async_with_optional_cycle" t [%sexp_of: t];
  ensure_in_a_thread t "run_in_async_with_optional_cycle";
  run_holding_async_lock ?wakeup_scheduler t f ~finish:(function
    | Error exn -> Error exn
    | Ok (maybe_run_a_cycle, a) ->
      (match maybe_run_a_cycle with
       | `Do_not_run_a_cycle -> ()
       | `Run_a_cycle -> have_lock_do_cycle t);
      Ok a)
;;

let with_async_lock t f =
  if am_holding_lock t
  then f ()
  else (
    lock t;
    protect ~f ~finally:(fun () -> unlock t))
;;

let without_async_lock t f =
  if am_holding_lock t
  then (
    unlock t;
    protect ~f ~finally:(fun () -> lock t))
  else f ()
;;

let ensure_the_scheduler_is_started t =
  if not (is_running t)
  then (
    let starting =
      (* Hold the lock when deciding if we're the first thread to start the scheduler. *)
      with_async_lock t (fun () ->
        if not (is_running t)
        then (
          t.start_type <- Called_block_on_async;
          let scheduler_ran_a_job = Thread_safe_ivar.create () in
          upon (return ()) (fun () -> Thread_safe_ivar.fill scheduler_ran_a_job ());
          `Yes scheduler_ran_a_job)
        else `No)
    in
    match starting with
    | `No -> ()
    | `Yes scheduler_ran_a_job ->
      (* Release the Async lock if necessary, so that the scheduler can acquire it. *)
      without_async_lock t (fun () ->
        ignore
          (Core_thread.create
             ~on_uncaught_exn:`Print_to_stderr
             (fun () ->
               Exn.handle_uncaught ~exit:true (fun () ->
                 let () =
                   match Linux_ext.pr_set_name_first16 with
                   | Ok f -> f "async-scheduler"
                   | Error _ -> ()
                 in
                 lock t;
                 never_returns (be_the_scheduler t)))
             ()
            : Core_thread.t);
        (* Block until the scheduler has run the above job. *)
        Thread_safe_ivar.read scheduler_ran_a_job))
;;

let block_on_async_not_holding_async_lock t f =
  (* Create a scheduler thread if the scheduler isn't already running. *)
  ensure_the_scheduler_is_started t;
  let maybe_blocked =
    run_holding_async_lock
      t
      (fun () -> Monitor.try_with ~run:`Schedule ~rest:`Log f ~name:"block_on_async")
      ~finish:(fun res ->
        match res with
        | Error exn -> `Available (Error exn)
        | Ok d ->
          (match Deferred.peek d with
           | Some v -> `Available v
           | None ->
             have_lock_do_cycle t;
             (match Deferred.peek d with
              | Some v -> `Available v
              | None ->
                let q = Squeue.create 1 in
                upon d (fun v -> Squeue.push_uncond q v);
                (* Squeue.pop can block, so we have to do it outside async *)
                `Blocked_wait_on_squeue q)))
  in
  match maybe_blocked with
  | `Available v -> v
  | `Blocked_wait_on_squeue q ->
    (* [run_holding_async_lock] released the lock.  If the scheduler wasn't already
       running when [block_on_async] was called, then we started it above.  So, the
       scheduler is running, and will eventually run the job to put something on the
       squeue.  So, it's OK to block waiting for it. *)
    Squeue.pop q
;;

let block_on_async t f =
  if debug then Debug.log "block_on_async" t [%sexp_of: t];
  (* We disallow calling [block_on_async] if the caller is running inside async.  This can
     happen if one is the scheduler, or if one is in some other thread that has used, e.g.
     [run_in_async] to call into async and run a cycle.  We do however, want to allow the
     main thread to call [block_on_async], in which case it should release the lock and
     allow the scheduler, which is running in another thread, to run. *)
  if i_am_the_scheduler t || (am_holding_lock t && not (is_main_thread ()))
  then raise_s [%message "called [block_on_async] from within async"];
  if not (am_holding_lock t)
  then block_on_async_not_holding_async_lock t f
  else (
    let execution_context =
      Kernel_scheduler.current_execution_context t.kernel_scheduler
    in
    unlock t;
    let res = block_on_async_not_holding_async_lock t f in
    (* If we're the main thread, we should lock the scheduler for the rest of main, to
       prevent the scheduler, which is now running in another thread, from interfering
       with the main thread.  We also restore the execution context, so that the code
       in the main thread will be in the same execution context as before it called
       [block_on_async].  The restored execution context will usually be
       [Execution_context.main], but need not be, if the user has done operations that
       adjust the current execution context, e.g. [Monitor.within]. *)
    lock t;
    Kernel_scheduler.set_execution_context t.kernel_scheduler execution_context;
    res)
;;

let block_on_async_exn t f = Result.ok_exn (block_on_async t f)

let run_in_async ?wakeup_scheduler t f =
  if debug then Debug.log "run_in_async" t [%sexp_of: t];
  ensure_in_a_thread t "run_in_async";
  run_holding_async_lock ?wakeup_scheduler t f ~finish:Fn.id
;;

let run_in_async_exn ?wakeup_scheduler t f =
  Result.ok_exn (run_in_async ?wakeup_scheduler t f)
;;

let run_in_async_wait t f =
  if debug then Debug.log "run_in_async_wait" t [%sexp_of: t];
  ensure_in_a_thread t "run_in_async_wait";
  block_on_async t f
;;

let run_in_async_wait_exn t f = Result.ok_exn (run_in_async_wait t f)

let deferred t =
  let ivar =
    if am_holding_lock t
    then Ivar.create ()
    else run_holding_async_lock t Ivar.create ~finish:Result.ok_exn
  in
  let fill x = run_in_async_exn t (fun () -> Ivar.fill_exn ivar x) in
  Ivar.read ivar, fill
;;

let t () = the_one_and_only ()
let am_holding_async_lock () = am_holding_lock (t ())
let deferred () = deferred (t ())

let run_in_async_with_optional_cycle ?wakeup_scheduler f =
  run_in_async_with_optional_cycle ?wakeup_scheduler (t ()) f
;;

let run_in_async ?wakeup_scheduler f = run_in_async ?wakeup_scheduler (t ()) f
let run_in_async_exn ?wakeup_scheduler f = run_in_async_exn ?wakeup_scheduler (t ()) f
let block_on_async f = block_on_async (t ()) f
let block_on_async_exn f = block_on_async_exn (t ()) f
let run_in_async_wait f = run_in_async_wait (t ()) f
let run_in_async_wait_exn f = run_in_async_wait_exn (t ()) f

let ok_to_drop_lock t =
  is_main_thread () && not (Kernel_scheduler.in_cycle t.kernel_scheduler)
;;

let without_async_lock_unchecked = without_async_lock

let without_async_lock f =
  let t = t () in
  if i_am_the_scheduler t || (am_holding_lock t && not (ok_to_drop_lock t))
  then
    raise_s
      [%sexp
        "called [become_helper_thread_and_block_on_async] from within async"
        , { i_am_the_scheduler = (i_am_the_scheduler t : bool)
          ; am_holding_lock = (am_holding_lock t : bool)
          ; ok_to_drop_lock = (ok_to_drop_lock t : bool)
          }]
  else without_async_lock t f
;;

module For_tests = struct
  let without_async_lock_unchecked f =
    let t = t () in
    without_async_lock_unchecked t f
  ;;
end
