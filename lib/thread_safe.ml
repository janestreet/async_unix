open Core.Std
open Import

open Raw_scheduler

let run_holding_async_lock
    (type a) (type b) t (f : unit -> a) ~(finish : (a, exn) Result.t -> b) : b =
  if not (am_holding_lock t) then lock t;
  protect ~finally:(fun () -> unlock t) ~f:(fun () ->
    (* We run [f] within the [main_execution_context] so that any errors are sent to its
       monitor, rather than whatever random monitor happened to be in effect. *)
    finish
      (Core_scheduler.with_execution_context (Core_scheduler.main_execution_context ())
         ~f:(fun () -> Result.try_with f)));
;;

let run_a_cycle t =
  ignore (have_lock_do_cycle t : [ `Jobs_remain | `No_jobs_remain ]);
  (* Since we weren't running the cycle from the select loop, we need to interrupt
     it, so it can deal with any changes (unhandled exception, clock event, etc.). *)
  thread_safe_interrupt_select t;
;;

let run_in_async_with_optional_cycle t f =
  run_holding_async_lock t f
    ~finish:(function
    | Error exn -> Error exn
    | Ok (maybe_run_a_cycle, a) ->
      begin match maybe_run_a_cycle with
      | `Do_not_run_a_cycle -> ()
      | `Run_a_cycle -> run_a_cycle t
      end;
      Ok a)
;;

let block_on_async t f =
  (* We disallow calling [block_on_async] if the caller is running inside async.  This can
     happen if one is the select loop, or if one is in some other thread that has used,
     e.g.  [run_in_async] to call into async and run a cycle.  We do however, want to
     allow the main thread to call [block_on_async], in which case it should release the
     lock and allow the select loop running in another thread to run. *)
  if i_am_the_select_loop_thread t
    || (am_holding_lock t && not (is_main_thread ()))
  then failwith "called [block_on_async] from within async";
  (* Create a scheduler thread if the scheduler isn't already running. *)
  if not t.go_has_been_called then
    ignore (Thread.create
              (fun () ->
                Exn.handle_uncaught ~exit:true (fun () -> never_returns (go ())))
              ());
  let maybe_blocked =
    run_holding_async_lock t
      (fun () -> Monitor.try_with f ~name:"block_on_async")
      ~finish:(fun res ->
        run_a_cycle t;
        match res with
        | Ok d -> begin
          match Deferred.peek d with
          | Some v -> `Available v
          | None ->
            let q = Squeue.create 1 in
            upon d (fun v -> Squeue.push_uncond q v);
            (* Squeue.pop can block, so we have to do it outside async *)
            `Blocked_wait_on_squeue q
          end
        | Error exn -> `Available (Error exn))
  in
  let res =
    match maybe_blocked with
    | `Available v -> v
    | `Blocked_wait_on_squeue q ->
      (* [run_holding_async_lock] released the lock.  If the scheduler wasn't already
         running when [block_on_async] was called, then we started it above.  So, the
         scheduler is running, and will eventually run the job to put something on the
         squeue.  So, it's OK to block waiting for it. *)
      Squeue.pop q
  in
  (* If we're the main thread, then we've already created the one-and-only scheduler,
     so we should lock the scheduler for the rest of main, to prevent the select loop,
     which is now running in another thread, from interfering with the main thread. *)
  if is_main_thread () then lock t;
  res
;;

let block_on_async_exn t f = Result.ok_exn (block_on_async t f)

let ensure_in_a_thread t name =
  if is_main_thread () then failwithf "called %s from the main thread" name ();
  if am_holding_lock t then failwithf "called %s while holding the async lock" name ();
;;

let run_in_async t f =
  ensure_in_a_thread t "run_in_async";
  run_holding_async_lock t f ~finish:(fun res -> run_a_cycle t; res);
;;

let run_in_async_exn t f = Result.ok_exn (run_in_async t f)

let run_in_async_wait t f =
  ensure_in_a_thread t "run_in_async_wait";
  block_on_async t f;
;;

let run_in_async_wait_exn t f = Result.ok_exn (run_in_async_wait t f)

let pipe t =
  let pipe_reader, pipe_writer =
    if am_holding_lock t then
      Pipe.create ()
    else
      run_holding_async_lock t Pipe.create ~finish:Result.ok_exn
  in
  let write x = run_in_async_wait_exn t (fun () -> Pipe.write pipe_writer x) in
  let close () = run_in_async_exn t (fun () -> Pipe.close pipe_writer) in
  (pipe_reader, write, close)
;;

let deferred t =
  let ivar =
    if am_holding_lock t then
      Ivar.create ()
    else
      run_holding_async_lock t Ivar.create ~finish:Result.ok_exn
  in
  let fill x = run_in_async_exn t (fun () -> Ivar.fill ivar x) in
  (Ivar.read ivar, fill)
;;

let t () = the_one_and_only ~should_lock:false

let am_holding_async_lock () = am_holding_lock (t ())
let deferred () = deferred (t ())
let pipe () = pipe (t ())
let run_in_async_with_optional_cycle f = run_in_async_with_optional_cycle (t ()) f
let run_in_async f = run_in_async (t ()) f
let run_in_async_exn f = run_in_async_exn (t ()) f
let block_on_async f = block_on_async (t ()) f
let block_on_async_exn f = block_on_async_exn (t ()) f
let run_in_async_wait f = run_in_async_wait (t ()) f
let run_in_async_wait_exn f = run_in_async_wait_exn (t ()) f
