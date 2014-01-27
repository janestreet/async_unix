open Core.Std
open Import

open Raw_scheduler

module Priority = Linux_ext.Priority

module Helper_thread = struct
  include Thread_pool.Helper_thread

  let create ?priority ?name () =
    let t = the_one_and_only ~should_lock:true in
    let finalize_helper_thread helper_thread =
      Thread_pool.finished_with_helper_thread t.thread_pool helper_thread
    in
    Result.map (Thread_pool.create_helper_thread t.thread_pool ?name ?priority)
      ~f:(fun helper_thread ->
        add_finalizer_exn t helper_thread finalize_helper_thread;
        helper_thread);
  ;;
end

let run ?priority ?thread ?(when_finished = `Best) ?name f =
  let t = the_one_and_only ~should_lock:true in
  let doit () =
    Deferred.create (fun ivar ->
      let doit () =
        (* At this point, we are in a thread-pool thread, not the async thread. *)
        let result = Result.try_with f in
        let locked =
          match when_finished with
          | `Take_the_async_lock  -> lock t; true
          | `Notify_the_scheduler -> false
          | `Best                 -> try_lock t
        in
        if locked then
          protect ~finally:(fun () -> unlock t) ~f:(fun () ->
            Ivar.fill ivar result;
            have_lock_do_cycle t)
        else
          thread_safe_enqueue_external_action t (fun () -> Ivar.fill ivar result);
      in
      match thread with
      | None -> ok_exn (Thread_pool.add_work t.thread_pool doit ?name ?priority)
      | Some helper_thread ->
        ok_exn
          (Thread_pool.add_work_for_helper_thread
            t.thread_pool
            helper_thread
            doit
            ?name
            ?priority))
    >>| Result.ok_exn
  in
  if t.is_running then
    doit ()
  else
    (* We use [bind unit ...] to force calls to [run_no_exn] to wait until after the
       scheduler is started.  We do this because [run_no_exn] will cause things to run in
       other threads, and when a job is finished in another thread, it will try to acquire
       the async lock and manipulate async datastructures.  This seems hard to think about
       if async hasn't even started yet. *)
    Deferred.bind Deferred.unit doit
;;

let syscall     ~name f = run ~name (fun () ->                Syscall.syscall f )
let syscall_exn ~name f = run ~name (fun () -> Result.ok_exn (Syscall.syscall f))

let pipe_of_squeue sq =
  let (r, w) = Pipe.create () in
  (* The functions are defined to avoid unnecessary allocation. *)
  let pull () =
    let q = Linked_queue.create () in
    Squeue.transfer_queue sq q;
    q
  in
  let rec continue q =
    Linked_queue.iter q ~f:(Pipe.write_without_pushback w);
    Pipe.pushback w >>> loop
  (* [run pull] runs [pull] in a thread, because [Squeue.transfer_queue] can block. *)
  and loop () = run pull >>> continue in
  loop ();
  r
;;
