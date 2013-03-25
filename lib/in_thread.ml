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

let run ?priority ?thread ?name f =
  let t = the_one_and_only ~should_lock:true in
  let doit () =
    Deferred.create (fun ivar ->
      let doit () =
        (* At this point, we are in a thread-pool thread, not the async thread. *)
        let result = Result.try_with f in
        with_lock t (fun () ->
          Ivar.fill ivar result;
          have_lock_do_cycle t);
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
  (* All the little definitions are to avoid unessary allocation, since it's possible this
     guy might be used to do something like consume a quotefeed. *)
  let (r, w) = Pipe.create () in
  let pull () =
    let q = Queue.create () in
    Squeue.transfer_queue sq q;
    q
  in
  let rec continue q = Pipe.write' w q >>> loop
  (* [run pull] runs [pull] in a thread, because [Squeue.transfer_queue] can block. *)
  and loop () = run pull >>> continue in
  loop ();
  r
;;
