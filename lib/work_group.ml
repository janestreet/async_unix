open Core.Std
open Import

include Async_core.Work_group

module Scheduler = Raw_scheduler

let create ?min_assignable_threads ?max_assigned_threads () =
  let module S = Scheduler in
  let s = S.the_one_and_only ~should_lock:true in
  let finalize_work_group t =
    ok_exn (Thread_pool.finished_with_work_group s.S.thread_pool t)
  in
  Result.map
    (Thread_pool.create_work_group s.S.thread_pool
       ?min_assignable_threads ?max_assigned_threads)
    ~f:(fun t ->
      (* We use a finalizer so that when a work group becomes unreachable, we can free up
         its reserved threads. *)
      S.add_finalizer_exn s t finalize_work_group;
      t)
;;
