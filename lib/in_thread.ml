open Core.Std
open Import

open Raw_scheduler

let delta_reserved_threads t bg i =
  if debug then Debug.log "delta_reserved_threads" i <:sexp_of< int >>;
  t.num_reserved_threads <- t.num_reserved_threads + i;
  bg.Block_group.num_reserved_threads <- bg.Block_group.num_reserved_threads + i;
;;

let delta_blocked_threads t bg i =
  t.num_blocked_threads <- t.num_blocked_threads + i;
  bg.Block_group.num_blocked_threads <- bg.Block_group.num_blocked_threads + i;
;;

let get_thread t bg =
  if bg.Block_group.num_blocked_threads < bg.Block_group.num_reserved_threads then begin
    delta_blocked_threads t bg 1;
    `Ok
  end else if (t.num_reserved_threads < t.max_num_live_threads
                && (bg.Block_group.num_reserved_threads
                    < bg.Block_group.max_reserved_threads)) then begin
    delta_reserved_threads t bg 1;
    delta_blocked_threads t bg 1;
    `Ok;
  end else
      `Out_of_threads
;;

let finish_with_thread t bg =
  delta_blocked_threads t bg (-1);
  (* If we don't need to reserve this thread, then unreserve it. *)
  if bg.Block_group.num_reserved_threads > bg.Block_group.min_reserved_threads
  then delta_reserved_threads t bg (-1);
;;

module Helper_thread = struct
  type t =
    { work_queue : Work.t Squeue.t;
    }

  let create ?name_first16 () =
    let t = the_one_and_only ~should_lock:true in
    let execution_context = Core_scheduler.current_execution_context () in
    let block_group = execution_context.Execution_context.block_group in
    if t.num_live_threads = t.max_num_live_threads then
      `Out_of_threads
    else
      match get_thread t block_group with
      | `Out_of_threads as x -> x
      | `Ok ->
        let work_queue = Squeue.create 10 in
        create_thread t work_queue ?default_thread_name_first16:name_first16;
        let finished () =
          finish_with_thread t block_group;
          Squeue.push_uncond work_queue
            { Work.
              set_thread_name_to = None;
              doit = (fun () -> `Stop);
            }
        in
        let helper_thread = { work_queue } in
        finalize t (fun _ -> finished ()) helper_thread;
        `Ok helper_thread;
  ;;
end

let run ?thread ?name_first16 f =
  let t = the_one_and_only ~should_lock:true in
  let doit () =
    let execution_context = Core_scheduler.current_execution_context () in
    let block_group = execution_context.Execution_context.block_group in
    Deferred.create (fun ivar ->
      let add_work_for_threads w = Squeue.push_uncond t.work_for_threads w in
      let doit () =
        let result = Result.try_with f in
        with_lock t (fun () ->
          if is_none thread then begin
            match Queue.dequeue block_group.Block_group.work with
            | Some w -> add_work_for_threads w
            | None -> finish_with_thread t block_group
          end;
          Ivar.fill ivar result;
          (* It's OK to ignore remaining jobs, because we interrupt the select loop, which
             will run them. *)
          ignore (have_lock_do_cycle t : [ `Jobs_remain | `No_jobs_remain ]);
          thread_safe_interrupt_select t);
        `Continue;
      in
      let work = { Work. doit; set_thread_name_to = name_first16 } in
      match thread with
      | Some helper_thread ->
        Squeue.push_uncond helper_thread.Helper_thread.work_queue work
      | None ->
        match get_thread t block_group with
        | `Out_of_threads ->
          Queue.enqueue block_group.Block_group.work work
        | `Ok ->
          if t.num_blocked_threads > t.num_live_threads then
            create_thread t t.work_for_threads;
          add_work_for_threads work)
    >>| Result.ok_exn
  in
  if t.go_has_been_called then
    doit ()
  else
    (* We use [bind unit ...] to force calls to [run_in_thread] to wait until after the
       scheduler is started.  We do this because [run_in_thread] will cause things to run
       in other threads, and when a job is finished in another thread, it will try to
       acquire the async lock and manipulate async datastructures.  This seems hard to
       think about if async hasn't even started yet. *)
    Deferred.bind Deferred.unit doit
;;

let syscall     f = run (fun () ->                Syscall.syscall f )
let syscall_exn f = run (fun () -> Result.ok_exn (Syscall.syscall f))

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
