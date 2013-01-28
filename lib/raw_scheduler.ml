open Core.Std
open Import

module Core_scheduler = Async_core.Scheduler
module Fd = Raw_fd
module Watching = Fd.Watching
module Signal = Core.Std.Signal

let debug = Debug.scheduler

module type File_descr_watcher = File_descr_watcher_intf.S

let file_descr_watcher =
  let module F = Config.File_descr_watcher in
  match Config.file_descr_watcher with
  | F.Select -> (module Select_file_descr_watcher : File_descr_watcher)
  | F.Epoll  -> (module Epoll_file_descr_watcher  : File_descr_watcher)
;;

module File_descr_watcher = (val file_descr_watcher : File_descr_watcher)

type 'a with_options = 'a Core_scheduler.with_options

include struct
  open Core_scheduler

  let schedule = schedule
  let schedule' = schedule'
  let within = within
  let within' = within'
  let within_context = within_context
  let within_v = within_v
end

let cycle_count () = Core_scheduler.(cycle_count (t ()))
let cycle_start () = Core_scheduler.(cycle_start (t ()))
let cycle_times () = Core_scheduler.(cycle_times (t ()))
let set_max_num_jobs_per_priority_per_cycle i =
  Core_scheduler.(set_max_num_jobs_per_priority_per_cycle (t ())) i
;;

type finalizer_job = Execution_context.t * (unit -> unit)

type t =
  { (* The scheduler [mutex] must be locked by all code that is manipulating scheduler
       data structures, which is almost all async code.  The [mutex] is automatically
       locked in the main thread when the scheduler is first created.  A [Nano_mutex]
       keeps track of which thread is holding the lock.  This means we can detect errors
       in which code incorrectly accesses async from a thread not holding the lock.  We do
       this when [detect_invalid_access_from_thread = true].  We also detect errors in
       which code tries to acquire the async lock while it already holds it, or releases
       the lock when it doesn't hold it. *)
    mutex : Nano_mutex.t;

    mutable is_running : bool;

    (* [fds_whose_watching_has_changed] holds all fds whose watching has changed since
       the last time their desired state was set in the [file_descr_watcher]. *)
    mutable fds_whose_watching_has_changed : Fd.t list;
    file_descr_watcher : File_descr_watcher.t;

    (* [fd_by_descr] holds every file descriptor that Async knows about.  Fds are added
       when they are created, and removed when they transition to [Closed]. *)
    fd_by_descr : Fd_by_descr.t;

    (* A distinguished thread, called the "scheduler" thread, is continually looping,
       checking file descriptors for I/O and then running a cycle.  It manages
       the [file_descr_watcher], runs finalizers, runs signal handlers.

       [scheduler_thread_id] is mutable because we create the scheduler before starting
       the scheduler running.  Once we start running the scheduler, [scheduler_thread_id]
       is set and never changes again. *)
    mutable scheduler_thread_id : int;

    (* The [interruptor] is used to wake up the scheduler when it is blocked on the file
       descriptor watcher. *)
    interruptor : Interruptor.t;

    signal_manager : Raw_signal_manager.t;

    (* Finalizers are very much like signals; they can come at any time and in any thread.
       When an OCaml finalizer fires, we stick a closure to do the work in a thread-safe
       queue of [finalizer_jobs], which the scheduler then schedules to run as an ordinary
       async job. *)
    finalizer_jobs : finalizer_job Thread_safe_queue.t sexp_opaque;

    (* The [thread_pool] is used for making blocking system calls in threads other than
       the scheduler thread, and for servicing [In_thread.run] requests. *)
    thread_pool : Thread_pool.t;

    core_scheduler : Core_scheduler.t;
  }
with fields, sexp_of

let current_execution_context t =
  Core_scheduler.current_execution_context t.core_scheduler;
;;

let with_execution_context t context ~f =
  Core_scheduler.with_execution_context t.core_scheduler context ~f;
;;

let create_fd t kind file_descr info =
  let fd = Fd.create kind file_descr info in
  (* Async is confused if the OS returns a file descriptor that async thinks is still
     in use.  The [add_exn] raises in this case. *)
  Fd_by_descr.add_exn t.fd_by_descr fd;
  fd
;;

let lock t =
  (* The following debug message is outside the lock, and so there can be races between
     multiple threads printing this message. *)
  if debug then Debug.log_string "waiting on lock";
  Nano_mutex.lock_exn t.mutex;
;;

let try_lock t =
  match Nano_mutex.try_lock_exn t.mutex with
  | `Acquired -> true
  | `Not_acquired -> false
;;

let unlock t =
  if debug then Debug.log_string "lock released";
  Nano_mutex.unlock_exn t.mutex;
;;

let with_lock t f =
  lock t;
  protect ~f ~finally:(fun () -> unlock t);
;;

let am_holding_lock t = Nano_mutex.current_thread_has_lock t.mutex

type the_one_and_only =
| Not_ready_to_initialize
| Ready_to_initialize of (unit -> t)
| Initialized of t

(* We use a mutex to protect creation of the one-and-only scheduler in the event that
   multiple threads attempt to call [the_one_and_only] simultaneously, which can
   happen in programs that are using [Thread_safe.run_in_async]. *)
let mutex_for_initializing_the_one_and_only_ref = Nano_mutex.create ()
let the_one_and_only_ref : the_one_and_only ref = ref Not_ready_to_initialize

let is_ready_to_initialize () =
  match !the_one_and_only_ref with
  | Not_ready_to_initialize | Initialized _ -> false
  | Ready_to_initialize _ -> true
;;

(* Handling the uncommon cases in this function allows [the_one_and_only] to be inlined.
   The presence of a string constant keeps this function from being inlined. *)
let the_one_and_only_uncommon_case ~should_lock =
  Nano_mutex.critical_section mutex_for_initializing_the_one_and_only_ref ~f:(fun () ->
    match !the_one_and_only_ref with
    | Initialized t -> t
    | Not_ready_to_initialize -> failwith "Async the_one_and_only not ready to initialize"
    | Ready_to_initialize f ->
      let t = f () in
      (* We supply [~should_lock:true] to lock the scheduler when the user does async
         stuff at the top level before calling [Scheduler.go], because we don't want
         anyone to be able to run jobs until [Scheduler.go] is called.  This could happen,
         e.g. by creating a reader that does a read system call in another (true) thread.
         The scheduler will remain locked until the scheduler unlocks it. *)
      if should_lock then lock t;
      the_one_and_only_ref := Initialized t;
      t)
;;

let the_one_and_only ~should_lock =
  match !the_one_and_only_ref with
  | Initialized t -> t
  | Not_ready_to_initialize | Ready_to_initialize _ ->
    the_one_and_only_uncommon_case ~should_lock
;;

let current_thread_id () = Core.Std.Thread.(id (self ()))

let is_main_thread () = current_thread_id () = 0

let remove_fd t fd = Fd_by_descr.remove t.fd_by_descr fd

let maybe_start_closing_fd t fd =
  if fd.Fd.num_active_syscalls = 0 then begin
    let module S = Fd.State in
    match fd.Fd.state with
    | S.Closed | S.Open -> ()
    | S.Close_requested close ->
      (* We must remove the fd now and not after the close has finished.  If we waited
         until after the close had finished, then the fd might have already been
         reused by the OS and replaced. *)
      remove_fd t fd;
      Fd.set_state fd S.Closed;
      upon (close ()) (fun () -> Ivar.fill fd.Fd.close_finished ());
  end;
;;

let dec_num_active_syscalls_fd t fd =
  fd.Fd.num_active_syscalls <- fd.Fd.num_active_syscalls - 1;
  maybe_start_closing_fd t fd;
;;

let invariant t : unit =
  try
    let check invariant field = invariant (Field.get field t) in
    Fields.iter
      ~mutex:ignore
      ~is_running:ignore
      ~fds_whose_watching_has_changed:(check (fun fds_whose_watching_has_changed ->
        List.iter fds_whose_watching_has_changed ~f:(fun fd ->
          assert fd.Fd.watching_has_changed;
          begin match Fd_by_descr.find t.fd_by_descr fd.Fd.file_descr with
          | None -> assert false
          | Some fd' -> assert (phys_equal fd fd')
          end)))
      ~file_descr_watcher:(check (fun file_descr_watcher ->
        File_descr_watcher.invariant file_descr_watcher;
        File_descr_watcher.iter t.file_descr_watcher ~f:(fun file_descr _ ->
          try
            match Fd_by_descr.find t.fd_by_descr file_descr with
            | None -> failwith "missing from fd_by_descr"
            | Some fd -> assert (Fd.num_active_syscalls fd > 0);
          with exn ->
            failwiths "fd problem" (exn, file_descr) (<:sexp_of< exn * File_descr.t >>))))
      ~fd_by_descr:(check (fun fd_by_descr ->
        Fd_by_descr.invariant fd_by_descr;
        Fd_by_descr.iter fd_by_descr ~f:(fun fd ->
          if fd.Fd.watching_has_changed then
            assert (List.exists t.fds_whose_watching_has_changed ~f:(fun fd' ->
              phys_equal fd fd')))))
      ~scheduler_thread_id:ignore
      ~interruptor:(check Interruptor.invariant)
      ~signal_manager:(check Raw_signal_manager.invariant)
      ~finalizer_jobs:ignore
      ~thread_pool:(check Thread_pool.invariant)
      ~core_scheduler:(check Core_scheduler.invariant);
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let create () =
  if debug then Debug.log_string "creating scheduler";
  let thread_pool = ok_exn (Thread_pool.create ~max_num_threads:Config.max_num_threads) in
  let main_work_group = ok_exn (Thread_pool.create_work_group thread_pool) in
  ok_exn (Async_core.Execution_context.(Backpatched.Hole.fill main_work_group_hole)
            main_work_group);
  let num_file_descrs = Config.max_num_open_file_descrs in
  let fd_by_descr = Fd_by_descr.create ~num_file_descrs in
  let finalizer_jobs = Thread_safe_queue.create () in
  let create_fd kind file_descr info =
    let fd = Fd.create kind file_descr info in
    Fd_by_descr.add_exn fd_by_descr fd;
    fd
  in
  let interruptor = Interruptor.create ~create_fd in
  let core_scheduler = Core_scheduler.t () in
  let t =
    { mutex = Nano_mutex.create ();
      is_running = false;
      fds_whose_watching_has_changed = [];
      file_descr_watcher = File_descr_watcher.create ~num_file_descrs;
      fd_by_descr;
      scheduler_thread_id = -1; (* set when [be_the_scheduler] is called *)
      interruptor;
      signal_manager =
        Raw_signal_manager.create ~thread_safe_notify_signal_delivered:(fun () ->
          Interruptor.thread_safe_interrupt interruptor);
      finalizer_jobs;
      thread_pool;
      core_scheduler;
    }
  in
  if Async_core.Config.detect_invalid_access_from_thread then
    Core_scheduler.set_check_access core_scheduler (fun () ->
      if not (am_holding_lock t) then begin
        Debug.log "attempt to access async from thread not holding the async lock"
          t <:sexp_of< t >>;
        exit 1;
      end);
  t
;;

let init () = the_one_and_only_ref := Ready_to_initialize create

let () = init ()

let reset_in_forked_process () =
  Async_core.Scheduler.reset_in_forked_process ();
  init ();
;;

let thread_safe_wakeup_scheduler t = Interruptor.thread_safe_interrupt t.interruptor

let i_am_the_scheduler t = current_thread_id () = t.scheduler_thread_id

let have_lock_do_cycle t =
  if debug then Debug.log "have_lock_do_cycle" t <:sexp_of< t >>;
  Core_scheduler.run_cycle t.core_scheduler;
  if not (i_am_the_scheduler t) then
    (* If we are not the scheduler, wake it up so it can process any remaining jobs, clock
       events, or an unhandled exception. *)
    thread_safe_wakeup_scheduler t;
;;

let set_fd_desired_watching t fd read_or_write desired =
  Read_write.set fd.Fd.watching read_or_write desired;
  if not fd.Fd.watching_has_changed then begin
    fd.Fd.watching_has_changed <- true;
    t.fds_whose_watching_has_changed <- fd :: t.fds_whose_watching_has_changed;
  end
;;

let request_start_watching t fd read_or_write =
  if Debug.file_descr_watcher then
    Debug.log "request_start_watching" (read_or_write, fd, t)
      (<:sexp_of< Read_write.Key.t * Fd.t * t >>);
  if not fd.Fd.supports_nonblock then
    (* Some versions of epoll complain if one asks it to monitor a file descriptor that
       doesn't support nonblocking I/O, e.g. a file.  So, we never ask the
       file-descr-watcher to monitor such descriptors. *)
    `Unsupported
  else begin
    let module W = Fd.Watching in
    let start_watching () =
      let ivar = Ivar.create () in
      set_fd_desired_watching t fd read_or_write (W.Watching ivar);
      if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t;
      `Watching ivar
    in
    match Read_write.get fd.Fd.watching read_or_write with
    | W.Watching _ -> `Already_watching
    | W.Stop_requested ->
      (* We don't [inc_num_active_syscalls] in this case, because we already did when
         we transitioned from [Not_watching] to [Watching]. *)
      start_watching ()
    | W.Not_watching ->
      match Fd.inc_num_active_syscalls fd with
      | `Already_closed -> `Already_closed
      | `Ok -> start_watching ()
  end
;;

let request_stop_watching t fd read_or_write value =
  if Debug.file_descr_watcher then
    Debug.log "request_stop_watching" (read_or_write, value, fd, t)
      (<:sexp_of< Read_write.Key.t * Fd.ready_to_result * Fd.t * t >>);
  let module W = Fd.Watching in
  match Read_write.get fd.Fd.watching read_or_write with
  | W.Stop_requested | W.Not_watching -> ()
  | W.Watching ready_to ->
    Ivar.fill ready_to value;
    set_fd_desired_watching t fd read_or_write W.Stop_requested;
    if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t;
;;

let sync_changed_fds_to_file_descr_watcher t =
  let make_file_descr_watcher_agree_with fd =
    fd.Fd.watching_has_changed <- false;
    let desired =
      Read_write.mapi fd.Fd.watching ~f:(fun read_or_write watching ->
        let module W = Watching in
        match watching with
        | W.Watching _ -> true
        | W.Not_watching -> false
        | W.Stop_requested ->
          Read_write.set fd.Fd.watching read_or_write W.Not_watching;
          dec_num_active_syscalls_fd t fd;
          false)
    in
    if Debug.file_descr_watcher then
      Debug.log "File_descr_watcher.set" (fd.Fd.file_descr, desired, t.file_descr_watcher)
        (<:sexp_of< File_descr.t * bool Read_write.t * File_descr_watcher.t >>);
    File_descr_watcher.set t.file_descr_watcher fd.Fd.file_descr desired
  in
  let changed = t.fds_whose_watching_has_changed in
  t.fds_whose_watching_has_changed <- [];
  List.iter changed ~f:make_file_descr_watcher_agree_with;
;;

let be_the_scheduler ?(raise_unhandled_exn = false) t =
  t.scheduler_thread_id <- current_thread_id ();
  (* We handle [Signal.pipe] so that write() calls on a closed pipe/socket get EPIPE but
     the process doesn't die due to an unhandled SIGPIPE. *)
  Raw_signal_manager.manage t.signal_manager Signal.pipe;
  let rec handle_finalizers () =
    if debug then Debug.log_string "scheduling finalizers";
    match Thread_safe_queue.dequeue t.finalizer_jobs with
    | None -> ()
    | Some (execution_context, work) ->
      Core_scheduler.add_job execution_context work ();
      handle_finalizers ()
  in
  let compute_timeout () =
    if debug then Debug.log_string "compute_timeout";
    if Core_scheduler.num_pending_jobs t.core_scheduler > 0 then
      (* We want to timeout immediately if there are still jobs remaining, so that we
         immediately come back and start running them after checking for I/O. *)
      `Immediately
    else begin
      let now = Time.now () in
      match Core_scheduler.next_upcoming_event t.core_scheduler with
      | None -> `Never
      | Some time ->
        if Time.(>) time now then
          `After (Time.diff time now)
        else
          `Immediately
    end
  in
  let handle_post post =
    (* We handle writes before reads so that we get all the writes started going to the
       external world before we process all the reads.  This will nicely batch together
       all the output based on the reads for the next writes. *)
    List.iter [ `Write; `Read ] ~f:(fun read_or_write ->
      let { File_descr_watcher_intf.Post. ready; bad } =
        Read_write.get post read_or_write
      in
      let fill zs value =
        List.iter zs ~f:(fun file_descr ->
          match Fd_by_descr.find t.fd_by_descr file_descr with
          | None ->
            failwiths "File_descr_watcher returned unknown file descr" file_descr
              (<:sexp_of< File_descr.t >>)
          | Some fd ->
            let module W = Fd.Watching in
            match Read_write.get fd.Fd.watching read_or_write with
            | W.Stop_requested -> ()
            | W.Not_watching ->
              failwiths "File_descr_watcher returned unwatched file descr" fd
                (<:sexp_of< Fd.t >>)
            | W.Watching ready_to ->
              Ivar.fill ready_to value;
              set_fd_desired_watching t fd read_or_write W.Stop_requested);
      in
      fill bad `Bad_fd;
      fill ready `Ready)
  in
  let rec loop () =
    (* At this point, we have the lock. *)
    if Config.check_invariants then invariant t;
    begin
      match request_start_watching t (Interruptor.read_fd t.interruptor) `Read with
      | `Already_watching | `Watching _ -> ()
      | `Unsupported | `Already_closed ->
        failwiths "can not watch interruptor" t <:sexp_of< t >>
    end;
    sync_changed_fds_to_file_descr_watcher t;
    if Debug.file_descr_watcher then
      Debug.log "File_descr_watcher.pre_check" t (<:sexp_of< t >>);
    let pre = File_descr_watcher.pre_check t.file_descr_watcher in
    (* [compute_timeout] must be the last thing before [thread_safe_check], because we
       want to make sure the timeout is zero if there are any scheduled jobs. *)
    let timeout = compute_timeout () in
    unlock t;
    if Debug.file_descr_watcher then
      Debug.log "File_descr_watcher.thread_safe_check" (timeout, t)
        (<:sexp_of< File_descr_watcher_intf.Timeout.t * t >>);
    let check_result =
      File_descr_watcher.thread_safe_check t.file_descr_watcher pre ~timeout
    in
    lock t;
    (* We call [Interruptor.clear] after [thread_safe_check] and before any of the
       processing that needs to happen in response to [thread_safe_interrupt].  That way,
       even if [Interruptor.clear] clears out an interrupt that hasn't been serviced yet,
       the interrupt will still be serviced by the immediately following processing. *)
    Interruptor.clear t.interruptor;
    if Debug.file_descr_watcher then
      Debug.log "File_descr_watcher.post_check" (check_result, t)
        (<:sexp_of< File_descr_watcher.Check_result.t * t >>);
    begin match File_descr_watcher.post_check t.file_descr_watcher check_result with
    | `Timeout | `Syscall_interrupted -> ()
    | `Ok post ->
      if Debug.file_descr_watcher then
        Debug.log "File_descr_watcher.post_check returned" (post, t)
          (<:sexp_of< File_descr_watcher_intf.Post.t Read_write.t * t >>);
      handle_post post;
    end;
    if debug then Debug.log_string "handling finalizers";
    handle_finalizers ();
    if debug then Debug.log_string "handling delivered signals";
    Raw_signal_manager.handle_delivered t.signal_manager;
    have_lock_do_cycle t;
    match Core_scheduler.uncaught_exn t.core_scheduler with
    | None -> loop ()
    | Some error -> unlock t; error
  in
  let exn =
    try `User_uncaught (loop ())
    with exn -> `Async_uncaught exn
  in
  let error =
    match exn with
    | `User_uncaught error -> error
    | `Async_uncaught exn ->
      Error.create "bug in async scheduler" (exn, t) <:sexp_of< exn * t >>
  in
  if raise_unhandled_exn then
    Error.raise error
  else begin
    Core.Std.eprintf "%s\n%!" (Sexp.to_string_hum (Error.sexp_of_t error));
    exit 1;
  end
;;

let add_finalizer t heap_block f =
  let e = current_execution_context t in
  let finalizer heap_block =
    (* Here we can be in any thread, and may not be holding the async lock.  So, we
       can only do thread-safe things.

       By putting [heap_block] in [finalizer_jobs], we are keeping it alive until the next
       time the scheduler gets around to calling [handle_finalizers].  Calling
       [thread_safe_wakeup_scheduler] ensures that will happen in short order.  Thus, we
       are not dramatically increasing the lifetime of [heap_block], since the OCaml
       runtime already resurrected [heap_block] so that we could refer to it here.  The
       OCaml runtime already removed the finalizer function when it noticed [heap_block]
       could be finalized, so there is no infinite loop in which we are causing the
       finalizer to run again.  Also, OCaml does not impose any requirement on finalizer
       functions that they need to dispose of the block, so it's fine that we keep
       [heap_block] around until later. *)
    Thread_safe_queue.enqueue t.finalizer_jobs (e, (fun () -> f heap_block));
    thread_safe_wakeup_scheduler t;
  in
  (* We use [Caml.Gc.finalise] instead of [Core.Std.Gc.add_finalizer] because the latter
     has its own wrapper around [Caml.Gc.finalise] to run finalizers synchronously. *)
  Caml.Gc.finalise finalizer heap_block;
;;

let add_finalizer_exn t x f =
  add_finalizer t (Heap_block.create_exn x)
    (fun heap_block -> f (Heap_block.value heap_block))
;;

let go ?raise_unhandled_exn () =
  if debug then Debug.log_string "Scheduler.go";
  let t = the_one_and_only ~should_lock:false in
  (* [go] is called from the main thread and so must acquire the lock if the thread has
     not already done so implicitly via use of an async operation that uses
     [the_one_and_only]. *)
  if not (am_holding_lock t) then lock t;
  if not t.is_running then begin
    t.is_running <- true;
    be_the_scheduler t ?raise_unhandled_exn;
  end else begin
    unlock t;
    (* We wakeup the scheduler so it can respond to whatever async changes this thread
       made. *)
    thread_safe_wakeup_scheduler t;
    (* Since the scheduler is already running, so we just pause forever. *)
    Time.pause_forever ();
  end
;;

let go_main ?raise_unhandled_exn ~main () =
  if not (is_ready_to_initialize ()) then
    failwith "Async was initialized prior to [Scheduler.go_main]";
  Deferred.upon Deferred.unit main;
  go ?raise_unhandled_exn ();
;;

let is_running () = (the_one_and_only ~should_lock:false).is_running

let report_long_cycle_times ?(cutoff = sec 1.) () =
  Stream.iter (cycle_times ())
    ~f:(fun span ->
      if Time.Span.(>) span cutoff then
        eprintf "%s\n%!"
          (Error.to_string_hum
             (Error.create "long async cycle" span <:sexp_of< Time.Span.t >>)))
;;
