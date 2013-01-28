open Core.Std
open Import

module Core_scheduler = Async_core.Scheduler
module Block_group = Async_core.Block_group
module Work = Block_group.Work
module Fd = Raw_fd
module Signal = Core.Std.Signal

type 'a with_options = 'a Core_scheduler.with_options

include struct
  open Core_scheduler

  let current_execution_context = current_execution_context
  let cycle_count = cycle_count
  let cycle_start = cycle_start
  let cycle_times = cycle_times
  let num_pending_jobs = num_pending_jobs
  let schedule = schedule
  let schedule' = schedule'
  let set_max_num_jobs_per_priority_per_cycle = set_max_num_jobs_per_priority_per_cycle
  let within = within
  let within' = within'
  let within_context = within_context
  let within_v = within_v
end

let debug = Debug.debug

type finalizer_job = Execution_context.t * (unit -> unit)

type t =
  { (* The scheduler [mutex] must be locked by all code that is manipulating scheduler
       data structures, which is almost all async code.  The [mutex] is automatically
       locked in the main thread when the scheduler is first created.  A [Nano_mutex]
       keeps track of which thread is holding the lock.  This means we can detect errors
       in which code is trying to acquire the async lock while it already holds it, or
       release the lock when it doesn't hold it. *)
    mutex : Nano_mutex.t;
    mutable go_has_been_called : bool;
    file_descr_watcher : (Fd.t, Fd.ready_to_result Ivar.t) File_descr_watcher.t;
    (* [fd_by_descr] holds every file descriptor that Async knows about.  Fds are added
       when they are created, and removed when they transition to [Closed]. *)
    fd_by_descr : Fd_by_descr.t;
    mutable id_of_thread_running_the_select_loop : int;
    (* The [select_interruptor] is used to interrupt the call to select when the select
       loop needs to be woken up to process changes, for any of the following reasons:

       * to start watching a file descriptor
       * to add a timer event
       * to process a finalizer
       * to process a signal
       * to process a toplevel unhandled exception *)
    select_interruptor : Interruptor.t;

    signal_manager : Raw_signal_manager.t;

    (* Finalizers are very much like signals; they can come at any time and in any
       thread.  So, when an OCaml finalizer fires, we stick a closure to do the work
       in a thread-safe queue of [finalizer_jobs], which the select loop then schedules
       to run as ordinary async jobs. *)
    finalizer_jobs : finalizer_job Thread_safe_queue.t sexp_opaque;

    (* [num_blocked_threads] is the sum over all block groups of num_blocked_threads for
       that block group. *)
    mutable num_blocked_threads : int;
    mutable num_live_threads : int;
    (* [num_reserved_threads] is the sum over all block groups of num_reserved_threads
       for that block group. *)
    mutable num_reserved_threads : int;
    max_num_live_threads : int;
    work_for_threads: Work.t Squeue.t;
  }
with fields, sexp_of

let create_fd t kind file_descr ~name =
  let fd = Fd.create kind file_descr ~name in
  Fd_by_descr.add t.fd_by_descr fd;
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
      (* We lock the scheduler because the user may do Async stuff at the top level before
         calling [Scheduler.go], and we don't want anyone to be able to run jobs until
         [Scheduler.go] is called.  This could happen, e.g. by creating a reader that does
         a read system call in another (true) thread.  The scheduler will remain locked
         until the select loop called by [Scheduler.go] unlocks it. *)
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

let current_thread_id () = Thread.id (Thread.self ())

let is_main_thread () = current_thread_id () = 0

let remove_fd t fd = Fd_by_descr.remove t.fd_by_descr fd

let maybe_start_closing_fd t fd =
  if fd.Fd.num_active_syscalls = 0 then begin
    let module S = Fd.State in
    match fd.Fd.state with
    | S.Closed | S.Open | S.Replaced -> ()
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

let invariant t =
  if debug then begin
    try
      Core_scheduler.invariant ();
      let check invariant field = invariant (Field.get field t) in
      Fields.iter
        ~mutex:ignore
        ~go_has_been_called:ignore
        ~file_descr_watcher:(check (fun file_descr_watcher ->
          File_descr_watcher.invariant file_descr_watcher;
          File_descr_watcher.iter t.file_descr_watcher ~f:(fun read_or_write _state fd ->
            try
              match Fd_by_descr.find t.fd_by_descr (Fd.file_descr fd) with
              | None -> failwith "missing from fd_by_descr"
              | Some fd' ->
                if not (Fd.equal fd fd')
                then failwiths "fd not equal to" fd' <:sexp_of< Fd.t >>;
                if Fd.num_active_syscalls fd = 0
                then failwith "zero active syscalls for fd being watched";
            with exn ->
              failwiths "fd problem" (exn, read_or_write, fd)
                (<:sexp_of< exn * Read_write.Key.t * Fd.t >>))))
        ~fd_by_descr:(check Fd_by_descr.invariant)
        ~id_of_thread_running_the_select_loop:ignore
        ~select_interruptor:(check Interruptor.invariant)
        ~signal_manager:(check Raw_signal_manager.invariant)
        ~finalizer_jobs:ignore
        ~num_blocked_threads:(check (fun num_blocked_threads ->
          assert (0 <= num_blocked_threads);
          assert (num_blocked_threads <= t.num_live_threads)))
        ~num_live_threads:(check (fun num_live_threads ->
          (* Due to reservations being released, we could have [num_live_threads >
             num_reserved_threads]. *)
          assert (num_live_threads <= t.max_num_live_threads)))
        ~num_reserved_threads:(check (fun num_reserved_threads ->
          assert (num_reserved_threads <= t.max_num_live_threads)))
        ~max_num_live_threads:(check (fun max_num_live_threads ->
          assert (max_num_live_threads >= 1)))
        ~work_for_threads:ignore
    with exn -> failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
  end
;;

let create () =
  let num_file_descrs = 1024 in
  let fd_by_descr = Fd_by_descr.create ~num_file_descrs in
  let finalizer_jobs = Thread_safe_queue.create () in
  let create_fd kind file_descr ~name =
    let fd = Fd.create kind file_descr ~name in
    Fd_by_descr.add fd_by_descr fd;
    fd
  in
  let select_interruptor = Interruptor.create ~create_fd in
  let t =
    { mutex = Nano_mutex.create ();
      go_has_been_called = false;
      file_descr_watcher = File_descr_watcher.create ~num_file_descrs;
      fd_by_descr;
      id_of_thread_running_the_select_loop = -1; (* set when [select_loop] is called *)
      select_interruptor;
      signal_manager =
        Raw_signal_manager.create ~thread_safe_notify_signal_delivered:(fun () ->
          Interruptor.thread_safe_interrupt select_interruptor);
      finalizer_jobs;
      num_blocked_threads = 0;
      num_live_threads = 0;
      num_reserved_threads = 0;
      max_num_live_threads = 50;
      work_for_threads = Squeue.create 10;
    }
  in
  t
;;

let thread_safe_interrupt_select t =
  Interruptor.thread_safe_interrupt t.select_interruptor
;;

let i_am_the_select_loop_thread t =
  current_thread_id () = t.id_of_thread_running_the_select_loop
;;

let have_lock_do_cycle t =
  let maybe_jobs_remain = Core_scheduler.run_cycle () in
  if not (i_am_the_select_loop_thread t) then
    (* If we are not the select loop thread, wake it up so it can process any remaining
       jobs, clock events, or an unhandled exception. *)
    thread_safe_interrupt_select t;
  maybe_jobs_remain
;;

let create_thread t ?(default_thread_name_first16 = "helper-thread") squeue =
  t.num_live_threads <- t.num_live_threads + 1;
  let dead () = t.num_live_threads <- t.num_live_threads - 1 in
  let (_ : Thread.t) = Thread.create (fun () ->
    let last_thread_name = ref "" in
    let set_thread_name thread_name =
      if String.(<>) thread_name !last_thread_name then begin
        begin match Linux_ext.pr_set_name_first16 with
        | Ok f -> f thread_name
        | Error _ -> ()
        end;
        last_thread_name := thread_name;
      end;
    in
    set_thread_name default_thread_name_first16;
    let rec loop () =
      let work = Squeue.pop squeue in
      let thread_name =
        Option.value work.Work.set_thread_name_to
          ~default:default_thread_name_first16
      in
      set_thread_name thread_name;
      match (try work.Work.doit () with e -> dead (); raise e) with
      | `Stop -> dead ()
      | `Continue -> loop ()
    in
    loop ()) ()
  in
  ()
;;

let request_start_watching t fd read_or_write ~interrupt_select =
  match Fd.inc_num_active_syscalls fd with
  | `Already_closed -> `Already_closed
  | `Ok ->
    let ivar = Ivar.create () in
    match
      File_descr_watcher.request_start_watching t.file_descr_watcher fd.Fd.file_descr
        fd ivar read_or_write
    with
    | `Already_watching ->
      dec_num_active_syscalls_fd t fd;
      `Already_watching
    | `Ok ->
      if interrupt_select then thread_safe_interrupt_select t;
      `Watching ivar
;;

let request_stop_watching t fd read_or_write value =
  match
    File_descr_watcher.request_stop_watching t.file_descr_watcher
      fd.Fd.file_descr read_or_write
  with
  | `Was_not_watching -> ()
  | `Ok ready_to ->
    Ivar.fill ready_to value;
    (* We interrupt the select loop so that it can process the request.  This is
       necessary even if [t.am_in_select_do_cycle], because the select loop only
       processes requests to stop watching after a call to select.  And we would like
       the existence of such a request to be sufficient to force select to not delay. *)
    thread_safe_interrupt_select t;
;;

let select_loop t =
  t.id_of_thread_running_the_select_loop <- current_thread_id ();
  let rec handle_finalizers () =
    if debug then Debug.log_string "scheduling finalizers";
    match Thread_safe_queue.dequeue t.finalizer_jobs with
    | None -> ()
    | Some (execution_context, work) ->
      Core_scheduler.add_job execution_context work ();
      handle_finalizers ()
  in
  let compute_timeout maybe_jobs_remain =
    if debug then Debug.log_string "compute_timeout";
    (* We want to set the timeout to `Zero if there are still jobs remaining, so that we
       immediately come back and start running them after select() checks for I/O. *)
    match maybe_jobs_remain with
    | `Jobs_remain -> `Zero
    | `No_jobs_remain ->
      let now = Time.now () in
      match Core_scheduler.next_upcoming_event () with
      | None -> `Forever
      | Some time ->
        if Time.(>) time now then
          `Wait_for (Time.diff time now)
        else
          `Zero
  in
  (* The select loop has the following structure to ensure that interrupts to the
     interruptor are not ignored.

     loop ()
       do_cycle
       unlock
       select
       lock
       Interruptor.clear
       handle_finalizers
       loop ()

     The key point is that [Interruptor.clear] happens after [select] and before any of
     the processing that needs to happen in response to [thread_safe_interrupt].  That
     way, even if [Interruptor.clear] clears out an interrupt that hasn't been serviced
     yet, the interrupt will still be serviced by the immediately following processing. *)
  let rec loop () =
    (* At this point, we have the lock. *)
    invariant t;
    let maybe_jobs_remain = have_lock_do_cycle t in
    invariant t;
    match Core_scheduler.uncaught_exception () with
    | Some exn ->
      unlock t;
      exn
    | None ->
      begin
        match
          request_start_watching t (Interruptor.read_fd t.select_interruptor) `Read
            ~interrupt_select:false
        with
      | `Already_watching | `Watching _ -> ()
      | `Already_closed -> failwiths "can not watch select interruptor" t <:sexp_of< t >>
      end;
      let rec select_loop () =
        let timeout = compute_timeout maybe_jobs_remain in
        let select_timeout =
          match timeout with
          | `Forever -> (-1.0)
          | `Zero -> 0.0
          | `Wait_for span ->
            (* Wake up at least every second.  Avoid any weirdness due to feeding large
               timeouts to select. *)
            Float.min (Time.Span.to_sec span) 1.0
        in
        if debug then Debug.log "selecting for" select_timeout <:sexp_of< float >>;
        let pre = File_descr_watcher.pre_check t.file_descr_watcher in
        unlock t;
        let check_result =
          File_descr_watcher.thread_safe_check
            t.file_descr_watcher pre ~timeout:select_timeout;
        in
        lock t;
        match File_descr_watcher.post_check t.file_descr_watcher check_result with
        | `Retry -> select_loop ()
        | `Ok post -> post
      in
      let post = select_loop () in
      if debug then Debug.log_string "select returned";
      Interruptor.clear t.select_interruptor;
      if debug then Debug.log_string "handling finalizers";
      handle_finalizers ();
      if debug then Debug.log_string "handling delivered signals";
      Raw_signal_manager.handle_delivered t.signal_manager;
      let handle_post read_or_write =
        let post = Read_write.get post read_or_write in
        let module P = File_descr_watcher in
        let fill zs value =
          List.iter zs ~f:(fun (fd, ready_to) ->
            Ivar.fill ready_to value;
            dec_num_active_syscalls_fd t fd);
        in
        fill post.P.bad `Bad_fd;
        fill post.P.ready `Ready;
        List.iter post.P.no_longer_watching ~f:(fun fd -> dec_num_active_syscalls_fd t fd);
      in
      (* We handle writes before reads so that we get all the writes started going to the
         external world before we process all the reads.  This will nicely batch together
         all the output based on the reads for the next writes. *)
      handle_post `Write;
      handle_post `Read;
      loop ();
  in
  let exn =
    try `User_uncaught (loop ())
    with exn -> `Async_uncaught exn
  in
  match exn with
  | `Async_uncaught exn ->
    (* This is a really bad death and causes the program to exit. *)
    failwiths "select_loop bug" (exn, t) <:sexp_of< exn * t >>
  | `User_uncaught error ->
    (* Do not put a print statement here.  Having the scheduler raise an
       uncaught exception is the necessary behavior for programs that call
       Scheduler.go and want to handle it. *)
    Error.raise error
;;

let finalize t f obj =
  let e = current_execution_context () in
  (* We use [Caml.Gc.finalise] instead of [Core.Std.Gc.finalise] because the latter has
     its own wrapper around [Caml.Gc.finalise] to run finalizers synchronously. *)
  Caml.Gc.finalise (fun x ->
    (* By putting [x] in [finalizer_jobs], we are keeping it alive until the next time
       [select_loop] gets around to calling [handle_finalizers].  Calling
       [thread_safe_interrupt_select] ensures that will happen in short order.  Thus,
       we are not dramatically increasing the lifetime of [x], since the OCaml runtime
       already resurrected [x] so that we could refer to it here.  The OCaml runtime
       already removed the finalizer function when it noticed [x] could be finalized, so
       there is no infinite loop in which we are causing the finalizer to run again.
       Also, OCaml does not impose any requirement on finalizer functions that they need
       to dispose of the block, so it's fine that we keep [x] around until later. *)
    Thread_safe_queue.enqueue t.finalizer_jobs (e, (fun () -> f x));
    thread_safe_interrupt_select t)
    obj;
;;

(* [go] is at the core of every program that uses Async.  If there is a bug in the code
   below, e.g. an unhandled exception, then it will cause the Async program to die.  The
   monitor system will have no chance.  Thus, the code that implements [go] should be read
   especially carefully.  [go] is called from the main thread and so must acquire the lock
   if the thread has not already done so implicitly via use of an async operation that
   used [the_one_and_only]. *)
let go ?(raise_unhandled_exn = Bool.False_.default) () =
  if debug then Debug.log_string "Scheduler.go";
  let raise_unhandled_exn = (raise_unhandled_exn :> bool) in
  let t = the_one_and_only ~should_lock:false in
  if not (am_holding_lock t) then lock t;
  if t.go_has_been_called then begin
    (* Someone else has run the scheduler already, so we just pause forever. *)
    unlock t;
    (* We call [thread_safe_interrupt_select] so that the select loop, which is running
       in another thread, can respond to whatever async changes this thread made. *)
    thread_safe_interrupt_select t;
    Time.pause_forever ()
  end else begin
    t.go_has_been_called <- true;
    (* We handle [Signal.pipe] so that write() calls on a closed pipe/socket get EPIPE but
       the process doesn't die due to an unhandled SIGPIPE. *)
    Raw_signal_manager.handle_signal t.signal_manager Signal.pipe;
    if raise_unhandled_exn
    then select_loop t
    else begin
      Exn.handle_uncaught ~exit:true (fun () -> never_returns (select_loop t));
      assert false
    end;
  end
;;

let go_main ?raise_unhandled_exn ~main () =
  if not (is_ready_to_initialize ()) then
    failwith "Async was initialized prior to [Scheduler.go_main]";
  Deferred.upon Deferred.unit main;
  go ?raise_unhandled_exn ();
;;

(* When a block group becomes unreachable, use a finalizer to free up its reserved
   threads. *)
let finalize_block_group t bg =
  t.num_reserved_threads <- t.num_reserved_threads - bg.Block_group.num_reserved_threads;
;;

let create_block_group t ?(min_reserved_threads = 0)
    ?(max_reserved_threads = max_int) () =
  let reserved = t.num_reserved_threads + min_reserved_threads in
  if reserved > t.max_num_live_threads then
    `Out_of_threads
  else begin
    t.num_reserved_threads <- reserved;
    let bg = Block_group.create ~min_reserved_threads ~max_reserved_threads in
    Block_group.invariant bg;
    finalize t (fun bg -> finalize_block_group t bg) bg;
    `Ok bg;
  end
;;

let init () =
  the_one_and_only_ref :=
    Ready_to_initialize
    (fun () ->
      let t = create () in
      let main_block_group =
        match create_block_group t ~min_reserved_threads:1 () with
        | `Ok t -> t
        | `Out_of_threads -> failwith "Async unable to create main block group"
      in
      Core_scheduler.initialize_execution_context main_block_group;
      t)

(* This is a top-level effect, but it simply fills in a ref cell with a closure,
   and is therefore OK. *)
let () = init ()

let is_running () = (the_one_and_only ~should_lock:false).go_has_been_called

let report_long_cycle_times ?(cutoff = sec 1.) () =
  Stream.iter (cycle_times ())
    ~f:(fun span ->
      if Time.Span.(>) span cutoff then
        eprintf "%s\n%!"
          (Error.to_string_hum
             (Error.create "long async cycle" span <:sexp_of< Time.Span.t >>)))
;;
