open Core.Std
open Import

module Fd = Raw_fd
module Job = Async_core.Job
module Watching = Fd.Watching
module Signal = Core.Std.Signal
module Timerfd = Linux_ext.Timerfd

let debug = Debug.scheduler

module File_descr_watcher = struct
  (* A file descriptor watcher implementation + a watcher.  We need the file-descr watcher
     as a first-class value to support choosing which file-descr watcher to use in
     [go_main].  We could define [t] as [Epoll of ... | Select of ...] and dispatch every
     call, but it is simpler to just pack the file descriptor watcher with its associated
     functions (OO-programming with modules...). *)
  module type S = sig
    include File_descr_watcher_intf.S
    val watcher : t
  end

  type t = (module S)

  let sexp_of_t t =
    let module F = (val t : S) in
    (* Include the backend information so we know which one it is. *)
    <:sexp_of< Config.File_descr_watcher.t * F.t >> (F.backend, F.watcher)
  ;;
end

type 'a with_options = 'a Core_scheduler.with_options

include struct
  open Core_scheduler

  let preserve_execution_context  = preserve_execution_context
  let preserve_execution_context' = preserve_execution_context'
  let schedule                    = schedule
  let schedule'                   = schedule'
  let within                      = within
  let within'                     = within'
  let within_context              = within_context
  let within_v                    = within_v
  let find_local                  = find_local
  let with_local                  = with_local
end

let cycle_count () = Core_scheduler.(cycle_count (t ()))
let cycle_start () = Core_scheduler.(cycle_start (t ()))
let cycle_times () = Core_scheduler.(cycle_times (t ()))
let set_max_num_jobs_per_priority_per_cycle i =
  Core_scheduler.(set_max_num_jobs_per_priority_per_cycle (t ())) i
;;

let force_current_cycle_to_end () = Core_scheduler.(force_current_cycle_to_end (t ()))

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
    mutable have_called_go : bool;

    (* [fds_whose_watching_has_changed] holds all fds whose watching has changed since
       the last time their desired state was set in the [file_descr_watcher]. *)
    mutable fds_whose_watching_has_changed : Fd.t list;
    file_descr_watcher : File_descr_watcher.t;

    (* [fd_by_descr] holds every file descriptor that Async knows about.  Fds are added
       when they are created, and removed when they transition to [Closed]. *)
    fd_by_descr : Fd_by_descr.t;

    (* If we are using a file descriptor watcher that does not support sub-millisecond
       timeout, this contains a timerfd used to handle the next expiration. *)
    mutable timerfd : Linux_ext.Timerfd.t option;

    (* A distinguished thread, called the "scheduler" thread, is continually looping,
       checking file descriptors for I/O and then running a cycle.  It manages
       the [file_descr_watcher] and runs signal handlers.

       [scheduler_thread_id] is mutable because we create the scheduler before starting
       the scheduler running.  Once we start running the scheduler, [scheduler_thread_id]
       is set and never changes again. *)
    mutable scheduler_thread_id : int;

    (* The [interruptor] is used to wake up the scheduler when it is blocked on the file
       descriptor watcher. *)
    interruptor : Interruptor.t;

    signal_manager : Raw_signal_manager.t;

    (* The [thread_pool] is used for making blocking system calls in threads other than
       the scheduler thread, and for servicing [In_thread.run] requests. *)
    thread_pool : Thread_pool.t;

    (* [handle_thread_pool_stuck] is called once per second if the thread pool is "stuck",
       i.e has not completed a job for one second and has no available threads. *)
    mutable handle_thread_pool_stuck : Time.Span.t -> unit;

    busy_pollers : Busy_pollers.t;
    mutable busy_poll_thread_is_running : bool;

    core_scheduler : Core_scheduler.t;

    (* configuration *)
    mutable max_inter_cycle_timeout : Max_inter_cycle_timeout.t;
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
      ~have_called_go:ignore
      ~fds_whose_watching_has_changed:(check (fun fds_whose_watching_has_changed ->
        List.iter fds_whose_watching_has_changed ~f:(fun fd ->
          assert fd.Fd.watching_has_changed;
          begin match Fd_by_descr.find t.fd_by_descr fd.Fd.file_descr with
          | None -> assert false
          | Some fd' -> assert (phys_equal fd fd')
          end)))
      ~file_descr_watcher:(check (fun file_descr_watcher ->
        let module F = (val file_descr_watcher : File_descr_watcher.S) in
        F.invariant F.watcher;
        F.iter F.watcher ~f:(fun file_descr _ ->
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
      ~timerfd:ignore
      ~scheduler_thread_id:ignore
      ~interruptor:(check Interruptor.invariant)
      ~signal_manager:(check Raw_signal_manager.invariant)
      ~thread_pool:(check Thread_pool.invariant)
      ~handle_thread_pool_stuck:ignore
      ~busy_pollers:(check Busy_pollers.invariant)
      ~busy_poll_thread_is_running:ignore
      ~core_scheduler:(check Core_scheduler.invariant)
      ~max_inter_cycle_timeout:ignore
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let update_check_access t do_check =
  Core_scheduler.set_check_access t.core_scheduler
    (if not do_check then
       None
     else
       Some (fun () ->
         if not (am_holding_lock t) then begin
           Debug.log "attempt to access async from thread not holding the async lock"
             (Backtrace.get_opt (), t) <:sexp_of< Backtrace.t option * t >>;
           exit 1;
         end))
;;

(* Try to create a timerfd.  It returns [None] if [Core] is not built with timerfd support
   or if it is not available on the current system. *)
let try_create_timerfd () =
  match Timerfd.create with
  | Result.Error _ -> None
  | Result.Ok create ->
    let clock = Timerfd.Clock.realtime in
    try
      Some (create clock ~flags:Timerfd.Flags.(nonblock + cloexec))
    with
    | Unix.Unix_error (Unix.ENOSYS, _, _) ->
      (* Kernel too old. *)
      None
    | Unix.Unix_error (Unix.EINVAL, _, _) ->
      (* Flags are only supported with Linux >= 2.6.27, try without them. *)
      let timerfd = create clock in
      Unix.set_close_on_exec (timerfd : Timerfd.t :> Unix.File_descr.t);
      Unix.set_nonblock (timerfd : Timerfd.t :> Unix.File_descr.t);
      Some timerfd
;;

let default_handle_thread_pool_stuck span =
  if Time.Span.(>=) span Config.report_thread_pool_stuck_for then begin
    let now = Time.now () in
    let message =
      sprintf "\
%s: Async's thread pool hasn't completed a job for %s, and is using the
maximum allowed number of threads (%d)."
        (Time.format now "%F %T %Z")
        (Time.Span.to_short_string span)
        (Config.Max_num_threads.raw Config.max_num_threads)
    in
    if Time.Span.(>=) span Config.abort_after_thread_pool_stuck_for
    then Monitor.send_exn Monitor.main (Failure message)
    else
      Core.Std.eprintf "%s\n%!"
        (String.concat [ message
                       ; sprintf "\
\  This is only a warning.  In %s, it will raise an
exception.
"
                           (Time.Span.to_short_string
                              (Time.Span.(-)
                                 Config.abort_after_thread_pool_stuck_for
                                 span))
                       ])
  end;
;;

let detect_stuck_thread_pool t =
  let last_time_work_was_completed = ref (Time.now ()) in
  let last_num_work_completed = ref (Thread_pool.num_work_completed t.thread_pool) in
  every (sec 1.) (fun () ->
    let num_work_completed = Thread_pool.num_work_completed t.thread_pool in
    let now = Time.now () in
    if num_work_completed > !last_num_work_completed then begin
      last_num_work_completed := num_work_completed;
      last_time_work_was_completed := now;
    end else
      let since_last_work_was_completed =
        Time.diff now !last_time_work_was_completed
      in
      if Time.Span.(>) since_last_work_was_completed (sec 1.)
      && not (Thread_pool.has_thread_available t.thread_pool)
      then t.handle_thread_pool_stuck since_last_work_was_completed);
;;

let create
      ?(file_descr_watcher       = Config.file_descr_watcher)
      ?(max_num_open_file_descrs = Config.max_num_open_file_descrs)
      ?(max_num_threads          = Config.max_num_threads)
      () =
  if debug then Debug.log_string "creating scheduler";
  let thread_pool =
    ok_exn (Thread_pool.create ~max_num_threads:(Max_num_threads.raw max_num_threads))
  in
  let num_file_descrs = Max_num_open_file_descrs.raw max_num_open_file_descrs in
  let fd_by_descr = Fd_by_descr.create ~num_file_descrs in
  let create_fd kind file_descr info =
    let fd = Fd.create kind file_descr info in
    Fd_by_descr.add_exn fd_by_descr fd;
    fd
  in
  let interruptor = Interruptor.create ~create_fd in
  let file_descr_watcher, timerfd =
    let module F = Config.File_descr_watcher in
    match file_descr_watcher with
    | F.Select ->
      let watcher = Select_file_descr_watcher.create ~num_file_descrs in
      let module W = struct
        include Select_file_descr_watcher
        let watcher = watcher
      end in
      ((module W : File_descr_watcher.S), None)
    | F.Epoll ->
      let watcher = Epoll_file_descr_watcher.create ~num_file_descrs in
      let timerfd = try_create_timerfd () in
      (* Start watching the timerfd. *)
      begin match timerfd with
      | None ->
        failwith "\
Async refuses to run using epoll on a system that doesn't support timer FDs, since
Async will be unable to timeout with sub-millisecond precision."
      | Some tfd ->
        let module Epoll = Linux_ext.Epoll in
        let epoll = Epoll_file_descr_watcher.epoll watcher in
        (* Use the edge-triggered behavior so we don't have to reset the timerfd when it
           expires. *)
        Epoll.set epoll (tfd : Timerfd.t :> Unix.File_descr.t) Epoll.Flags.(in_ + et);
      end;
      let module W = struct
        include Epoll_file_descr_watcher
        let watcher = watcher
      end in
      ((module W : File_descr_watcher.S), timerfd)
  in
  let core_scheduler = Core_scheduler.t () in
  let t =
    { mutex = Nano_mutex.create ();
      is_running = false;
      have_called_go = false;
      fds_whose_watching_has_changed = [];
      file_descr_watcher;
      fd_by_descr;
      timerfd;
      scheduler_thread_id = -1; (* set when [be_the_scheduler] is called *)
      interruptor;
      signal_manager =
        Raw_signal_manager.create ~thread_safe_notify_signal_delivered:(fun () ->
          Interruptor.thread_safe_interrupt interruptor);
      thread_pool;
      handle_thread_pool_stuck = default_handle_thread_pool_stuck;
      busy_pollers = Busy_pollers.create ();
      busy_poll_thread_is_running = false;
      core_scheduler;
      max_inter_cycle_timeout = Config.max_inter_cycle_timeout;
    }
  in
  detect_stuck_thread_pool t;
  update_check_access t Config.detect_invalid_access_from_thread;
  t
;;

let init () = the_one_and_only_ref := Ready_to_initialize (fun () -> create ())

let () = init ()

let reset_in_forked_process () =
  begin match !the_one_and_only_ref with
  | Initialized { timerfd = Some tfd; _ } ->
    Unix.close (tfd :> Unix.File_descr.t)
  | _ ->
    ()
  end;
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

let request_start_watching t fd read_or_write watching =
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
      set_fd_desired_watching t fd read_or_write watching;
      if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t;
      `Watching
    in
    match Read_write.get fd.Fd.watching read_or_write with
    | W.Watch_once _ | W.Watch_repeatedly _ -> `Already_watching
    | W.Stop_requested ->
      (* We don't [inc_num_active_syscalls] in this case, because we already did when we
         transitioned from [Not_watching] to [Watching].  Also, it is possible that [fd]
         was closed since we transitioned to [Stop_requested], in which case we don't want
         to [start_watching]; we want to report that it was closed and leave it
         [Stop_requested] so the the file-descr-watcher will stop watching it and we can
         actually close it. *)
      if Fd.is_closed fd
      then `Already_closed
      else start_watching ()
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
  | W.Watch_once ready_to ->
    Ivar.fill ready_to value;
    set_fd_desired_watching t fd read_or_write W.Stop_requested;
    if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t;
  | W.Watch_repeatedly (job, finished) ->
    begin match value with
    | `Ready -> Core_scheduler.add_job2 t.core_scheduler job
    | `Closed | `Bad_fd | `Interrupted as value ->
      Ivar.fill finished value;
      set_fd_desired_watching t fd read_or_write W.Stop_requested;
    end;
    if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t;
;;

let sync_changed_fds_to_file_descr_watcher t =
  let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
  let make_file_descr_watcher_agree_with fd =
    fd.Fd.watching_has_changed <- false;
    let desired =
      Read_write.mapi fd.Fd.watching ~f:(fun read_or_write watching ->
        let module W = Watching in
        match watching with
        | W.Watch_once _ | W.Watch_repeatedly _ -> true
        | W.Not_watching -> false
        | W.Stop_requested ->
          Read_write.set fd.Fd.watching read_or_write W.Not_watching;
          dec_num_active_syscalls_fd t fd;
          false)
    in
    if Debug.file_descr_watcher then
      Debug.log "File_descr_watcher.set" (fd.Fd.file_descr, desired, F.watcher)
        (<:sexp_of< File_descr.t * bool Read_write.t * F.t >>);
    F.set F.watcher fd.Fd.file_descr desired
  in
  let changed = t.fds_whose_watching_has_changed in
  t.fds_whose_watching_has_changed <- [];
  List.iter changed ~f:make_file_descr_watcher_agree_with;
;;

let be_the_scheduler ?(raise_unhandled_exn = false) t =
  let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
  Core_scheduler.set_thread_safe_finalizer_hook t.core_scheduler
    (fun () -> thread_safe_wakeup_scheduler t);
  t.scheduler_thread_id <- current_thread_id ();
  (* We handle [Signal.pipe] so that write() calls on a closed pipe/socket get EPIPE but
     the process doesn't die due to an unhandled SIGPIPE. *)
  Raw_signal_manager.manage t.signal_manager Signal.pipe;
  let compute_timeout () =
    if debug then Debug.log_string "compute_timeout";
    if Core_scheduler.num_pending_jobs t.core_scheduler > 0 then
      (* We want to timeout immediately if there are still jobs remaining, so that we
         immediately come back and start running them after checking for I/O. *)
      `Immediately
    else begin
      let timeout_after span =
        match t.timerfd with
        | None ->
          (* There is no timerfd, use the file descriptor watcher timeout. *)
          `After span
        | Some timerfd ->
          Linux_ext.Timerfd.set timerfd (`After span);
          (* Since the timerfd will handle the wakeup, the file-descr watcher doesn't have
             to. *)
          `Never
      in
      let max_inter_cycle_timeout =
        Max_inter_cycle_timeout.raw t.max_inter_cycle_timeout
      in
      match Core_scheduler.next_upcoming_event t.core_scheduler with
      | None -> timeout_after max_inter_cycle_timeout
      | Some time ->
        let now = Time.now () in
        if Time.(>) time now then
          timeout_after (Time.Span.min (Time.diff time now) max_inter_cycle_timeout)
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
          | None -> begin
              match t.timerfd with
              | Some tfd when file_descr = (tfd :> Unix.File_descr.t) -> begin
                  match read_or_write with
                  | `Read ->
                    (* We don't need to actually call [read] since we are using the
                       edge-triggered behavior. *)
                    ()
                  | `Write ->
                    failwiths
                      "File_descr_watcher returned the timerfd as ready to be written to"
                      file_descr (<:sexp_of< File_descr.t >>)
                end
              | _ ->
                failwiths "File_descr_watcher returned unknown file descr" file_descr
                  (<:sexp_of< File_descr.t >>)
            end
          | Some fd ->
            let module W = Fd.Watching in
            match Read_write.get fd.Fd.watching read_or_write with
            | W.Stop_requested -> ()
            | W.Not_watching ->
              failwiths "File_descr_watcher returned unwatched file descr" fd
                (<:sexp_of< Fd.t >>)
            | W.Watch_once ready_to ->
              Ivar.fill ready_to value;
              set_fd_desired_watching t fd read_or_write W.Stop_requested;
            | W.Watch_repeatedly (job, finished) ->
              match value with
              | `Ready -> Core_scheduler.add_job2 t.core_scheduler job
              | `Closed | `Bad_fd | `Interrupted as value ->
                Ivar.fill finished value;
                set_fd_desired_watching t fd read_or_write W.Stop_requested)
      in
      fill bad `Bad_fd;
      fill ready `Ready)
  in
  begin
    let interruptor_finished = Ivar.create () in
    let interruptor_read_fd = Interruptor.read_fd t.interruptor in
    let problem_with_interruptor () =
      failwiths "can not watch interruptor" (interruptor_read_fd, t) <:sexp_of< Fd.t * t >>
    in
    begin match
      request_start_watching t interruptor_read_fd `Read
        (Watching.Watch_repeatedly (Job.create Execution_context.main Fn.ignore (),
                                    interruptor_finished))
    with
    | `Already_watching | `Watching -> ()
    | `Unsupported | `Already_closed -> problem_with_interruptor ()
    end;
    upon (Ivar.read interruptor_finished) (fun _ -> problem_with_interruptor ());
  end;
  let rec loop () =
    (* At this point, we have the lock. *)
    if Core_scheduler.check_invariants t.core_scheduler then invariant t;
    match Core_scheduler.uncaught_exn t.core_scheduler with
    | Some error -> unlock t; error
    | None ->
      sync_changed_fds_to_file_descr_watcher t;
      if Debug.file_descr_watcher then
        Debug.log "File_descr_watcher.pre_check" t (<:sexp_of< t >>);
      let pre = F.pre_check F.watcher in
      (* [compute_timeout] must be the last thing before [thread_safe_check], because we
         want to make sure the timeout is zero if there are any scheduled jobs. *)
      let timeout = compute_timeout () in
      unlock t;
      if Debug.file_descr_watcher then
        Debug.log "File_descr_watcher.thread_safe_check" (timeout, t)
          (<:sexp_of< File_descr_watcher_intf.Timeout.t * t >>);
      let check_result = F.thread_safe_check F.watcher pre ~timeout in
      lock t;
      (* We call [Interruptor.clear] after [thread_safe_check] and before any of the
         processing that needs to happen in response to [thread_safe_interrupt].  That
         way, even if [Interruptor.clear] clears out an interrupt that hasn't been
         serviced yet, the interrupt will still be serviced by the immediately following
         processing. *)
      Interruptor.clear t.interruptor;
      if Debug.file_descr_watcher then
        Debug.log "File_descr_watcher.post_check" (check_result, t)
          (<:sexp_of< F.Check_result.t * t >>);
      begin match F.post_check F.watcher check_result with
      | `Timeout | `Syscall_interrupted -> ()
      | `Ok post ->
        if Debug.file_descr_watcher then
          Debug.log "File_descr_watcher.post_check returned" (post, t)
            (<:sexp_of< File_descr_watcher_intf.Post.t Read_write.t * t >>);
        handle_post post;
      end;
      if debug then Debug.log_string "handling delivered signals";
      Raw_signal_manager.handle_delivered t.signal_manager;
      have_lock_do_cycle t;
      loop ();
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
  Core_scheduler.add_finalizer t.core_scheduler heap_block f
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
  if t.have_called_go then failwith "cannot Scheduler.go more than once";
  t.have_called_go <- true;
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

let go_main
      ?raise_unhandled_exn
      ?file_descr_watcher
      ?max_num_open_file_descrs
      ?max_num_threads
      ~main () =
  if not (is_ready_to_initialize ()) then
    failwith "Async was initialized prior to [Scheduler.go_main]";
  let max_num_open_file_descrs =
    Option.map max_num_open_file_descrs ~f:Max_num_open_file_descrs.create_exn
  in
  let max_num_threads =
    Option.map max_num_threads ~f:Max_num_threads.create_exn
  in
  the_one_and_only_ref :=
    Ready_to_initialize (fun () ->
      create
        ?file_descr_watcher
        ?max_num_open_file_descrs
        ?max_num_threads
        ());
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

let set_check_invariants bool =
  Core_scheduler.(set_check_invariants (t ()) bool)
;;

let set_detect_invalid_access_from_thread bool =
  update_check_access (the_one_and_only ~should_lock:false) bool
;;

let set_record_backtraces bool =
  Core_scheduler.(set_record_backtraces (t ()) bool)
;;

let set_max_inter_cycle_timeout span =
  (the_one_and_only ~should_lock:false).max_inter_cycle_timeout <-
    Max_inter_cycle_timeout.create_exn span
;;

let start_busy_poller_thread_if_not_running t =
  if not t.busy_poll_thread_is_running then begin
    t.busy_poll_thread_is_running <- true;
    let core_scheduler = t.core_scheduler in
    let _thread : Thread.t =
      Thread.create (fun () ->
        let rec loop () =
          lock t;
          if Busy_pollers.is_empty t.busy_pollers then begin
            t.busy_poll_thread_is_running <- false;
            unlock t;
            (* We don't loop here, thus exiting the thread. *)
          end else begin
            Busy_pollers.poll t.busy_pollers;
            if Core_scheduler.num_pending_jobs core_scheduler > 0 then
              Core_scheduler.run_cycle core_scheduler;
            unlock t;
            (* This yield is necessary to allow other OCaml threads to run. *)
            Thread.yield ();
            loop ();
          end
        in
        loop ();
      ) ()
    in
    ()
  end;
;;

let add_busy_poller poll =
  let t = the_one_and_only ~should_lock:true in
  let result = Busy_pollers.add t.busy_pollers poll in
  start_busy_poller_thread_if_not_running t;
  result
;;

type 'b folder = { folder : 'a. 'b -> t -> (t, 'a) Field.t -> 'b }

let t () = the_one_and_only ~should_lock:true

let fold_fields (type a) ~init folder : a =
  let t = t () in
  let f ac field = folder.folder ac t field in
  Fields.fold ~init
    ~mutex:f
    ~is_running:f
    ~have_called_go:f
    ~fds_whose_watching_has_changed:f
    ~file_descr_watcher:f
    ~fd_by_descr:f
    ~timerfd:f
    ~scheduler_thread_id:f
    ~interruptor:f
    ~signal_manager:f
    ~thread_pool:f
    ~handle_thread_pool_stuck:f
    ~busy_poll_thread_is_running:f
    ~busy_pollers:f
    ~core_scheduler:f
    ~max_inter_cycle_timeout:f
;;

let handle_thread_pool_stuck f =
  let t = t () in
  t.handle_thread_pool_stuck <- unstage (Core_scheduler.preserve_execution_context f);
;;

