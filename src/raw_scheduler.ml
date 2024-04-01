open Core
open Import
module Fd = Raw_fd
module Watching = Fd.Watching
module Signal = Core.Signal
module Timerfd = Linux_ext.Timerfd
module Tsc = Time_stamp_counter

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
    [%sexp_of: Config.File_descr_watcher.t * F.t] (F.backend, F.watcher)
  ;;
end

module Which_watcher = struct
  module Custom = struct
    module type S =
      File_descr_watcher_intf.S
        with type 'a additional_create_args =
          handle_fd_read_bad:(File_descr.t -> unit)
          -> handle_fd_write_bad:(File_descr.t -> unit)
          -> 'a

    type t = (module S)
  end

  type t =
    | Config of Config.File_descr_watcher.t
    | Custom of Custom.t
end

module External_fd_event = struct
  type t =
    { file_descr : File_descr.t
    ; read_or_write : Read_write_pair.Key.t
    ; event_type : [ `Ready | `Bad_fd ] (* HUP is reported as `Ready *)
    }
end

module Thread_pool_stuck_status = struct
  type t =
    | No_unstarted_work
    | Stuck of
        { stuck_since : Time_ns.t
        ; num_work_completed : int
        }
  [@@deriving sexp_of]
end

include Async_kernel_scheduler

type start_type =
  | Not_started
  | Called_go
  | Called_block_on_async (* Thread_safe.block_on_async started the scheduler *)
  | Called_external_run of { active : bool ref }
[@@deriving sexp_of]

type t =
  { (* The scheduler [mutex] must be locked by all code that is manipulating scheduler
       data structures, which is almost all async code.  The [mutex] is automatically
       locked in the main thread when the scheduler is first created.  A [Nano_mutex]
       keeps track of which thread is holding the lock.  This means we can detect errors
       in which code incorrectly accesses async from a thread not holding the lock.  We do
       this when [detect_invalid_access_from_thread = true].  We also detect errors in
       which code tries to acquire the async lock while it already holds it, or releases
       the lock when it doesn't hold it. *)
    mutex : Nano_mutex.t
  ; mutable start_type : start_type
  ; (* [fds_whose_watching_has_changed] holds all fds whose watching has changed since
       the last time their desired state was set in the [file_descr_watcher]. *)
    fds_whose_watching_has_changed : Fd.t Stack.t
  ; file_descr_watcher : File_descr_watcher.t
  ; (* Returns how many events the poll has processed. *)
    busy_pollers : (Busy_poller.packed Uniform_array.t[@sexp.opaque])
  ; mutable num_busy_pollers : int
  ; mutable time_spent_waiting_for_io : Tsc.Span.t
  ; (* [fd_by_descr] holds every file descriptor that Async manages.  Fds are added
       when they are created, and removed when they transition to [Closed]. *)
    fd_by_descr : Fd.t By_descr.t
  ; (* [external_fd_by_descr] holds file descriptors registered via External.
       Async does no I/O on these, nor does it open or close them, but it reports
       readiness of them from External.run_one_cycle *)
    external_fd_by_descr : bool Read_write_pair.t By_descr.t
  ; (* [external_ready_fds] communicates the set of ready external file descriptors from
       [post_check_handle_fd] to [run_one_cycle], and is empty at other times *)
    mutable external_fd_events : (External_fd_event.t list[@sexp.opaque])
  ; (* If we are using a file descriptor watcher that does not support sub-millisecond
       timeout, [timerfd] contains a timerfd used to handle the next expiration.
       [timerfd_set_at] holds the the time at which [timerfd] is set to expire.  This
       lets us avoid calling [Time_ns.now] and [Linux_ext.Timerfd.set_after] unless
       we need to change that time. *)
    mutable timerfd : Linux_ext.Timerfd.t option
  ; mutable timerfd_set_at : Time_ns.t
  ; (* A distinguished thread, called the "scheduler" thread, is continually looping,
       checking file descriptors for I/O and then running a cycle.  It manages
       the [file_descr_watcher] and runs signal handlers.

       [scheduler_thread_id] is mutable because we create the scheduler before starting
       the scheduler running.  Once we start running the scheduler, [scheduler_thread_id]
       is set and never changes again. *)
    mutable scheduler_thread_id : int
  ; (* The [interruptor] is used to wake up the scheduler when it is blocked on the file
       descriptor watcher. *)
    interruptor : Interruptor.t
  ; signal_manager : Signal_manager.t
  ; (* The [thread_pool] is used for making blocking system calls in threads other than
       the scheduler thread, and for servicing [In_thread.run] requests. *)
    thread_pool : Thread_pool.t
  ; (* [handle_thread_pool_stuck] is called once per second if the thread pool is"stuck",
       i.e has not completed a job for one second and has no available threads. *)
    mutable handle_thread_pool_stuck : Thread_pool.t -> stuck_for:Time_ns.Span.t -> unit
  ; mutable thread_pool_stuck : Thread_pool_stuck_status.t
  ; dns_lookup_throttle : unit Throttle.t
      (* [dns_lookup_throttle] exists to prevent the entire thread pool from being used on DNS
     lookups. DNS is being special-cased here compared to other blocking operations as
     it's somewhat common for processes to do lots of concurrent DNS lookups, and DNS
     lookups can block for a long time, especially in the presence network unavailability.
  *)
  ; mutable next_tsc_calibration : Tsc.t
  ; kernel_scheduler : Kernel_scheduler.t
  ; (* [have_lock_do_cycle] is used to customize the implementation of running a cycle.
       E.g. in Ecaml it is set to something that causes Emacs to run a cycle. *)
    mutable have_lock_do_cycle : (unit -> unit) option (* configuration*)
  ; mutable max_inter_cycle_timeout : Max_inter_cycle_timeout.t
  ; mutable min_inter_cycle_timeout : Min_inter_cycle_timeout.t
  ; (* [initialized_at] is the call stack from when the scheduler was initialized.  It's
       generally more confusing than useful if it's shown on crash, so we omit it from the
       sexp. *)
    initialized_at : (Backtrace.t[@sexp.opaque])
  ; uring : (Io_uring_raw.t option[@sexp.opaque])
  }
[@@deriving fields ~iterators:(fold, iter), sexp_of]

let max_num_threads t = Thread_pool.max_num_threads t.thread_pool
let max_num_open_file_descrs t = By_descr.capacity t.fd_by_descr

let current_execution_context t =
  Kernel_scheduler.current_execution_context t.kernel_scheduler
;;

let with_execution_context t context ~f =
  Kernel_scheduler.with_execution_context t.kernel_scheduler context ~f
;;

let thread_pool_cpu_affinity t = Thread_pool.cpu_affinity t.thread_pool

let lock t =
  (* The following debug message is outside the lock, and so there can be races between
     multiple threads printing this message. *)
  if debug then Debug.log_string "waiting on lock";
  Nano_mutex.lock_exn t.mutex
;;

let try_lock t =
  match Nano_mutex.try_lock_exn t.mutex with
  | `Acquired -> true
  | `Not_acquired -> false
;;

let unlock t =
  if debug then Debug.log_string "lock released";
  Nano_mutex.unlock_exn t.mutex
;;

let with_lock t f =
  lock t;
  protect ~f ~finally:(fun () -> unlock t)
;;

let am_holding_lock t = Nano_mutex.current_thread_has_lock t.mutex

type the_one_and_only =
  | Not_ready_to_initialize of
      (* this [unit] makes the representation always be a pointer, thus
         making the pattern-match faster *)
      unit
  | Ready_to_initialize of (unit -> t)
  | Initialized of t

(* We use a mutex to protect creation of the one-and-only scheduler in the event that
   multiple threads attempt to call [the_one_and_only] simultaneously, which can
   happen in programs that are using [Thread_safe.run_in_async]. *)
let mutex_for_initializing_the_one_and_only_ref = Nano_mutex.create ()
let the_one_and_only_ref : the_one_and_only ref = ref (Not_ready_to_initialize ())

let is_ready_to_initialize () =
  match !the_one_and_only_ref with
  | Not_ready_to_initialize () | Initialized _ -> false
  | Ready_to_initialize _ -> true
;;

let is_initialized () =
  match !the_one_and_only_ref with
  | Initialized _ -> true
  | Not_ready_to_initialize () | Ready_to_initialize _ -> false
;;

(* Handling the uncommon cases in this function allows [the_one_and_only] to be inlined.
   The presence of a string constant (and the cold annotation) keeps this function from
   being inlined. *)
let[@cold] the_one_and_only_uncommon_case () =
  Nano_mutex.critical_section mutex_for_initializing_the_one_and_only_ref ~f:(fun () ->
    match !the_one_and_only_ref with
    | Initialized t -> t
    | Not_ready_to_initialize () ->
      raise_s [%message "Async the_one_and_only not ready to initialize"]
    | Ready_to_initialize f ->
      let t = f () in
      the_one_and_only_ref := Initialized t;
      t)
;;

let the_one_and_only () =
  match !the_one_and_only_ref with
  | Initialized t -> t
  | Not_ready_to_initialize () | Ready_to_initialize _ ->
    the_one_and_only_uncommon_case ()
;;

let fds_created_before_initialization = ref []

let create_fd_registration t fd =
  match By_descr.add t.fd_by_descr fd.Fd.file_descr fd with
  | Ok () -> ()
  | Error error ->
    let backtrace =
      if Ppx_inline_test_lib.am_running then None else Some (Backtrace.get ())
    in
    raise_s
      [%message
        "Async was unable to add a file descriptor to its table of open file descriptors"
          ~file_descr:(Fd.file_descr fd : File_descr.t)
          (error : Error.t)
          (backtrace : (Backtrace.t option[@sexp.option]))
          ~scheduler:
            (if Ppx_inline_test_lib.am_running then None else Some t
              : (t option[@sexp.option]))]
;;

let create_fd ?avoid_setting_nonblock kind file_descr info =
  (* We make it possible to create a writer without initializing the async scheduler, as
     this is something that happens a fair amount at toplevel of programs. *)
  let fd = Fd.create ?avoid_setting_nonblock kind file_descr info in
  if is_initialized ()
  then create_fd_registration (the_one_and_only ()) fd
  else
    Nano_mutex.critical_section mutex_for_initializing_the_one_and_only_ref ~f:(fun () ->
      if is_initialized ()
      then create_fd_registration (the_one_and_only ()) fd
      else fds_created_before_initialization := fd :: !fds_created_before_initialization);
  fd
;;

let current_thread_id () = Core_thread.(id (self ()))

(* OCaml runtime happens to assign the thread id of [0] to the main thread
   (the initial thread that starts the program). *)
let is_main_thread () = current_thread_id () = 0
let remove_fd t fd = By_descr.remove t.fd_by_descr fd.Fd.file_descr

let maybe_start_closing_fd t (fd : Fd.t) =
  if fd.num_active_syscalls = 0
  then (
    match fd.state with
    | Closed | Open _ -> ()
    | Close_requested (execution_context, do_close_syscall) ->
      (* We must remove the fd now and not after the close has finished.  If we waited
         until after the close had finished, then the fd might have already been
         reused by the OS and replaced. *)
      remove_fd t fd;
      Fd.set_state fd Closed;
      Kernel_scheduler.enqueue t.kernel_scheduler execution_context do_close_syscall ())
;;

let dec_num_active_syscalls_fd t (fd : Fd.t) =
  fd.num_active_syscalls <- fd.num_active_syscalls - 1;
  maybe_start_closing_fd t fd
;;

let invariant t : unit =
  try
    let check invariant field = invariant (Field.get field t) in
    Fields.iter
      ~mutex:ignore
      ~have_lock_do_cycle:ignore
      ~start_type:ignore
      ~fds_whose_watching_has_changed:
        (check (fun fds_whose_watching_has_changed ->
           Stack.iter fds_whose_watching_has_changed ~f:(fun (fd : Fd.t) ->
             assert fd.watching_has_changed;
             match By_descr.find t.fd_by_descr fd.file_descr with
             | None -> assert false
             | Some fd' -> assert (phys_equal fd fd'))))
      ~file_descr_watcher:
        (check (fun file_descr_watcher ->
           let module F = (val file_descr_watcher : File_descr_watcher.S) in
           F.invariant F.watcher;
           F.iter F.watcher ~f:(fun file_descr _ ->
             try
               match By_descr.find t.fd_by_descr file_descr with
               | None -> raise_s [%message "missing from fd_by_descr"]
               | Some fd -> assert (Fd.num_active_syscalls fd > 0)
             with
             | exn ->
               raise_s [%message "fd problem" (exn : exn) (file_descr : File_descr.t)])))
      ~busy_pollers:ignore
      ~num_busy_pollers:ignore
      ~time_spent_waiting_for_io:ignore
      ~fd_by_descr:
        (check (fun fd_by_descr ->
           By_descr.invariant fd_by_descr;
           By_descr.iter fd_by_descr ~f:(fun fd ->
             if fd.watching_has_changed
             then
               assert (
                 Stack.exists t.fds_whose_watching_has_changed ~f:(fun fd' ->
                   phys_equal fd fd')))))
      ~external_fd_by_descr:ignore
      ~external_fd_events:(check (fun fds -> assert (List.is_empty fds)))
      ~timerfd:ignore
      ~timerfd_set_at:ignore
      ~scheduler_thread_id:ignore
      ~interruptor:(check Interruptor.invariant)
      ~signal_manager:(check Signal_manager.invariant)
      ~thread_pool:(check Thread_pool.invariant)
      ~handle_thread_pool_stuck:ignore
      ~thread_pool_stuck:ignore
      ~dns_lookup_throttle:ignore
      ~next_tsc_calibration:ignore
      ~kernel_scheduler:(check Kernel_scheduler.invariant)
      ~max_inter_cycle_timeout:ignore
      ~min_inter_cycle_timeout:
        (check (fun min_inter_cycle_timeout ->
           assert (
             Time_ns.Span.( <= )
               (Min_inter_cycle_timeout.raw min_inter_cycle_timeout)
               (Max_inter_cycle_timeout.raw t.max_inter_cycle_timeout))))
      ~initialized_at:ignore
      ~uring:ignore
  with
  | exn -> raise_s [%message "Scheduler.invariant failed" (exn : exn) ~scheduler:(t : t)]
;;

let update_check_access t do_check =
  Kernel_scheduler.set_check_access
    t.kernel_scheduler
    (if not do_check
     then None
     else
       Some
         (fun () ->
           if not (am_holding_lock t)
           then (
             Debug.log
               "attempt to access Async from thread not holding the Async lock"
               (Backtrace.get (), t, Time.now ())
               [%sexp_of: Backtrace.t * t * Time.t];
             exit 1)))
;;

(* Try to create a timerfd.  It returns [None] if [Core] is not built with timerfd support
   or if it is not available on the current system. *)
let try_create_timerfd () =
  match Timerfd.create with
  | Error _ -> None
  | Ok create ->
    let clock = Timerfd.Clock.realtime in
    (try Some (create clock ~flags:Timerfd.Flags.(nonblock + cloexec)) with
     | Unix.Unix_error (ENOSYS, _, _) ->
       (* Kernel too old. *)
       None
     | Unix.Unix_error (EINVAL, _, _) ->
       (* Flags are only supported with Linux >= 2.6.27, try without them. *)
       let timerfd = create clock in
       Unix.set_close_on_exec (timerfd : Timerfd.t :> Unix.File_descr.t);
       Unix.set_nonblock (timerfd : Timerfd.t :> Unix.File_descr.t);
       Some timerfd)
;;

let default_handle_thread_pool_stuck thread_pool ~stuck_for =
  if Time_ns.Span.( >= ) stuck_for Config.report_thread_pool_stuck_for
  then (
    let should_abort =
      Time_ns.Span.( >= ) stuck_for Config.abort_after_thread_pool_stuck_for
    in
    let text = "Async's thread pool is stuck" in
    let text =
      if should_abort
      then text
      else
        sprintf
          "%s, and will raise an exception in %s"
          text
          (Time_ns.Span.to_short_string
             (Time_ns.Span.( - ) Config.abort_after_thread_pool_stuck_for stuck_for))
    in
    let message =
      [%message
        ""
          ~_:(if am_running_test then Time_ns.epoch else Time_ns.now () : Time_ns.t)
          text
          ~stuck_for:(Time_ns.Span.to_short_string stuck_for : string)
          ~num_threads_created:(Thread_pool.num_threads thread_pool : int)
          ~max_num_threads:(Thread_pool.max_num_threads thread_pool : int)
          ~last_thread_creation_failure:
            (Thread_pool.last_thread_creation_failure thread_pool
              : (Sexp.t option[@sexp.option]))]
    in
    if should_abort
    then (
      (* Core dumps are exceptionally useful when investigating thread pool stuck errors,
         since they give you access to all the call stacks that got stuck, so dump it here
         (core dump in [be_the_scheduler] is not kicking in here because this error is
         classified as [`User_uncaught], not [`Async_uncaught]).

         Not dumping when [am_running_test] to avoid potentially making some existing
         tests slower in case they are deliberately testing this scenario. *)
      if not am_running_test then Dump_core_on_job_delay.dump_core ();
      Monitor.send_exn Monitor.main (Error.to_exn (Error.create_s message)))
    else Core.Debug.eprint_s message)
;;

let thread_pool_has_unfinished_work t = Thread_pool.unfinished_work t.thread_pool <> 0
let thread_safe_wakeup_scheduler t = Interruptor.thread_safe_interrupt t.interruptor
let i_am_the_scheduler t = current_thread_id () = t.scheduler_thread_id

let set_fd_desired_watching t (fd : Fd.t) read_or_write desired =
  Read_write_pair.set fd.watching read_or_write desired;
  if not fd.watching_has_changed
  then (
    fd.watching_has_changed <- true;
    Stack.push t.fds_whose_watching_has_changed fd)
;;

let give_up_on_watching t fd read_or_write (watching : Watching.t) =
  if Debug.file_descr_watcher
  then
    Debug.log
      "give_up_on_watching"
      (read_or_write, fd, t)
      [%sexp_of: Read_write_pair.Key.t * Fd.t * t];
  match watching with
  | Stop_requested | Not_watching -> ()
  | Watch_once ready_to ->
    Ivar.fill_exn ready_to `Unsupported;
    set_fd_desired_watching t fd read_or_write Stop_requested
  | Watch_repeatedly (job, finished) ->
    Kernel_scheduler.free_job t.kernel_scheduler job;
    Ivar.fill_exn finished `Unsupported;
    set_fd_desired_watching t fd read_or_write Stop_requested
;;

let request_start_watching t fd read_or_write watching =
  if Debug.file_descr_watcher
  then
    Debug.log
      "request_start_watching"
      (read_or_write, fd, t)
      [%sexp_of: Read_write_pair.Key.t * Fd.t * t];
  let result =
    match Read_write_pair.get fd.watching read_or_write with
    | Watch_once _ | Watch_repeatedly _ -> `Already_watching
    | Stop_requested ->
      (* We don't [inc_num_active_syscalls] in this case, because we already did when we
         transitioned from [Not_watching] to [Watching].  Also, it is possible that [fd]
         was closed since we transitioned to [Stop_requested], in which case we don't want
         to [start_watching]; we want to report that it was closed and leave it
         [Stop_requested] so the the file-descr-watcher will stop watching it and we can
         actually close it. *)
      if Fd.is_closed fd then `Already_closed else `Watching
    | Not_watching ->
      (match Fd.inc_num_active_syscalls fd with
       | `Already_closed -> `Already_closed
       | `Ok -> `Watching)
  in
  (match result with
   | `Already_closed | `Already_watching -> ()
   | `Watching ->
     set_fd_desired_watching t fd read_or_write watching;
     if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t);
  result
;;

let request_stop_watching t fd read_or_write value =
  if Debug.file_descr_watcher
  then
    Debug.log
      "request_stop_watching"
      (read_or_write, value, fd, t)
      [%sexp_of: Read_write_pair.Key.t * Fd.ready_to_result * Fd.t * t];
  match Read_write_pair.get fd.watching read_or_write with
  | Stop_requested | Not_watching -> ()
  | Watch_once ready_to ->
    Ivar.fill_exn ready_to value;
    set_fd_desired_watching t fd read_or_write Stop_requested;
    if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t
  | Watch_repeatedly (job, finished) ->
    (match value with
     | `Ready -> Kernel_scheduler.enqueue_job t.kernel_scheduler job ~free_job:false
     | (`Closed | `Bad_fd | `Interrupted | `Unsupported) as value ->
       Kernel_scheduler.free_job t.kernel_scheduler job;
       Ivar.fill_exn finished value;
       set_fd_desired_watching t fd read_or_write Stop_requested;
       if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t)
;;

let[@cold] post_check_got_timerfd file_descr =
  raise_s
    [%message
      "File_descr_watcher returned the timerfd as ready to be written to"
        (file_descr : File_descr.t)]
;;

let[@cold] post_check_invalid_fd file_descr =
  raise_s
    [%message
      "File_descr_watcher returned unknown file descr" (file_descr : File_descr.t)]
;;

let post_check_handle_fd t file_descr read_or_write (event_type : [ `Ready | `Bad_fd ]) =
  if By_descr.mem t.fd_by_descr file_descr
  then (
    let fd = By_descr.find_exn t.fd_by_descr file_descr in
    request_stop_watching t fd read_or_write (event_type :> Fd.ready_to_result))
  else (
    match t.timerfd with
    | Some tfd when File_descr.equal file_descr (tfd :> Unix.File_descr.t) ->
      (match read_or_write with
       | `Read ->
         (* We don't need to actually call [read] since we are using the
            edge-triggered behavior. *)
         ()
       | `Write -> post_check_got_timerfd file_descr)
    | _ ->
      if By_descr.mem t.external_fd_by_descr file_descr
      then (
        let ev : External_fd_event.t = { file_descr; read_or_write; event_type } in
        t.external_fd_events <- ev :: t.external_fd_events)
      else post_check_invalid_fd file_descr)
;;

external magic_trace_long_async_cycle : unit -> unit = "magic_trace_long_async_cycle"
  [@@noalloc]

let cycle_took_longer_than_100us =
  let open Bool.Non_short_circuiting in
  let ( > ) = Time_ns.Span.( > ) in
  let too_long = ref false in
  let[@inline] too_long_if bool = too_long := !too_long || bool in
  fun [@inline never] ~cycle_time ->
    too_long := false;
    [%probe "magic_trace_async_cycle_longer_than_100us" (too_long := true)];
    [%probe
      "magic_trace_async_cycle_longer_than_300us"
        (too_long_if (cycle_time > Time_ns.Span.of_int_us 300))];
    [%probe
      "magic_trace_async_cycle_longer_than_1ms"
        (too_long_if (cycle_time > Time_ns.Span.of_int_ms 1))];
    [%probe
      "magic_trace_async_cycle_longer_than_3ms"
        (too_long_if (cycle_time > Time_ns.Span.of_int_ms 3))];
    [%probe
      "magic_trace_async_cycle_longer_than_10ms"
        (too_long_if (cycle_time > Time_ns.Span.of_int_ms 10))];
    [%probe
      "magic_trace_async_cycle_longer_than_30ms"
        (too_long_if (cycle_time > Time_ns.Span.of_int_ms 30))];
    [%probe
      "magic_trace_async_cycle_longer_than_100ms"
        (too_long_if (cycle_time > Time_ns.Span.of_int_ms 100))];
    [%probe
      "magic_trace_async_cycle_longer_than_300ms"
        (too_long_if (cycle_time > Time_ns.Span.of_int_ms 300))];
    [%probe
      "magic_trace_async_cycle_longer_than_1s"
        (too_long_if (cycle_time > Time_ns.Span.of_int_sec 1))];
    [%probe
      "magic_trace_async_cycle_longer_than_3s"
        (too_long_if (cycle_time > Time_ns.Span.of_int_sec 3))];
    [%probe
      "magic_trace_async_cycle_longer_than_10s"
        (too_long_if (cycle_time > Time_ns.Span.of_int_sec 10))];
    [%probe
      "magic_trace_async_cycle_longer_than_30s"
        (too_long_if (cycle_time > Time_ns.Span.of_int_sec 30))];
    [%probe
      "magic_trace_async_cycle_longer_than_1m"
        (too_long_if (cycle_time > Time_ns.Span.of_int_sec 60))];
    if !too_long then magic_trace_long_async_cycle ()
;;

let[@inline] maybe_report_long_async_cycles_to_magic_trace ~cycle_time =
  let ( > ) = Time_ns.Span.( > ) in
  (* This first check is lifted out to avoid the linear scan through each probe during
     short async cycles. *)
  if cycle_time > Time_ns.Span.of_int_us 100 then cycle_took_longer_than_100us ~cycle_time
;;

let%test_unit ("maybe_report_long_async_cycles_to_magic_trace doesn't allocate" [@tags
                                                                                  "64-bits-only"])
  =
  let cycle_time = Time_ns.Span.of_int_sec 15 in
  let words_before = Gc.major_plus_minor_words () in
  maybe_report_long_async_cycles_to_magic_trace ~cycle_time;
  let words_after = Gc.major_plus_minor_words () in
  [%test_result: int] (words_after - words_before) ~expect:0
;;

let[@inline] maybe_report_long_async_cycles_to_magic_trace t : unit =
  maybe_report_long_async_cycles_to_magic_trace
    ~cycle_time:t.kernel_scheduler.last_cycle_time
;;

let create
  ~mutex
  ?(thread_pool_cpu_affinity = Config.thread_pool_cpu_affinity)
  ?(file_descr_watcher = Which_watcher.Config Config.file_descr_watcher)
  ?(max_num_open_file_descrs = Config.max_num_open_file_descrs)
  ?(max_num_threads = Config.max_num_threads)
  ()
  =
  if debug then Debug.log_string "creating scheduler";
  let thread_pool =
    ok_exn
      (Thread_pool.create
         ()
         ~cpu_affinity:thread_pool_cpu_affinity
         ~max_num_threads:(Max_num_threads.raw max_num_threads))
  in
  let num_file_descrs = Max_num_open_file_descrs.raw max_num_open_file_descrs in
  let fd_by_descr = By_descr.create ~num_file_descrs in
  let create_fd kind file_descr info =
    let fd = Fd.create kind file_descr info in
    ok_exn (By_descr.add fd_by_descr fd.Fd.file_descr fd);
    fd
  in
  let external_fd_by_descr = By_descr.create ~num_file_descrs in
  let interruptor = Interruptor.create ~create_fd in
  let t_ref = ref None in
  (* set below, after [t] is defined *)
  let handle_fd read_or_write ready_or_bad_fd file_descr =
    match !t_ref with
    | None -> assert false
    | Some t -> post_check_handle_fd t file_descr read_or_write ready_or_bad_fd
  in
  let handle_fd_read_ready = handle_fd `Read `Ready in
  let handle_fd_read_bad = handle_fd `Read `Bad_fd in
  let handle_fd_write_ready = handle_fd `Write `Ready in
  let handle_fd_write_bad = handle_fd `Write `Bad_fd in
  let file_descr_watcher, timerfd, uring =
    match file_descr_watcher with
    | Custom (module Custom) ->
      let watcher =
        Custom.create
          ~num_file_descrs
          ~handle_fd_read_ready
          ~handle_fd_read_bad
          ~handle_fd_write_ready
          ~handle_fd_write_bad
      in
      let module W = struct
        include Custom

        let watcher = watcher
      end
      in
      (module W : File_descr_watcher.S), None, None
    | Config Select ->
      let watcher =
        Select_file_descr_watcher.create
          ~num_file_descrs
          ~handle_fd_read_ready
          ~handle_fd_read_bad
          ~handle_fd_write_ready
          ~handle_fd_write_bad
      in
      let module W = struct
        include Select_file_descr_watcher

        let watcher = watcher
      end
      in
      (module W : File_descr_watcher.S), None, None
    | Config (Epoll | Epoll_if_timerfd) ->
      let timerfd =
        match try_create_timerfd () with
        | None ->
          raise_s
            [%message
              {|Async refuses to run using epoll on a system that doesn't support timer FDs, since
Async will be unable to timeout with sub-millisecond precision.|}]
        | Some timerfd -> timerfd
      in
      let watcher =
        Epoll_file_descr_watcher.create
          ~num_file_descrs
          ~timerfd
          ~handle_fd_read_ready
          ~handle_fd_write_ready
      in
      let module W = struct
        include Epoll_file_descr_watcher

        let watcher = watcher
      end
      in
      (module W : File_descr_watcher.S), Some timerfd, None
    | Config Io_uring ->
      let uring =
        Io_uring_raw.create
          ~queue_depth:
            (Io_uring_max_submission_entries.raw Config.io_uring_max_submission_entries)
          ()
        |> Or_error.ok_exn
      in
      let watcher =
        Io_uring_file_descr_watcher.create
          ~uring
          ~num_file_descrs
          ~handle_fd_read_ready
          ~handle_fd_write_ready
      in
      let module W = struct
        include Io_uring_file_descr_watcher

        let watcher = watcher
      end
      in
      (module W : File_descr_watcher.S), None, Some uring
  in
  let dns_lookup_throttle =
    let max_concurrent_dns_lookups =
      Int.max (Thread_pool.max_num_threads thread_pool / 2) 1
    in
    Throttle.create
      ~continue_on_error:true
      ~max_concurrent_jobs:max_concurrent_dns_lookups
  in
  let kernel_scheduler = Kernel_scheduler.t () in
  let t =
    { mutex
    ; start_type = Not_started
    ; fds_whose_watching_has_changed = Stack.create ()
    ; file_descr_watcher
    ; busy_pollers = Uniform_array.create ~len:256 Busy_poller.empty
    ; num_busy_pollers = 0
    ; time_spent_waiting_for_io = Tsc.Span.of_int_exn 0
    ; fd_by_descr
    ; external_fd_by_descr
    ; external_fd_events = []
    ; timerfd
    ; timerfd_set_at = Time_ns.max_value_for_1us_rounding
    ; scheduler_thread_id = -1 (* set when [be_the_scheduler] is called *)
    ; interruptor
    ; signal_manager =
        Signal_manager.create ~thread_safe_notify_signal_delivered:(fun () ->
          Interruptor.thread_safe_interrupt interruptor)
    ; thread_pool
    ; handle_thread_pool_stuck = default_handle_thread_pool_stuck
    ; thread_pool_stuck = No_unstarted_work
    ; dns_lookup_throttle
    ; next_tsc_calibration = Tsc.now ()
    ; kernel_scheduler
    ; have_lock_do_cycle = None
    ; max_inter_cycle_timeout = Config.max_inter_cycle_timeout
    ; min_inter_cycle_timeout = Config.min_inter_cycle_timeout
    ; initialized_at = Backtrace.get ()
    ; uring
    }
  in
  t_ref := Some t;
  update_check_access t Config.detect_invalid_access_from_thread;
  List.iter (List.rev !fds_created_before_initialization) ~f:(create_fd_registration t);
  fds_created_before_initialization := [];
  t
;;

let init ~take_the_lock =
  let mutex = Nano_mutex.create () in
  if take_the_lock
  then
    (* We create a mutex that's initially locked by the main thread to support the
       case when the user does async stuff at the top level before calling
       [Scheduler.go].  This lock makes sure that async jobs don't run until
       [Scheduler.go] is called.  That could happen, e.g. by creating a reader that
       does a read system call in another (true) thread.  The scheduler remains
       locked until the scheduler unlocks it. *)
    Nano_mutex.lock_exn mutex;
  the_one_and_only_ref := Ready_to_initialize (fun () -> create ~mutex ())
;;

let () = init ~take_the_lock:true

let reset_in_forked_process ~take_the_lock =
  (match !the_one_and_only_ref with
   | Not_ready_to_initialize () | Ready_to_initialize _ -> ()
   | Initialized { file_descr_watcher; timerfd; _ } ->
     let module F = (val file_descr_watcher : File_descr_watcher.S) in
     F.reset_in_forked_process F.watcher;
     (match timerfd with
      | None -> ()
      | Some tfd -> Unix.close (tfd :> Unix.File_descr.t)));
  Kernel_scheduler.reset_in_forked_process ();
  fds_created_before_initialization := [];
  init ~take_the_lock
;;

let reset_in_forked_process_without_taking_lock () =
  reset_in_forked_process ~take_the_lock:false
;;

let reset_in_forked_process () = reset_in_forked_process ~take_the_lock:true

let make_async_unusable () =
  reset_in_forked_process ();
  Kernel_scheduler.make_async_unusable ();
  the_one_and_only_ref
    := Ready_to_initialize
         (fun () ->
           raise_s [%sexp "Async is unusable due to [Scheduler.make_async_unusable]"])
;;

let thread_safe_enqueue_external_job t f =
  Kernel_scheduler.thread_safe_enqueue_external_job t.kernel_scheduler f
;;

let have_lock_do_cycle t =
  if debug then Debug.log "have_lock_do_cycle" t [%sexp_of: t];
  match t.have_lock_do_cycle with
  | Some f -> f ()
  | None ->
    Kernel_scheduler.run_cycle t.kernel_scheduler;
    maybe_report_long_async_cycles_to_magic_trace t;
    (* If we are not the scheduler, wake it up so it can process any remaining jobs, clock
       events, or an unhandled exception. *)
    if not (i_am_the_scheduler t) then thread_safe_wakeup_scheduler t
;;

let[@cold] log_sync_changed_fds_to_file_descr_watcher t file_descr desired =
  let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
  Debug.log
    "File_descr_watcher.set"
    (file_descr, desired, F.watcher)
    [%sexp_of: File_descr.t * bool Read_write_pair.t * F.t]
;;

let[@cold] sync_changed_fd_failed t fd desired exn =
  let bt = Backtrace.Exn.most_recent () in
  raise_s
    [%message
      "sync_changed_fds_to_file_descr_watcher unable to set fd"
        (desired : bool Read_write_pair.t)
        (fd : Fd.t)
        (exn : exn)
        (bt : Backtrace.t)
        ~scheduler:(t : t)]
;;

let sync_changed_fds_to_file_descr_watcher t =
  (* We efficiently do nothing if nothing has changed, avoiding even the definition of
     [module F], which can have some cost. *)
  if not (Stack.is_empty t.fds_whose_watching_has_changed)
  then
    let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
    while not (Stack.is_empty t.fds_whose_watching_has_changed) do
      let fd = Stack.pop_exn t.fds_whose_watching_has_changed in
      fd.watching_has_changed <- false;
      let desired =
        Read_write_pair.map fd.watching ~f:(fun watching ->
          match watching with
          | Watch_once _ | Watch_repeatedly _ -> true
          | Not_watching | Stop_requested -> false)
      in
      if Debug.file_descr_watcher
      then log_sync_changed_fds_to_file_descr_watcher t fd.file_descr desired;
      match
        try F.set F.watcher fd.file_descr desired with
        | exn -> sync_changed_fd_failed t fd desired exn
      with
      | `Unsupported -> Read_write_pair.iteri fd.watching ~f:(give_up_on_watching t fd)
      | `Ok ->
        (* We modify Async's data structures after calling [F.set], so that
           the error message produced by [sync_changed_fd_failed] displays
           them as they were before the call. *)
        Read_write_pair.iteri fd.watching ~f:(fun read_or_write watching ->
          match watching with
          | Watch_once _ | Watch_repeatedly _ | Not_watching -> ()
          | Stop_requested ->
            Read_write_pair.set fd.watching read_or_write Not_watching;
            dec_num_active_syscalls_fd t fd)
    done
;;

let maybe_calibrate_tsc t =
  if Lazy.is_val Tsc.calibrator
  then (
    let now = Tsc.now () in
    if Tsc.( >= ) now t.next_tsc_calibration
    then (
      let calibrator = force Tsc.calibrator in
      Tsc.Calibrator.calibrate calibrator;
      t.next_tsc_calibration
        <- Tsc.add now (Tsc.Span.of_ns (Int63.of_int 1_000_000_000) ~calibrator)))
;;

let create_job ?execution_context t f x =
  let execution_context =
    match execution_context with
    | Some e -> e
    | None -> current_execution_context t
  in
  Kernel_scheduler.create_job t.kernel_scheduler execution_context f x
;;

let dump_core_on_job_delay () =
  match Config.dump_core_on_job_delay with
  | Do_not_watch -> ()
  | Watch { dump_if_delayed_by; how_to_dump } ->
    Dump_core_on_job_delay.start_watching
      ~dump_if_delayed_by:(Time_ns.Span.to_span_float_round_nearest dump_if_delayed_by)
      ~how_to_dump
;;

let num_busy_pollers t = t.num_busy_pollers

let add_busy_poller t ~max_busy_wait_duration f =
  if t.num_busy_pollers = Uniform_array.length t.busy_pollers
  then raise_s [%message "[add_busy_poller] maximum number of pollers exceeded"];
  Uniform_array.set t.busy_pollers t.num_busy_pollers f;
  t.num_busy_pollers <- t.num_busy_pollers + 1;
  t.max_inter_cycle_timeout
    <- Max_inter_cycle_timeout.create_exn
         (Time_ns.Span.min
            (Max_inter_cycle_timeout.raw t.max_inter_cycle_timeout)
            max_busy_wait_duration)
;;

let init t =
  dump_core_on_job_delay ();
  Kernel_scheduler.set_thread_safe_external_job_hook t.kernel_scheduler (fun () ->
    thread_safe_wakeup_scheduler t);
  t.scheduler_thread_id <- current_thread_id ();
  (* We handle [Signal.pipe] so that write() calls on a closed pipe/socket get EPIPE but
     the process doesn't die due to an unhandled SIGPIPE. *)
  Signal_manager.manage t.signal_manager Signal.pipe;
  let interruptor_finished = Ivar.create () in
  let interruptor_read_fd = Interruptor.read_fd t.interruptor in
  let problem_with_interruptor () =
    raise_s
      [%message
        "can not watch interruptor" (interruptor_read_fd : Fd.t) ~scheduler:(t : t)]
  in
  (match
     request_start_watching
       t
       interruptor_read_fd
       `Read
       (Watch_repeatedly
          ( Kernel_scheduler.create_job
              t.kernel_scheduler
              Execution_context.main
              Fn.ignore
              ()
          , interruptor_finished ))
   with
   | `Already_watching | `Watching -> ()
   | `Unsupported | `Already_closed -> problem_with_interruptor ());
  upon (Ivar.read interruptor_finished) (fun _ -> problem_with_interruptor ())
;;

let fds_may_produce_events t =
  let interruptor_fd = Interruptor.read_fd t.interruptor in
  By_descr.exists t.fd_by_descr ~f:(fun fd ->
    (* Jobs created by the interruptor don't do anything, so we don't need to
       count them as something that can drive progress. When interruptor is involved, the
       progress is driven by other modules (e.g. the thread_pool).
       The caller should inspect those directly.

       We don't need a similar special-case for [timerfd] because that's never added
       to [fd_by_descr], in the first place.
    *)
    (not (Fd.equal fd interruptor_fd))
    && Read_write_pair.exists (Fd.watching fd) ~f:(fun watching ->
         match (watching : Fd.Watching.t) with
         | Not_watching -> false
         (* Stop_requested will enqueue a single job, so we have jobs to do still at this point. *)
         | Watch_once _ | Watch_repeatedly _ | Stop_requested -> true))
;;

(* We avoid allocation in [check_file_descr_watcher], since it is called every time in
   the scheduler loop. *)
let check_file_descr_watcher t ~timeout span_or_unit =
  let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
  if Debug.file_descr_watcher
  then Debug.log "File_descr_watcher.pre_check" t [%sexp_of: t];
  let pre = F.pre_check F.watcher in
  unlock t;
  (* We yield so that other OCaml threads (especially thread-pool threads) get a chance to
     run.  This is a good point to yield, because we do not hold the Async lock, which
     allows other threads to acquire it.  [Thread.yield] only yields if other OCaml
     threads are waiting to acquire the OCaml lock, and is fast if not.  As of OCaml 4.07,
     [Thread.yield] on Linux calls [nanosleep], which causes the Linux scheduler to
     actually switch to other threads. *)
  Thread.yield ();
  if Debug.file_descr_watcher
  then
    Debug.log
      "File_descr_watcher.thread_safe_check"
      (File_descr_watcher_intf.Timeout.variant_of timeout span_or_unit, t)
      [%sexp_of: [ `Immediately | `After of Time_ns.Span.t ] * t];
  let before = Tsc.now () in
  let check_result = F.thread_safe_check F.watcher pre timeout span_or_unit in
  let after = Tsc.now () in
  t.time_spent_waiting_for_io
    <- Tsc.Span.( + ) t.time_spent_waiting_for_io (Tsc.diff after before);
  lock t;
  (* We call [Interruptor.clear] after [thread_safe_check] and before any of the
     processing that needs to happen in response to [thread_safe_interrupt].  That
     way, even if [Interruptor.clear] clears out an interrupt that hasn't been
     serviced yet, the interrupt will still be serviced by the immediately following
     processing. *)
  Interruptor.clear t.interruptor;
  if Debug.file_descr_watcher
  then
    Debug.log
      "File_descr_watcher.post_check"
      (check_result, t)
      [%sexp_of: F.Check_result.t * t];
  F.post_check F.watcher check_result
;;

let[@inline always] run_busy_pollers_once t ~deadline =
  let did_work = ref false in
  (try
     for i = 0 to t.num_busy_pollers - 1 do
       let poller = Uniform_array.unsafe_get t.busy_pollers i in
       if Busy_poller.poll poller ~deadline > 0 then did_work := true
     done
   with
   | exn -> Monitor.send_exn Monitor.main exn);
  !did_work
;;

let run_busy_pollers t ~timeout =
  let calibrator = force Tsc.calibrator in
  let deadline =
    ref (Tsc.add (Tsc.now ()) (Tsc.Span.of_time_ns_span timeout ~calibrator))
  in
  while
    let pollers_did_something = run_busy_pollers_once t ~deadline:!deadline in
    let now = Tsc.now () in
    if pollers_did_something
    then
      if Kernel_scheduler.can_run_a_job t.kernel_scheduler
      then deadline := now
      else if Kernel_scheduler.has_upcoming_event t.kernel_scheduler
      then (
        let new_timeout =
          Time_ns.diff
            (Kernel_scheduler.next_upcoming_event_exn t.kernel_scheduler)
            (Tsc.to_time_ns now ~calibrator)
          |> Tsc.Span.of_time_ns_span ~calibrator
        in
        deadline := Tsc.min !deadline (Tsc.add now new_timeout));
    Tsc.( < ) now !deadline
  do
    ()
  done
;;

(* We compute the timeout as the last thing before [check_file_descr_watcher], because
   we want to make sure the timeout is zero if there are any scheduled jobs.  The code
   is structured to avoid calling [Time_ns.now] and [Linux_ext.Timerfd.set_*] if
   possible.  In particular, we only call [Time_ns.now] if we need to compute the
   timeout-after span.  And we only call [Linux_ext.Timerfd.set_after] if the time that
   we want it to fire is different than the time it is already set to fire. *)
let compute_timeout_and_check_file_descr_watcher t =
  let min_inter_cycle_timeout = (t.min_inter_cycle_timeout :> Time_ns.Span.t) in
  let max_inter_cycle_timeout = (t.max_inter_cycle_timeout :> Time_ns.Span.t) in
  let have_busy_pollers = t.num_busy_pollers > 0 in
  let file_descr_watcher_timeout =
    match t.timerfd, have_busy_pollers with
    | None, _ | Some _, true ->
      (* Since there is no timerfd, use the file descriptor watcher timeout. *)
      if Kernel_scheduler.can_run_a_job t.kernel_scheduler
      then min_inter_cycle_timeout
      else if not (Kernel_scheduler.has_upcoming_event t.kernel_scheduler)
      then max_inter_cycle_timeout
      else (
        let next_event_at = Kernel_scheduler.next_upcoming_event_exn t.kernel_scheduler in
        Time_ns.Span.min
          max_inter_cycle_timeout
          (Time_ns.Span.max
             min_inter_cycle_timeout
             (Time_ns.diff next_event_at (Time_ns.now ()))))
    | Some timerfd, false ->
      (* Set [timerfd] to fire if necessary, taking into account [can_run_a_job],
         [min_inter_cycle_timeout], and [next_event_at]. *)
      let have_min_inter_cycle_timeout =
        Time_ns.Span.( > ) min_inter_cycle_timeout Time_ns.Span.zero
      in
      if Kernel_scheduler.can_run_a_job t.kernel_scheduler
      then
        if not have_min_inter_cycle_timeout
        then Time_ns.Span.zero
        else (
          t.timerfd_set_at <- Time_ns.max_value_for_1us_rounding;
          Linux_ext.Timerfd.set_after timerfd min_inter_cycle_timeout;
          max_inter_cycle_timeout)
      else if not (Kernel_scheduler.has_upcoming_event t.kernel_scheduler)
      then max_inter_cycle_timeout
      else (
        let next_event_at = Kernel_scheduler.next_upcoming_event_exn t.kernel_scheduler in
        let set_timerfd_at =
          if not have_min_inter_cycle_timeout
          then next_event_at
          else
            Time_ns.max
              next_event_at
              (Time_ns.add (Time_ns.now ()) min_inter_cycle_timeout)
        in
        if not (Time_ns.equal t.timerfd_set_at set_timerfd_at)
        then (
          t.timerfd_set_at <- set_timerfd_at;
          Linux_ext.Timerfd.set_at timerfd set_timerfd_at);
        max_inter_cycle_timeout)
  in
  if Time_ns.Span.( <= ) file_descr_watcher_timeout Time_ns.Span.zero
  then (
    ignore (run_busy_pollers_once t ~deadline:Tsc.zero : bool);
    check_file_descr_watcher t ~timeout:Immediately ())
  else if have_busy_pollers
  then (
    run_busy_pollers t ~timeout:file_descr_watcher_timeout;
    check_file_descr_watcher t ~timeout:Immediately ())
  else check_file_descr_watcher t ~timeout:After file_descr_watcher_timeout
;;

let one_iter t =
  if Kernel_scheduler.check_invariants t.kernel_scheduler then invariant t;
  maybe_calibrate_tsc t;
  sync_changed_fds_to_file_descr_watcher t;
  compute_timeout_and_check_file_descr_watcher t;
  if debug then Debug.log_string "handling delivered signals";
  Signal_manager.handle_delivered t.signal_manager;
  have_lock_do_cycle t;
  Kernel_scheduler.uncaught_exn t.kernel_scheduler
;;

let be_the_scheduler ?(raise_unhandled_exn = false) t =
  init t;
  let rec loop () =
    match one_iter t with
    | Some error -> error
    | None -> loop ()
  in
  let error_kind, error =
    try `User_uncaught, loop () with
    | exn ->
      unlock t;
      `Async_uncaught, Error.create "bug in async scheduler" (exn, t) [%sexp_of: exn * t]
  in
  if raise_unhandled_exn
  then Error.raise error
  else (
    (* One reason to run [do_at_exit] handlers before printing out the error message is
       that it helps curses applications bring the terminal in a good state, otherwise
       the error message might get corrupted.  Also, the OCaml top-level uncaught
       exception handler does the same. *)
    (try Stdlib.do_at_exit () with
     | _ -> ());
    (match error_kind with
     | `User_uncaught ->
       (* Don't use Debug.log, to avoid redundant error (task_id in particular) *)
       eprintf !"%{Sexp#hum}\n%!" [%sexp (Time_ns.now () : Time_ns.t), (error : Error.t)]
     | `Async_uncaught ->
       Debug.log "unhandled exception in Async scheduler" error [%sexp_of: Error.t];
       Debug.log_string "dumping core";
       Dump_core_on_job_delay.dump_core ());
    Unix.exit_immediately 1)
;;

let add_finalizer t heap_block f =
  Kernel_scheduler.add_finalizer t.kernel_scheduler heap_block f
;;

let add_finalizer_exn t x f =
  add_finalizer t (Heap_block.create_exn x) (fun heap_block ->
    f (Heap_block.value heap_block))
;;

let async_kernel_config_task_id () =
  let pid = Unix.getpid () in
  let thread_id = Thread.id (Thread.self ()) in
  [%sexp_of: [ `pid of Pid.t ] * [ `thread_id of int ]] (`pid pid, `thread_id thread_id)
;;

let set_task_id () = Async_kernel_config.task_id := async_kernel_config_task_id

let raise_if_any_jobs_were_scheduled () =
  match Kernel_scheduler.backtrace_of_first_job (Kernel_scheduler.t ()) with
  | None -> ()
  | Some bt ->
    raise_s
      [%sexp
        "error: program is attempting to schedule async work too soon (at toplevel of a \
         library, usually)"
        , (bt : Backtrace.t)]
;;

let is_running t =
  match t.start_type with
  | Not_started -> false
  | _ -> true
;;

let go ?raise_unhandled_exn () =
  if debug then Debug.log_string "Scheduler.go";
  set_task_id ();
  let t = the_one_and_only () in
  (* [go] can be called from a thread other than the main thread, for example in programs
     that reset scheduler after fork, so in some cases it must acquire the lock if
     the thread has not already done so. *)
  if not (am_holding_lock t) then lock t;
  match t.start_type with
  | Not_started ->
    t.start_type <- Called_go;
    be_the_scheduler t ?raise_unhandled_exn
  | Called_block_on_async ->
    (* This case can occur if the main thread uses Thread_safe.block_on_async before
       starting Async. Then, the scheduler is started and running in another thread,
       so we block forever instead of calling [be_the_scheduler] *)
    unlock t;
    (* We wakeup the scheduler so it can respond to whatever async changes this thread
       made. *)
    thread_safe_wakeup_scheduler t;
    (* Since the scheduler is already running, so we just pause forever. *)
    Time.pause_forever ()
  | Called_external_run _ ->
    raise_s [%message "cannot mix Scheduler.go and Scheduler.External"]
  | Called_go -> raise_s [%message "cannot Scheduler.go more than once"]
;;

let go_main
  ?raise_unhandled_exn
  ?file_descr_watcher
  ?max_num_open_file_descrs
  ?max_num_threads
  ~main
  ()
  =
  (match !the_one_and_only_ref with
   | Not_ready_to_initialize () | Ready_to_initialize _ -> ()
   | Initialized { initialized_at; _ } ->
     raise_s
       [%message
         "Async was initialized prior to [Scheduler.go_main]"
           (initialized_at : Backtrace.t)]);
  let max_num_open_file_descrs =
    Option.map max_num_open_file_descrs ~f:Max_num_open_file_descrs.create_exn
  in
  let max_num_threads = Option.map max_num_threads ~f:Max_num_threads.create_exn in
  let mutex = Nano_mutex.create () in
  Nano_mutex.lock_exn mutex;
  the_one_and_only_ref
    := Ready_to_initialize
         (fun () ->
           create ~mutex ?file_descr_watcher ?max_num_open_file_descrs ?max_num_threads ());
  Deferred.upon (return ()) main;
  go ?raise_unhandled_exn ()
;;

let is_the_one_and_only_running () =
  if is_ready_to_initialize () then false else is_running (the_one_and_only ())
;;

let report_long_cycle_times ?(cutoff = sec 1.) () =
  Stream.iter
    (long_cycles ~at_least:(cutoff |> Time_ns.Span.of_span_float_round_nearest))
    ~f:(fun span ->
      eprintf
        "%s\n%!"
        (Error.to_string_hum
           (Error.create "long async cycle" span [%sexp_of: Time_ns.Span.t])))
;;

let set_check_invariants bool = Kernel_scheduler.(set_check_invariants (t ()) bool)

let set_detect_invalid_access_from_thread bool =
  update_check_access (the_one_and_only ()) bool
;;

let set_max_inter_cycle_timeout span =
  (the_one_and_only ()).max_inter_cycle_timeout
    <- Max_inter_cycle_timeout.create_exn (Time_ns.Span.of_span_float_round_nearest span)
;;

type 'b folder = { folder : 'a. 'b -> t -> (t, 'a) Field.t -> 'b }

let t () = the_one_and_only ()

let with_t_once_started ~f =
  match !the_one_and_only_ref with
  | Initialized t when is_running t -> f t
  | _ -> Deferred.bind (return ()) ~f:(fun () -> f (t ()))
;;

let fold_fields (type a) ~init folder : a =
  let t = t () in
  let f ac field = folder.folder ac t field in
  Fields.fold
    ~init
    ~mutex:f
    ~start_type:f
    ~fds_whose_watching_has_changed:f
    ~file_descr_watcher:f
    ~busy_pollers:f
    ~num_busy_pollers:f
    ~time_spent_waiting_for_io:f
    ~fd_by_descr:f
    ~external_fd_by_descr:f
    ~external_fd_events:f
    ~timerfd:f
    ~timerfd_set_at:f
    ~scheduler_thread_id:f
    ~interruptor:f
    ~signal_manager:f
    ~thread_pool:f
    ~handle_thread_pool_stuck:f
    ~thread_pool_stuck:f
    ~dns_lookup_throttle:f
    ~next_tsc_calibration:f
    ~kernel_scheduler:f
    ~have_lock_do_cycle:f
    ~max_inter_cycle_timeout:f
    ~min_inter_cycle_timeout:f
    ~initialized_at:f
    ~uring:f
;;

let handle_thread_pool_stuck f =
  let t = t () in
  let kernel_scheduler = t.kernel_scheduler in
  let execution_context = Kernel_scheduler.current_execution_context kernel_scheduler in
  t.handle_thread_pool_stuck
    <- (fun _ ~stuck_for ->
         Kernel_scheduler.enqueue
           kernel_scheduler
           execution_context
           (fun () -> f ~stuck_for)
           ())
;;

module For_metrics = struct
  module Thread_pool_stats_subscription = struct
    type t = unit

    let created = ref false

    let create_exn () =
      if !created
      then failwith "Thread_pool_stats_subscription.create_exn can only be called once";
      created := true;
      ()
    ;;

    let get_and_reset () =
      let t = t () in
      Thread_pool.get_and_reset_stats t.thread_pool
    ;;
  end
end

module External = struct
  let current_thread_can_cycle () =
    if is_ready_to_initialize ()
    then true
    else (
      let t = the_one_and_only () in
      if not (am_holding_lock t)
      then
        raise_s
          [%message "Attempt to call current_thread_can_cycle without holding Async lock"];
      match t.start_type with
      | Not_started -> true
      | Called_external_run { active } when not !active -> i_am_the_scheduler t
      | Called_go | Called_block_on_async | Called_external_run _ ->
        if i_am_the_scheduler t
        then
          raise_s
            [%message
              "Scheduler.External.current_thread_can_cycle called from within Async"];
        false)
  ;;

  let collect_events event_list =
    List.map event_list ~f:(fun (ev : External_fd_event.t) ->
      match ev.event_type with
      | `Bad_fd -> raise_s [%message "Bad file descriptor" (ev.file_descr : File_descr.t)]
      | `Ready -> ev.file_descr, ev.read_or_write)
  ;;

  let run_one_cycle t =
    set_task_id ();
    let active =
      match t.start_type with
      | Called_external_run { active } -> active
      | Called_go | Called_block_on_async ->
        if t.scheduler_thread_id = current_thread_id ()
        then
          raise_s [%message "Scheduler.External.run_one_cycle called from within Async"]
        else
          raise_s
            [%message
              "Scheduler.External.run_one_cycle called while scheduler already running \
               in another thread"]
      | Not_started ->
        let active = ref false in
        t.start_type <- Called_external_run { active };
        init t;
        active
    in
    if !active
    then raise_s [%message "Scheduler.External.run_one_cycle called recursively"];
    if t.scheduler_thread_id <> current_thread_id ()
    then raise_s [%message "Scheduler.External.run_one_cycle called from wrong thread"];
    active := true;
    Exn.protect
      ~finally:(fun () ->
        active := false;
        t.external_fd_events <- [])
      ~f:(fun () ->
        Option.iter (one_iter t) ~f:Error.raise;
        collect_events t.external_fd_events)
  ;;

  let check_thread () =
    if not (current_thread_can_cycle ())
    then raise_s [%message "FD registration must only be done from the scheduler thread"]
  ;;

  let register_fd fd ops =
    check_thread ();
    let t = the_one_and_only () in
    match t.uring with
    | Some _ ->
      (* Unlike with the [epoll] or [select] fd watchers, the fd checking in uring happens
         asynchronously, so we can't implement a synchronous version of [unregister_fd]:
         we need to wait for the cancellation to be acknowledged by the kernel, which
         means waiting for asynchronous io_uring completions. *)
      raise_s
        [%message "Cannot watch external fds while using the Ocaml_uring fd watcher"]
    | None ->
      let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
      let%bind.Result () = By_descr.add t.external_fd_by_descr fd ops in
      (match F.set F.watcher fd ops with
       | exception exn ->
         By_descr.remove t.external_fd_by_descr fd;
         Error (Error.of_exn ~backtrace:`Get exn)
       | `Unsupported ->
         By_descr.remove t.external_fd_by_descr fd;
         Error (Error.of_string "Unsupported file descriptor type in register_fd")
       | `Ok -> Ok ())
  ;;

  let not_watching = Read_write_pair.create ~read:false ~write:false

  let unregister_fd fd =
    check_thread ();
    let t = the_one_and_only () in
    let module F = (val t.file_descr_watcher : File_descr_watcher.S) in
    if not (By_descr.mem t.external_fd_by_descr fd)
    then Error (Error.of_string "Attempt to unregister an FD which is not registered")
    else (
      By_descr.remove t.external_fd_by_descr fd;
      match F.set F.watcher fd not_watching with
      | exception exn -> Error (Error.of_exn ~backtrace:`Get exn)
      | `Unsupported ->
        (* Probably this can't happen because unsupported fd can't be registered
           in the first place *)
        Error (Error.of_string "Unsupported file descriptor type in unregister_fd")
      | `Ok -> Ok ())
  ;;

  let is_registered fd =
    check_thread ();
    let t = the_one_and_only () in
    By_descr.mem t.external_fd_by_descr fd
  ;;

  let run_one_cycle ~max_wait =
    let t = the_one_and_only () in
    if not (am_holding_lock t)
    then raise_s [%message "Attempt to run_one_cycle without holding Async lock"];
    match max_wait with
    | `Zero ->
      (* Ensure there is at least one ready-to-run job, so that Async doesn't block *)
      Kernel_scheduler.enqueue t.kernel_scheduler Execution_context.main ignore ();
      run_one_cycle t
    | `Until wake_at ->
      let wake = Clock_ns.Event.at wake_at in
      Exn.protect
        ~f:(fun () -> run_one_cycle t)
        ~finally:(fun () -> Clock_ns.Event.abort_if_possible wake ())
    | `Indefinite -> run_one_cycle t
  ;;

  let rec run_cycles_until_determined d =
    match Deferred.peek d with
    | Some x -> x
    | None ->
      (match run_one_cycle ~max_wait:`Indefinite with
       | [] -> run_cycles_until_determined d
       | ready_fds ->
         (* We're blocking until [d] is determined, so we ignore the fact that some fds
            are ready. Readiness is level-triggered so the events will reappear in the
            next [run_one_cycle] if ignored.

            However, if we just call [run_one_cycle] again these fds will still be ready,
            and we'll spin instead of blocking if we need to wait. So, to let Async wait
            without spinning uselessly, we temporarily unregister the ready fds and
            re-register afterwards.

            This is not tail recursive, but the stack depth is bounded by the number
            of externally registered FDs that are or become ready *)
         let t = the_one_and_only () in
         let fd_ops =
           List.map ~f:fst ready_fds
           (* FDs may be duplicated in ready_fds if they are simultaneously ready for
              reading and writing *)
           |> List.dedup_and_sort ~compare:[%compare: File_descr.t]
           |> List.map ~f:(fun fd -> fd, By_descr.find_exn t.external_fd_by_descr fd)
         in
         (* Try to ensure that we leave the set of registered fds unchanged,
            even if an exception is raised somewhere *)
         let rec temporarily_unregister = function
           | [] -> run_cycles_until_determined d
           | (fd, ops) :: fd_ops ->
             unregister_fd fd |> Or_error.ok_exn;
             Exn.protect
               ~finally:(fun () -> register_fd fd ops |> Or_error.ok_exn)
               ~f:(fun () -> temporarily_unregister fd_ops)
         in
         temporarily_unregister fd_ops)
  ;;
end
