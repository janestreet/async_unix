open Core.Std
open Import

module Priority = Linux_ext.Priority

let priority_zero = Priority.of_int 0

let getpriority =
  match Linux_ext.getpriority with
  | Error _ -> const priority_zero
  | Ok f -> f
;;

let setpriority =
  match Linux_ext.setpriority with
  | Error _ -> Fn.ignore
  | Ok f -> f
;;

let set_thread_name =
  match Linux_ext.pr_set_name_first16 with
  | Ok f -> f
  | Error _ -> Fn.ignore
;;

(* We define everything in an [Internal] module, and then wrap in a
   [Mutex.critical_section] each thread-safe function exposed in the mli. *)
module Internal = struct
  module Mutex = Nano_mutex

  let debug = Debug.thread_pool
  let check_invariant = ref false

  let error = Or_error.error

  module Pool_id : Unique_id = Unique_id.Int63 (Unit)

  module Work = struct
    type t =
      { (* When this work starts running, the name of the thread will be set (via
           [Linux_ext.pr_set_name]) to [name]. *)
        name : string;
        doit : unit -> unit;
        priority : Priority.t;
      }
    with sexp_of
  end

  module Work_queue = struct
    type elt =
    | Stop
    | Work of Work.t
    with sexp_of

    type t = elt Squeue.t with sexp_of

    let create () = Squeue.create 1

    let enqueue t work = Squeue.push_uncond t work
  end

  module Helper_thread = struct
    (* [Helper_thread.t] is a wrapper around [Thread.t] so the [threads] bag in the thread
       pool doesn't interfere with finalizers that clients attach to
       [Helper_thread.t]'s. *)
    type 'thread t =
      { in_pool : Pool_id.t;
        mutable state : [ `In_use | `Finishing | `Finished ];
        thread : 'thread;
        (* [default_name] will be used as the name of work run by the helper thread,
           unless that work is added with an overriding name. *)
        default_name : string;
        (* [default_priority] will be used as the priority of work run by the helper
           thread, unless that work is added with an overriding priority. *)
        default_priority : Priority.t;
      }
    with fields, sexp_of
  end

  module Thread = struct
    type t =
      { (* [name] is the name of the thread that the OS knows, i.e. the argument supplied
           to the most recent call to [set_thread_name] by the thread. *)
        mutable name : string;
        (* [thread_id] is the OCaml thread id of the OCaml thread that this corresponds
           to.  It is an option only because we create this object before creating the
           thread.  We set it to [Some] as soon as we create the thread, and then never
           change it. *)
        mutable thread_id : int option;
        (* [priority] is the priority of the thread that the OS knows, i.e. the argument
           supplied in the most recent call to [setpriority] by the thread. *)
        mutable priority : Priority.t;
        (* A thread can be "available", meaning that it isn't working on anything, or
           doing work added to the thread pool, or serving as a helper thread.  *)
        mutable state : [ `Available
                        | `Working
                        | `Helper of t sexp_opaque Helper_thread.t
                        ];
        (* [unfinished_work] is the amount of work remaining for this thread to do.  It
           includes all the work in [work_queue], plus perhaps an additional work that is
           running. *)
        mutable unfinished_work : int;
        (* [work_queue] is where this thread pulls work from.  Each thread has its own
           queue.  If a thread is working for the general pool, then its work queue has at
           most one element.  If a thread is a helper thread, then the work queue has all
           the unfinished work that has been added for the helper thread. *)
        work_queue : Work_queue.t;
      }
    with fields, sexp_of

    let invariant t : unit =
      try
        let check invariant field = invariant (Field.get field t) in
        Fields.iter
          ~name:ignore
          ~thread_id:(check (fun o -> assert (is_some o)))
          ~priority:ignore
          ~state:ignore
          ~unfinished_work:(check (fun unfinished_work ->
            assert (unfinished_work = Squeue.length t.work_queue
                   || unfinished_work = Squeue.length t.work_queue + 1)))
          ~work_queue:ignore
      with exn ->
        failwiths "Thread.invariant failed" (exn, t) <:sexp_of< exn * t >>
    ;;

    let is_available t =
      match t.state with
      | `Available -> true
      | `Working | `Helper _ -> false
    ;;

    let create priority =
      { name = "";
        thread_id = None;
        priority;
        state = `Available;
        unfinished_work = 0;
        work_queue = Work_queue.create ();
      }
    ;;

    let enqueue_work t work =
      t.unfinished_work <- t.unfinished_work + 1;
      Work_queue.enqueue t.work_queue (Work_queue.Work work);
    ;;

    let stop t = Work_queue.enqueue t.work_queue Work_queue.Stop

    let initialize_ocaml_thread t =
      set_thread_name t.name;
      (* We call [getpriority] to see whether we need to set the priority.  This is only
         used for initialization, not for ongoing work.  This is not a performance
         optimization.  It is done so that in programs that don't use priorities, we never
         call [setpriority], and thus prevent problems due to the user's "ulimit -e" being
         too restrictive. *)
      if not (Priority.equal (getpriority ()) t.priority) then
        setpriority t.priority;
    ;;

    let set_name t name =
      if String.(<>) name t.name then begin
        set_thread_name name;
        t.name <- name;
      end;
    ;;

    let set_priority t priority =
      if not (Priority.equal t.priority priority) then begin
        setpriority priority;
        t.priority <- priority;
      end;
    ;;

  end

  (* [Thread_pool.t] *)
  type t =
    { id : Pool_id.t;
      (** [state] starts as [`In_use] when the thread pool is created.  When the user
          calls [finished_with], it transitions to [`Finishing].  When the last work
          is done, it transitions to [`Finished]. *)
      mutable state : [ `In_use | `Finishing | `Finished ];
      (* [mutex] is used to protect all access to [t] and its substructures, since the
         threads actually doing the work need to access [t]. *)
      mutex : Mutex.t;
      (** [default_priority] is the priority that will be used for work unless that work
          is added with an overriding priority.  It is set to whatever the priority is
          when the thread pool is created. *)
      default_priority : Priority.t;
      (* [max_num_threads] is the maximum number of threads that the thread pool is
         allowed to create. *)
      max_num_threads : int;
      (* [num_threads] is the number of threads that have been created by the pool.  The
         thread pool guarantees that [num_threads <= max_num_threads]. *)
      mutable num_threads : int;
      (* [thread_creation_failure_lockout] is the amount of time that must pass after a
         thread-creation failure before the thread pool will make another attempt to
         create a thread. *)
      mutable thread_creation_failure_lockout : Time.Span.t;
      (* [last_thread_creation_failure] holds the last time that [Core.Std.Thread.create]
         raised.  It is used to avoid calling [Thread.create] too frequently when it is
         failing. *)
      mutable last_thread_creation_failure : Time.t;
      (* [threads] holds all the threads that have been created by the pool. *)
      mutable threads : Thread.t list;
      (* [available_threads] holds all threads that have [state = `Available].  It is used
         as a stack so that the most recently used available thread is used next, on the
         theory that this is better for locality. *)
      mutable available_threads : Thread.t list;
      (* [work_queue] holds work to be done for which no thread is available. *)
      work_queue : Work.t Queue.t;
      (* [unfinished_work] holds the amount of work that has been submitted to the pool
         but not yet been completed. *)
      mutable unfinished_work : int;
      mutable num_work_completed : int;
    }
  with fields, sexp_of

  let invariant t : unit =
    try
      let check invariant field = invariant (Field.get field t) in
      Fields.iter
        ~id:ignore
        ~state:(check (function
                  | `In_use | `Finishing -> ()
                  | `Finished ->
                    assert (t.unfinished_work = 0);
                    assert (t.num_threads = 0)))
        ~mutex:(check Mutex.invariant)
        ~default_priority:ignore
        ~max_num_threads:(check (fun max_num_threads ->
          assert (max_num_threads >= 1)))
        ~num_threads:(check (fun num_threads ->
          assert (num_threads = List.length t.threads);
          assert (num_threads <= t.max_num_threads)))
        ~thread_creation_failure_lockout:ignore
        ~last_thread_creation_failure:ignore
        ~threads:(check (fun threads -> List.iter threads ~f:Thread.invariant))
        ~available_threads:(check (fun available_threads ->
          assert (List.length available_threads <= t.num_threads);
          List.iter available_threads ~f:(fun thread ->
            assert (List.exists t.threads ~f:(fun thread' -> phys_equal thread thread'));
            assert (Thread.is_available thread))))
        ~work_queue:(check (fun work_queue ->
          (* It is possible that:

             {[
               has_unstarted_work t
               && t.num_threads < t.max_num_threads
             ]}

             This happens when adding work and [Core.Std.Thread.create] raises.  In that
             case, the thread pool enqueues the work and continues with the threads it
             has.  If the thread pool can't make progress, then Async's thread-pool-stuck
             detection will later report it. *)
          assert (Queue.is_empty work_queue
                  ||  List.is_empty t.available_threads)))
        ~unfinished_work:(check (fun unfinished_work -> assert (unfinished_work >= 0)))
        ~num_work_completed:(check (fun num_work_completed ->
          assert (num_work_completed >= 0)))
    with exn ->
      failwiths "Thread_pool.invariant failed" (exn, t) <:sexp_of< exn * t >>
  ;;

  let is_in_use t =
    match t.state with
    | `In_use -> true
    | `Finishing | `Finished -> false
  ;;

  let has_unstarted_work t = not (Queue.is_empty t.work_queue)

  let create ~max_num_threads =
    if max_num_threads < 1 then
      error "Thread_pool.create max_num_threads was < 1" max_num_threads
        (<:sexp_of< int >>)
    else
      let t =
        { id                                      = Pool_id.create ()
        ; state                                   = `In_use
        ; mutex                                   = Mutex.create ()
        ; default_priority                        = getpriority ()
        ; max_num_threads
        ; num_threads                             = 0
        ; threads                                 = []
        ; thread_creation_failure_lockout         = sec 1.
        ; last_thread_creation_failure            = Time.epoch
        ; available_threads                       = []
        ; work_queue                              = Queue.create ()
        ; unfinished_work                         = 0
        ; num_work_completed                      = 0
        }
      in
      Ok t
  ;;

  let maybe_finish t =
    match t.state with
    | `In_use | `Finished -> ()
    | `Finishing ->
      if t.unfinished_work = 0 then begin
        let set x f = Option.value_exn (Field.setter f) t x in
        Fields.iter
          ~id:ignore
          ~state:(set `Finished)
          ~mutex:ignore
          ~default_priority:ignore
          ~max_num_threads:ignore
          ~num_threads:(set 0)
          ~thread_creation_failure_lockout:ignore
          ~last_thread_creation_failure:ignore
          ~threads:(fun _ ->
            List.iter t.threads ~f:Thread.stop;
            t.threads <- [])
          ~available_threads:(set [])
          ~work_queue:ignore
          ~unfinished_work:ignore
          ~num_work_completed:ignore;
      end;
  ;;

  let finished_with t =
    if debug then Debug.log "Thread_pool.finished_with" t <:sexp_of< t >>;
    match t.state with
    | `Finishing | `Finished -> ()
    | `In_use ->
      t.state <- `Finishing;
      maybe_finish t;
  ;;

  let assign_work_to_thread thread work =
    thread.Thread.state <- `Working;
    Thread.enqueue_work thread work;
  ;;

  let make_thread_available t thread =
    if debug then
      Debug.log "make_thread_available" (thread, t) (<:sexp_of< Thread.t * t >>);
    match Queue.dequeue t.work_queue with
    | Some work -> assign_work_to_thread thread work
    | None ->
      thread.Thread.state <- `Available;
      t.available_threads <- thread :: t.available_threads;
      maybe_finish t;
  ;;

  let maybe_finish_helper_thread t helper_thread =
    let module H = Helper_thread in
    match helper_thread.H.state with
    | `In_use | `Finished -> ()
    | `Finishing ->
      let thread = helper_thread.H.thread in
      if thread.Thread.unfinished_work = 0 then begin
        helper_thread.H.state <- `Finished;
        make_thread_available t thread;
      end;
  ;;

  let create_thread t =
    if debug then Debug.log "create_thread" t <:sexp_of< t >>;
    let thread = Thread.create t.default_priority in
    let ocaml_thread =
      Or_error.try_with (fun () ->
        Core.Std.Thread.create (fun () ->
          Thread.initialize_ocaml_thread thread;
          let rec loop () =
            match Squeue.pop thread.Thread.work_queue with
            | Work_queue.Stop -> ()
            | Work_queue.Work work ->
              if debug then
                Debug.log "thread got work" (work, thread, t)
                  (<:sexp_of< Work.t * Thread.t * t >>);
              Thread.set_name thread work.Work.name;
              Thread.set_priority thread work.Work.priority;
              (try
                 work.Work.doit () (* the actual work *)
               with _ -> ());
              t.num_work_completed <- t.num_work_completed + 1;
              if debug then Debug.log "thread finished with work" (work, thread, t)
                              (<:sexp_of< Work.t * Thread.t * t >>);
              Mutex.critical_section t.mutex ~f:(fun () ->
                t.unfinished_work <- t.unfinished_work - 1;
                thread.Thread.unfinished_work <- thread.Thread.unfinished_work - 1;
                begin match thread.Thread.state with
                | `Available -> assert false
                | `Helper helper_thread -> maybe_finish_helper_thread t helper_thread
                | `Working -> make_thread_available t thread;
                end);
              loop ()
          in
          loop ()) ())
    in
    Or_error.map ocaml_thread ~f:(fun ocaml_thread ->
      thread.Thread.thread_id <- Some (Core.Std.Thread.id ocaml_thread);
      t.num_threads <- t.num_threads + 1;
      t.threads <- thread :: t.threads;
      thread)
  ;;

  let get_available_thread t =
    if debug then Debug.log "get_available_thread" t (<:sexp_of< t >>);
    match t.available_threads with
    | thread :: rest -> t.available_threads <- rest; `Ok thread
    | [] ->
      if t.num_threads = t.max_num_threads
      then `None_available
      else
        let now = Time.now () in
        if Time.Span.( < ) (Time.diff now t.last_thread_creation_failure)
             t.thread_creation_failure_lockout
        then `None_available
        else
          match create_thread t with
          | Ok thread -> `Ok thread
          | Error _ ->
            t.last_thread_creation_failure <- now;
            `None_available
  ;;

  let inc_unfinished_work t = t.unfinished_work <- t.unfinished_work + 1

  let default_thread_name = "thread-pool thread"

  let add_work ?priority ?name t doit =
    if debug then Debug.log "add_work" t <:sexp_of< t >>;
    if not (is_in_use t) then
      error "add_work called on finished thread pool" t <:sexp_of< t >>
    else begin
      let work =
        { Work.
          doit;
          name = Option.value name ~default:default_thread_name;
          priority = Option.value priority ~default:t.default_priority;
        }
      in
      inc_unfinished_work t;
      begin match get_available_thread t with
      | `None_available -> Queue.enqueue t.work_queue work
      | `Ok thread -> assign_work_to_thread thread work
      end;
      Ok ()
    end
  ;;

  let create_helper_thread ?priority ?name t =
    if debug then
      Debug.log "create_helper_thread" t <:sexp_of< t >>;
    if not (is_in_use t) then
      error "create_helper_thread called on finished thread pool" t <:sexp_of< t >>
    else
      match get_available_thread t with
      | `None_available ->
        error "create_helper_thread could not get a thread" t (<:sexp_of< t >>);
      | `Ok thread ->
        let helper_thread =
          { Helper_thread.
            default_name = Option.value name ~default:"helper_thread";
            default_priority = Option.value priority ~default:t.default_priority;
            in_pool = t.id;
            state = `In_use;
            thread;
          }
        in
        thread.Thread.state <- `Helper helper_thread;
        Ok helper_thread;
  ;;

  let add_work_for_helper_thread ?priority ?name t helper_thread doit =
    if debug then
      Debug.log "add_work_for_helper_thread" (helper_thread, t)
        (<:sexp_of< Thread.t Helper_thread.t * t >>);
    let module H = Helper_thread in
    if not (Pool_id.equal t.id helper_thread.H.in_pool) then
      error "add_work_for_helper_thread called on helper thread not in pool"
        (helper_thread, t) (<:sexp_of< Thread.t Helper_thread.t * t >>)
    else if not (is_in_use t) then
      error "add_work_for_helper_thread called on finished thread pool" t <:sexp_of< t >>
    else begin
      match helper_thread.H.state with
      | `Finishing | `Finished ->
        error "add_work_for_helper_thread called on helper thread no longer in use"
          (helper_thread, t) <:sexp_of< Thread.t Helper_thread.t * t >>
      | `In_use ->
        let { Helper_thread. thread; _ } = helper_thread in
        inc_unfinished_work t;
        Thread.enqueue_work thread
          { Work.
            name = Option.value name ~default:(Helper_thread.default_name helper_thread);
            doit;
            priority =
              Option.value priority
                ~default:(Helper_thread.default_priority helper_thread);
          };
        Ok ();
    end
  ;;

  let finished_with_helper_thread t helper_thread =
    if debug then
      Debug.log "finished_with_helper_thread" (helper_thread, t)
        (<:sexp_of< Thread.t Helper_thread.t * t >>);
    let module H = Helper_thread in
    if not (Pool_id.equal t.id helper_thread.H.in_pool) then
      failwiths "finished_with_helper_thread called on helper thread not in pool"
        (helper_thread, t) (<:sexp_of< Thread.t Helper_thread.t * t >>)
    else
      match helper_thread.H.state with
      | `Finishing | `Finished -> ()
      | `In_use ->
        helper_thread.H.state <- `Finishing;
        maybe_finish_helper_thread t helper_thread;
  ;;
end

(* Now we define everything to be exported, being careful to wrap everything in
   [Mutex.critical_section] that needs to be. *)
open Internal

type t = Internal.t with sexp_of

let critical_section t ~f =
  Mutex.critical_section t.mutex ~f:(fun () ->
    protect ~f ~finally:(fun () -> if !check_invariant then invariant t));
;;

let invariant t = critical_section t ~f:(fun () -> invariant t)

let create ~max_num_threads =
  Result.map (create ~max_num_threads) ~f:(fun t ->
    if !check_invariant then invariant t;
    t)
;;

let finished_with t = critical_section t ~f:(fun () -> finished_with t)

let default_priority   = default_priority
let has_unstarted_work = has_unstarted_work
let max_num_threads    = max_num_threads
let num_threads        = num_threads
let num_work_completed = num_work_completed

module Helper_thread = struct
  open Helper_thread

  type t = Thread.t Helper_thread.t with sexp_of

  let default_name = default_name
  let default_priority = default_priority
end

let add_work ?priority ?name t doit =
  critical_section t ~f:(fun () -> add_work ?priority ?name t doit);
;;

let create_helper_thread ?priority ?name t =
  critical_section t ~f:(fun () -> create_helper_thread ?priority ?name t)
;;

let add_work_for_helper_thread ?priority ?name t helper_thread doit =
  critical_section t ~f:(fun () ->
    add_work_for_helper_thread ?priority ?name t helper_thread doit);
;;

let finished_with_helper_thread t helper_thread =
  critical_section t ~f:(fun () ->
    finished_with_helper_thread t helper_thread);
;;

TEST_MODULE = struct

  let () = check_invariant := true

  let wait_until_no_unfinished_work t =
    let rec loop i =
      if t.unfinished_work > 0 then begin
        Time.pause (sec 0.01);
        loop (i + 1);
      end;
    in
    loop 0
  ;;

  (* A simple thread-safe [Ivar] implementation.*)
  module Ivar : sig
    type 'a t with sexp_of

    val create : unit -> _ t
    val fill : 'a t -> 'a -> unit
    val read : 'a t -> 'a (* blocks until value is available *)
  end = struct
    module Mutex = Core.Std.Mutex
    module Condition = Core.Std.Condition

    type 'a t =
      { mutable value : 'a option;
        mutable num_waiting : int;
        mutex : Mutex.t sexp_opaque;
        (* Threads that do [Ivar.read] when [is_none value] block using [Condition.wait
           full].  When [Ivar.fill] sets [value], it uses [Condition.broadcast] to wake
           up all the blocked threads. *)
        full : Condition.t sexp_opaque;
      }
    with sexp_of

    let create () =
      { value = None;
        num_waiting = 0;
        mutex = Mutex.create ();
        full = Condition.create ();
      }
    ;;

    let critical_section t ~f = Mutex.critical_section t.mutex ~f

    let fill t v =
      critical_section t ~f:(fun () ->
        if is_some t.value
        then failwith "Ivar.fill of full ivar"
        else begin
          t.value <- Some v;
          Condition.broadcast t.full;
        end);
    ;;

    let read t =
      match t.value with
      | Some v -> v
      | None ->
        critical_section t ~f:(fun () ->
          match t.value with
          | Some v -> v
          | None ->
            t.num_waiting <- t.num_waiting + 1;
            Condition.wait t.full t.mutex;
            t.num_waiting <- t.num_waiting - 1;
            match t.value with
            | Some v -> v
            | None -> assert false)
    ;;
  end

  (* [create] and [finished_with]. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    assert (max_num_threads t = 1);
    assert (num_threads t = 0); (* no threads should have been created *)
    finished_with t;
  ;;

  (* Error cases for [create]. *)
  TEST =
    List.for_all [ -1; 0 ] ~f:(fun max_num_threads ->
      Result.is_error (create ~max_num_threads))
  ;;

  (* Error cases for [add_work]. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    finished_with t;
    assert (Result.is_error (add_work t ignore));
  ;;

  (* Work finishing after [finished_with] is called causes the thread pool to finish. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let finish_work = Ivar.create () in
    ok_exn (add_work t (fun () -> Ivar.read finish_work));
    finished_with t;
    Ivar.fill finish_work ();
    wait_until_no_unfinished_work t;
    assert (t.state = `Finished);
  ;;

  (* Check that the expected concurrency is used. *)
  TEST_UNIT =
    List.iter [ 1; 2; 5; 10; 100; 1000 ] ~f:(fun num_jobs ->
      List.iter [ 1; 2; 5; 10; 100 ] ~f:(fun max_num_threads ->
        if debug then
          eprintf "num_jobs = %d  max_num_threads = %d\n%!"
            num_jobs max_num_threads;
        let expected_max_concurrent_jobs = min num_jobs max_num_threads in
        let max_observed_concurrent_jobs = ref 0 in
        let num_concurrent_jobs = ref 0 in
        let job_starts = ref [] in
        let t = ok_exn (create ~max_num_threads) in
        let worker_threads_have_fully_started = Ivar.create () in
        let worker_threads_should_continue = Ivar.create () in
        let _ : Core.Std.Thread.t =
          Core.Std.Thread.create (fun () ->
            let start = Time.now () in
            let rec loop () =
              if is_in_use t then begin
                let how_long = Time.diff (Time.now ()) start in
                if Time.Span.(>=) how_long (sec 10.) then begin
                  Debug.log "thread-pool unit test hung"
                    (t,
                     worker_threads_have_fully_started,
                     worker_threads_should_continue)
                    (<:sexp_of< t * unit Ivar.t * unit Ivar.t >>);
                  exit 1;
                end else begin
                  Time.pause (sec 0.1);
                  loop ();
                end
              end;
            in
            loop ()
          ) ()
        in
        let jobs = ref [] in
        for i = 0 to num_jobs - 1; do
          let job =
            ok_exn (add_work t (fun () ->
              job_starts := i :: !job_starts;
              if List.length !job_starts = expected_max_concurrent_jobs then
                Ivar.fill worker_threads_have_fully_started ();
              incr num_concurrent_jobs;
              max_observed_concurrent_jobs :=
                max !max_observed_concurrent_jobs !num_concurrent_jobs;
              assert (!num_concurrent_jobs <= max_num_threads);
              Ivar.read worker_threads_should_continue;
              decr num_concurrent_jobs))
          in
          jobs := job :: !jobs
        done;
        Ivar.read worker_threads_have_fully_started;
        assert (!num_concurrent_jobs = expected_max_concurrent_jobs);
        assert (List.length !job_starts = expected_max_concurrent_jobs);
        if max_num_threads = 1 then
          assert (!job_starts = List.init expected_max_concurrent_jobs ~f:Fn.id);
        Ivar.fill worker_threads_should_continue ();
        wait_until_no_unfinished_work t;
        assert (!max_observed_concurrent_jobs = expected_max_concurrent_jobs);
        if max_num_threads = 1 then
          assert (List.rev !job_starts = List.init num_jobs ~f:Fn.id);
        assert (t.num_threads <= max_num_threads);
        finished_with t;
      ))
  ;;

  (* Helper threads. *)

  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let helper_thread = ok_exn (create_helper_thread t) in
    let helper_continue = Ivar.create () in
    let helper_finished = Ivar.create () in
    let work_finished = Ivar.create () in
    ok_exn (add_work_for_helper_thread t helper_thread
              (fun () ->
                Ivar.read helper_continue;
                Ivar.fill helper_finished ();
              ));
    ok_exn (add_work t (fun () -> Ivar.fill work_finished ()));
    Ivar.fill helper_continue ();
    Ivar.read helper_finished;
    finished_with_helper_thread t helper_thread;
    Ivar.read work_finished;
    wait_until_no_unfinished_work t;
    finished_with t;
  ;;

  (* Calling [finished_with_helper_thread] while work remains is allowed, and causes
     the thread to be returned to the general pool once it finishes all its work. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let helper_thread = ok_exn (create_helper_thread t) in
    let general_work_got_done = ref false in
    ok_exn (add_work t (fun () -> general_work_got_done := true));
    let helper_continue = Ivar.create () in
    let helper_finished = Ivar.create () in
    ok_exn (add_work_for_helper_thread t helper_thread
              (fun () ->
                Ivar.read helper_continue;
                Ivar.fill helper_finished ()));
    finished_with_helper_thread t helper_thread;
    assert (Result.is_error (add_work_for_helper_thread t helper_thread Fn.ignore));
    assert (not !general_work_got_done);
    Ivar.fill helper_continue ();
    Ivar.read helper_finished;
    wait_until_no_unfinished_work t;
    assert !general_work_got_done;
    finished_with t;
  ;;

  (* Error cases for mismatches between pool and helper thread. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let t_bad = ok_exn (create ~max_num_threads:1) in
    let helper_thread = ok_exn (create_helper_thread t) in
    assert (Result.is_error (add_work_for_helper_thread  t_bad helper_thread ignore));
    assert (Result.is_error (Result.try_with (fun () ->
                               finished_with_helper_thread t_bad helper_thread)));
    finished_with_helper_thread t helper_thread;
    finished_with t;
  ;;

  (* Setting thread name and priority. *)
  TEST_UNIT =
    Result.iter Core.Std.Unix.RLimit.nice ~f:(fun rlimit_nice ->
      let test_parameters =
        let open Core.Std.Unix.RLimit in
        let nice_limit = get rlimit_nice in
        match nice_limit.max with
        | Infinity ->
          let max = 40 in
          `Test ({ nice_limit with cur = Limit (Int64.of_int_exn max) }, max)
        | Limit max ->
          if Int64.( < ) max (Int64.of_int 2)
          then `Cannot_test
          else `Test ({ nice_limit with cur = Limit max }, (Int64.to_int_exn max))
      in
      match test_parameters with
      | `Cannot_test -> ()
      | `Test (nice_limit, cur_limit) ->
        Core.Std.Unix.RLimit.set rlimit_nice nice_limit;
        for priority = 20 - cur_limit to 20 do
          let initial_priority = Priority.of_int priority in
          match Linux_ext.getpriority, Linux_ext.pr_get_name with
          | Error _, _ | _, Error _ -> ()
          | Ok getpriority, Ok get_name ->
            let t = ok_exn (create ~max_num_threads:2) in
            let check4 ~name ~priority
                (check : ?name:string -> ?priority:Priority.t -> unit -> unit Or_error.t)
                =
              ok_exn (check ());
              ok_exn (check ~name ());
              ok_exn (check ~priority ());
              ok_exn (check ~name ~priority ());
              wait_until_no_unfinished_work t;
            in
            check4 ~name:"new name" ~priority:(Priority.decr initial_priority)
              (fun ?name ?priority () ->
                add_work ?priority ?name t (fun () ->
                  assert (get_name () = Option.value name ~default:default_thread_name);
                  assert (getpriority ()
                          = Option.value priority ~default:(default_priority t))));
            check4 ~name:"new name" ~priority:(Priority.decr initial_priority)
              (fun ?name ?priority () ->
                let helper_thread = ok_exn (create_helper_thread t ?priority ?name) in
                let default_thread_name =
                  Option.value name ~default:default_thread_name
                in
                let default_priority =
                  Option.value priority ~default:(default_priority t)
                in
                check4 ~name:"new name 2" ~priority:(Priority.decr initial_priority)
                  (fun ?name ?priority () ->
                    add_work_for_helper_thread ?priority ?name t helper_thread (fun () ->
                      assert (get_name ()
                              = Option.value name ~default:default_thread_name);
                      assert (getpriority ()
                              = Option.value priority ~default:default_priority)));
                finished_with_helper_thread t helper_thread;
                Ok ());
            finished_with t;
        done)
  ;;

  (* [Core.Std.Thread.create] failure *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:2) in
    (* simulate failure *)
    t.last_thread_creation_failure <- Time.now ();
    t.thread_creation_failure_lockout <- sec 100.;
    let finish_work = Ivar.create () in
    ok_exn (add_work t (fun () -> Ivar.read finish_work));
    assert (has_unstarted_work t);
    t.thread_creation_failure_lockout <- sec 0.;
    ok_exn (add_work t (fun () -> Ivar.read finish_work));
    Ivar.fill finish_work ();
    wait_until_no_unfinished_work t;
  ;;
end
