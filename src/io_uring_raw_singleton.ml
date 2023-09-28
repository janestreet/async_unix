open! Core
open Import
module Eventfd = Linux_ext.Eventfd

type t =
  | Not_supported of unit
  | Ok of Io_uring_raw.t

module Eventfd_driver = struct
  (**
     The submission and completion of tasks is completely autonomous, using two async jobs
     (one for submission and one for completion) that get scheduled when needed.
     Submission is done at the end of every cycle (submissions when the queue is empty
     should be very cheap). If a syscall makes its way to the completion queue, the job
     that will fill the corresponding deferred is scheduled the next time the async
     scheduler checks for I/O through the file descriptor watcher.
  *)
  let register_hooks uring eventfd =
    Io_uring_raw.register_eventfd uring (Eventfd.to_file_descr eventfd);
    Async_kernel_scheduler.Expert.run_every_cycle_end (fun () ->
      let (_ : int) = Io_uring_raw.submit uring in
      ());
    let fd =
      Raw_scheduler.create_fd
        Raw_fd.Kind.Fifo
        (Eventfd.to_file_descr eventfd)
        (Info.create_s [%sexp "io_uring_raw eventfd"])
    in
    (* Although this job is only ever scheduled when the eventfd is ready to read, we
       still have to run it in nonblocking mode and handle operations that would block.
       This is needed because this job could end up being scheduled more than once at a
       time and be run multiple times within the same async cycle.
       (max_num_jobs_per_priority_per_cycle can cause this job to be run in a future cycle)
    *)
    let eventfd_ready_job =
      Raw_scheduler.create_job
        (Raw_scheduler.the_one_and_only ())
        (fun () ->
          try
            let (_ : Int64.t) = Eventfd.read eventfd in
            let (_ : int) = Io_uring_raw.fill_completions uring in
            ()
          with
          | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
          | exn -> raise exn)
        ()
    in
    let finished_watching = Ivar.create () in
    (match
       Raw_scheduler.request_start_watching
         (Raw_scheduler.the_one_and_only ())
         fd
         `Read
         (Raw_fd.Watching.Watch_repeatedly (eventfd_ready_job, finished_watching))
     with
     | `Watching -> ()
     | (`Already_closed | `Already_watching) as result ->
       raise_s
         [%sexp
           (("unexpected result when asked to watch eventfd", result)
             : string * [ `Already_closed | `Already_watching ])]);
    Deferred.upon (Ivar.read finished_watching) (fun reason ->
      raise_s
        [%sexp
          (("unexpectedly stopped watching eventfd", reason)
            : string * [ `Bad_fd | `Closed | `Interrupted | `Unsupported ])])
  ;;

  let force_uring_exn () =
    let uring =
      Io_uring_raw.create
        ~queue_depth:
          (Io_uring_max_submission_entries.raw Config.io_uring_max_submission_entries)
        ()
    in
    match Eventfd.create, uring with
    | Error eventfd_error, Error uring_error ->
      Error.raise (Error.of_list [ eventfd_error; uring_error ])
    | Error eventfd_error, Ok uring ->
      Io_uring_raw.exit uring;
      Error.raise eventfd_error
    | Ok _, Error uring_error -> Error.raise uring_error
    | Ok create_eventfd, Ok uring ->
      let eventfd =
        create_eventfd ~flags:Eventfd.Flags.(cloexec + nonblock) (Int32.of_int_exn 0)
      in
      register_hooks uring eventfd;
      Ok uring
  ;;

  let force_uring_noraise () =
    try force_uring_exn () with
    | _exn -> Not_supported ()
  ;;
end

module From_scheduler_driver = struct
  let force_uring () =
    match Raw_scheduler.uring (Raw_scheduler.t ()) with
    | None -> Not_supported ()
    | Some uring -> Ok uring
  ;;
end

let create_global_io_uring () =
  match Config.io_uring_mode with
  | Disabled -> Not_supported ()
  | Eventfd -> Eventfd_driver.force_uring_exn ()
  | If_available_eventfd -> Eventfd_driver.force_uring_noraise ()
  | From_scheduler -> From_scheduler_driver.force_uring ()
;;

let global_io_uring = lazy (create_global_io_uring ())

let the_one_and_only () =
  match force global_io_uring with
  | Not_supported () -> None
  | Ok io_uring -> Some io_uring
;;
