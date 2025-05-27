open Core
open Import
module Fd = Raw_fd

let debug = Debug.interruptor

(* The [phase] state machine of an interruptor looks like this:

           [create]
               |
               v
      +--->  Awake <---------- [clear] ----------+
      |        |                                 |
      |        +-- [interrupt] ----------> Interrupted
    [clear]    |                                 ^
      |     [sleep]                              |
      |        |                                 |
      |        v                                 |
      +-  Sleeping --[interrupt+write]-----------+

   The key is that [interrupt] writes to the pipe only when transitioning from [Sleeping]
   to [Interrupted].

   When an [interrupt] happens while the sleeper is [Awake], no write to/read from the
   pipe is needed.
*)

type phase =
  | Sleeping
  | Awake
  | Interrupted
[@@deriving sexp_of]

type t =
  { read_fd : (Fd.t Capsule.Initial.Data.t[@sexp.opaque])
  ; write_fd : File_descr.t
  ; (* See the [phase] state machine description above. *)
    phase : phase Atomic.t
  ; clearbuffer : (Bytes.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let invariant _ = ()
let read_fd t = Capsule.Initial.Data.unwrap t.read_fd

let create ~create_fd =
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.set_close_on_exec pipe_read;
  Unix.set_close_on_exec pipe_write;
  let pipe_read =
    create_fd Fd.Kind.Fifo pipe_read (Info.of_string "interruptor_pipe_read")
  in
  let pipe_write = create_fd Fifo pipe_write (Info.of_string "interruptor_pipe_write") in
  { read_fd = Capsule.Initial.Data.wrap pipe_read
  ; write_fd = pipe_write.file_descr
  ; phase = Atomic.make Awake
  ; clearbuffer = Bytes.make 1 ' '
  }
;;

(* [thread_safe_interrupt]
   As the name implies, it is safe to call from any thread; [thread_safe_interrupt] does
   not assume the scheduler lock is held, although it is fine if it is. *)
let thread_safe_interrupt t =
  if debug then Debug.log_string "Interruptor.thread_safe_interrupt";
  let rec loop () =
    match Atomic.get t.phase with
    | Interrupted ->
      ( (* Nothing to do as both of these indicate that an interrupt was already made. *) )
    | Awake ->
      (match
         Atomic.compare_and_set t.phase ~if_phys_equal_to:Awake ~replace_with:Interrupted
       with
       | Set_here -> ()
       | Compare_failed ->
         (* There are two (main) possibilities:

            - either the watcher just went to sleep, or

            - someone else finished an interrupt.

            Neither of these cases is likely to be contended.  If the watcher went to
            sleep, we should just wake it up.  If someone else finished an interrupt, then
            we are done.  It is highly unlikely that we would need multiple retries and
            adding a backoff here is unlikely to improve performance. *)
         loop ())
    | Sleeping ->
      (match
         Atomic.compare_and_set
           t.phase
           ~if_phys_equal_to:Sleeping
           ~replace_with:Interrupted
       with
       | Compare_failed ->
         ( (* Nothing to do as failure here means that the watcher either woke up or was
              woken up. *) )
       | Set_here ->
         if debug then Debug.log_string "writing to interrupt_pipe_write";
         Syscall.syscall_exn (fun () ->
           let bytes_written = Caml_unix.write_substring t.write_fd " " 0 1 in
           (* The above blocking write should always succeed immediately as we do not
              accumulate bytes in the pipe. *)
           assert (bytes_written = 1)))
  in
  loop ()
;;

module Sleep = struct
  type t =
    | Clear_pending_interrupts
    | Sleep
end

let sleep t : Sleep.t =
  match Atomic.compare_and_set t.phase ~if_phys_equal_to:Awake ~replace_with:Sleeping with
  | Set_here -> Sleep
  | Compare_failed -> Clear_pending_interrupts
;;

let clear_fd t =
  Fd.syscall_exn (read_fd t) ~nonblocking:true (fun file_descr ->
    match
      let bytes_read = Caml_unix.read file_descr t.clearbuffer 0 1 in
      assert (bytes_read = 1)
    with
    | () -> ()
    | exception Unix.Unix_error (EAGAIN, _, _) ->
      (* This happens because Async schedules fd readiness callback jobs every cycle,
         with no guarantee that these jobs run the same cycle.

         So if the limit of 500 jobs per cycle is reached, these callbacks are left in
         the queue and duplicated next cycle. *)
      ())
;;

let clear t =
  if debug then Debug.log_string "Interruptor.clear";
  Atomic.set t.phase Awake
;;

let already_interrupted t =
  match Atomic.get t.phase with
  | Interrupted -> true
  | Sleeping | Awake -> false
;;
