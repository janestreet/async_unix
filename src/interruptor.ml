open Core
open Import
module Fd = Raw_fd

let debug = Debug.interruptor

type t =
  { read_fd : ((Fd.t, Capsule.Expert.initial) Capsule.Data.t[@sexp.opaque])
  ; write_fd : File_descr.t
  ; (* [already_interrupted] keeps track of whether we've already interrupted since the
       most recent call to [clear], and if so, avoid writing to the pipe again.
       [already_interrupted] does not exactly track the state of [pipe].  It is possible
       for [already_interrupted] to be false and for the [pipe] to be nonempty.  The key
       property is that if [already_interrupted] is true then [pipe] is nonempty*)
    already_interrupted : bool Atomic.t
  ; clearbuffer : (Bytes.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let invariant _ = ()
let read_fd t = Capsule.Expert.Data.unwrap ~access:Capsule.Expert.initial t.read_fd

let create ~create_fd =
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.set_close_on_exec pipe_read;
  Unix.set_close_on_exec pipe_write;
  let pipe_read =
    create_fd Fd.Kind.Fifo pipe_read (Info.of_string "interruptor_pipe_read")
  in
  let pipe_write = create_fd Fifo pipe_write (Info.of_string "interruptor_pipe_write") in
  Raw_fd.set_nonblock_if_necessary ~nonblocking:true pipe_read;
  { read_fd = Capsule.Expert.Data.wrap ~access:Capsule.Expert.initial pipe_read
  ; write_fd = pipe_write.file_descr
  ; already_interrupted = Atomic.make false
  ; clearbuffer = Bytes.make 1024 ' '
  }
;;

(* [bytes_w] is a toplevel to make sure it's not allocated multiple times. *)
let bytes_w = Capsule.Isolated.create (fun () -> Bytes.of_string "w")

(* [thread_safe_interrupt]
   As the name implies, it is safe to call from any thread; [thread_safe_interrupt] does
   not assume the scheduler lock is held, although it is fine if it is. *)
let thread_safe_interrupt t =
  if debug then Debug.log_string "Interruptor.thread_safe_interrupt";
  match
    Atomic.compare_and_set
      t.already_interrupted
      ~if_phys_equal_to:false
      ~replace_with:true
  with
  | Compare_failed -> ()
  | Set_here ->
    if debug then Debug.log_string "writing to interrupt_pipe_write";
    Syscall.syscall_exn (fun () ->
      try
        ignore
          (Capsule.Isolated.with_shared bytes_w ~f:(fun bytes_w ->
             Unix.write_assume_fd_is_nonblocking t.write_fd bytes_w)
           : int)
      with
      | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) -> ())
;;

let clear t =
  if debug then Debug.log_string "Interruptor.clear";
  (* We only need to clear the pipe if it was written to.  This saves a system call in the
     common case. *)
  if Atomic.get t.already_interrupted
  then
    Fd.syscall_exn (read_fd t) ~nonblocking:true (fun file_descr ->
      let rec loop () =
        let read_again =
          try
            let bytes_read =
              Unix.read_assume_fd_is_nonblocking
                file_descr
                t.clearbuffer
                ~pos:0
                ~len:(Bytes.length t.clearbuffer)
            in
            ignore (bytes_read : int);
            true
          with
          | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) -> false
        in
        if read_again then loop ()
      in
      loop ());
  (* We must clear [already_interrupted] after emptying the pipe.  If we did it before,
     a [thread_safe_interrupt] could come along in between.  We would then be left with
     [already_interrupted = true] and an empty pipe, which would then cause a
     [thread_safe_interrupt] after [clear] returns to incorrectly be a no-op. *)
  Atomic.set t.already_interrupted false
;;

let already_interrupted t = Atomic.get t.already_interrupted
