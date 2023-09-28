open Core
open Import
open File_descr_watcher_intf
open Read_write_pair.Export
module Table = Bounded_int_table

module Flags = struct
  include Io_uring_raw.Poll_mask

  let of_rw = function
    | `Read -> pollin
    | `Write -> pollout
  ;;
end

module Fd_state = struct
  type t =
    { running_job : Io_uring_raw.Handle.t
    ; flags : Io_uring_raw.Poll_mask.t
    }
end

type t =
  { uring : (Io_uring_raw.t[@sexp.opaque])
  ; states : ((File_descr.t, Fd_state.t) Table.t[@sexp.opaque])
  ; handle_fd_read_ready : File_descr.t -> Flags.t -> unit
  ; handle_fd_write_ready : File_descr.t -> Flags.t -> unit
  }
[@@deriving sexp_of, fields ~iterators:iter]

let backend = Config.File_descr_watcher.Io_uring

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~uring:ignore
      ~states:
        (check (fun states ->
           Table.iter states ~f:(fun { Fd_state.running_job = _; flags } ->
             assert (
               List.exists
                 Flags.[ pollin; pollout; pollin + pollout ]
                 ~f:(fun flags' -> Flags.(mem flags' flags && mem flags flags'))))))
      ~handle_fd_read_ready:ignore
      ~handle_fd_write_ready:ignore
  with
  | exn ->
    raise_s
      [%message
        "Io_uring_file_descr_watcher.invariant failed"
          (exn : exn)
          ~io_uring_file_descr_watcher:(t : t)]
;;

type 'a additional_create_args = uring:Io_uring_raw.t -> 'a

let create ~uring ~num_file_descrs ~handle_fd_read_ready ~handle_fd_write_ready =
  if not (Io_uring_raw.supports_ext_arg uring)
  then
    raise_s
      [%sexp
        "Cannot create an Ocaml_uring file descriptor watcher if IORING_FEAT_EXT_ARG is \
         not supported because then it is not thread safe."];
  let states =
    Table.create
      ~num_keys:num_file_descrs
      ~key_to_int:File_descr.to_int
      ~sexp_of_key:File_descr.sexp_of_t
      ()
  in
  let handle_fd read_or_write handle_fd =
    let bit = Flags.of_rw read_or_write in
    fun file_descr flags ->
      (* [io_uring], similar to [epoll], has an implicit event
         flags for hangup (HUP) and error (ERR), whereas select will just return that fd
         as "ready" in its appropriate fd_set.  Since we don't know if it's ready for IN
         or OUT, we have to go lookup the entry if the HUP or ERR flag is set. *)
      if Flags.mem bit flags
         || ((Flags.mem Flags.pollerr flags || Flags.mem Flags.pollhup flags)
             &&
             match Table.find states file_descr with
             | None -> false
             | Some { Fd_state.running_job = _; flags } -> Flags.mem bit flags)
      then handle_fd file_descr
  in
  { uring
  ; states
  ; handle_fd_read_ready = handle_fd `Read handle_fd_read_ready
  ; handle_fd_write_ready = handle_fd `Write handle_fd_write_ready
  }
;;

let reset_in_forked_process _ = ()

let iter t ~f =
  Table.iteri t.states ~f:(fun ~key:file_descr ~data:{ running_job = _; flags } ->
    if Flags.mem Flags.pollin flags then f file_descr `Read;
    if Flags.mem Flags.pollout flags then f file_descr `Write)
;;

let rec add_poll t file_descr flags =
  let handle = Io_uring_raw.poll_add t.uring file_descr flags in
  Table.set t.states ~key:file_descr ~data:{ running_job = handle; flags };
  Deferred.don't_wait_for
    (let%bind res = Io_uring_raw.syscall_result handle in
     match Table.find t.states file_descr with
     | None -> return ()
     | Some { running_job; flags } ->
       if phys_equal running_job handle
       then (
         match res with
         (* This is ECANCELED *)
         | Error (Unix.EUNKNOWNERR 125) -> ()
         | Error err -> failwith (Unix.Error.message err)
         | Ok res ->
           handle_fd_read_ready t file_descr (Flags.of_int res);
           handle_fd_write_ready t file_descr (Flags.of_int res);
           add_poll t file_descr flags);
       return ())
;;

let remove_poll_exn t file_descr =
  match Table.find t.states file_descr with
  | Some { running_job; _ } ->
    Table.remove t.states file_descr;
    Deferred.don't_wait_for (Io_uring_raw.cancel t.uring running_job)
  | None ->
    raise_s
      [%sexp
        "Attempted to remove polling for a file descriptor that was not being polled"]
;;

let set t file_descr desired =
  let actual_flags =
    match Table.find t.states file_descr with
    | None -> None
    | Some { running_job = _; flags } -> Some flags
  in
  let desired_flags =
    match desired.read, desired.write with
    | false, false -> None
    | true, false -> Some Flags.pollin
    | false, true -> Some Flags.pollout
    | true, true -> Some Flags.(pollin + pollout)
  in
  match actual_flags, desired_flags with
  | None, None -> `Ok
  | None, Some d ->
    add_poll t file_descr d;
    `Ok
  | Some _, None ->
    remove_poll_exn t file_descr;
    `Ok
  | Some a, Some d ->
    if not (Flags.mem a d && Flags.mem d a)
    then (
      remove_poll_exn t file_descr;
      add_poll t file_descr d);
    `Ok
;;

module Pre = struct
  type t = unit [@@deriving sexp_of]
end

let pre_check t =
  (* This has the efect of submitting at the end of every cycle. *)
  let (_ : int) = Io_uring_raw.submit t.uring in
  ()
;;

module Check_result = struct
  type t = bool [@@deriving sexp_of]
end

(* Filling completions is cheap when nothing is ready, so in case we want to timeout
   immediately, it's ok to just try and fill completions without checking the completion
   queue as well. *)
let thread_safe_check (type a) t () (timeout : a Timeout.t) (span_or_unit : a) =
  match timeout with
  | Immediately -> true
  | After -> Io_uring_raw.cqe_ready t.uring ~timeout:(Time_ns.Span.to_sec span_or_unit)
;;

let post_check t ready =
  if ready
  then (
    let (_ : int) = Io_uring_raw.fill_completions t.uring in
    ())
;;
