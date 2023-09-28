open! Core
open! Import
module _ = Io_uring_raw_null

[%%import "io_uring_config.h"]
[%%ifdef JSC_IO_URING]

module Uring = Ocaml_uring.Uring
module Int63 = Optint.Int63
module Poll_mask = Uring.Poll_mask

module Clock = struct
  type t =
    | Boottime
    | Realtime

  let to_uring_clock t =
    match t with
    | Boottime -> Uring.Boottime
    | Realtime -> Uring.Realtime
  ;;
end

module Statx = Uring.Statx
module Open_flags = Uring.Open_flags
module Resolve = Uring.Resolve

module Syscall_result = struct
  type t = (int, Unix.Error.t) Result.t [@@deriving sexp_of]
end

type t = Syscall_result.t Ivar.t Uring.t

module Status = struct
  type t =
    | To_prepare
    | Prepared_or_finished
    | Cancel_prepared of (Syscall_result.t Ivar.t[@sexp.opaque])
    | Cancelled_early
  [@@deriving sexp_of]
end

module Handle = struct
  type t =
    { result : Syscall_result.t Ivar.t
    ; mutable job : Syscall_result.t Ivar.t Ocaml_uring.Uring.job option
    ; mutable status : Status.t
    }
  [@@deriving fields ~getters]

  let invariant (t : t) =
    try
      match t.status with
      | To_prepare ->
        assert (Ivar.is_empty t.result);
        assert (Option.is_none t.job)
      | Prepared_or_finished | Cancel_prepared _ -> assert (Option.is_some t.job)
      | Cancelled_early -> assert (Ivar.is_full t.result)
    with
    | exn -> raise_s [%message "Io_uring_raw.Status.invariant failed" (exn : exn)]
  ;;

  let set_job t new_job = t.job <- new_job
  let set_status t new_status = t.status <- new_status
end

let supports_ops probe =
  List.for_all
    ~f:(fun op -> Uring.op_supported probe op)
    Uring.Op.
      [ nop
      ; read
      ; write
      ; readv
      ; writev
      ; poll_add
      ; openat2
      ; close
      ; linkat
      ; unlinkat
      ; timeout
      ; statx
      ; async_cancel
      ]
;;

let create ?polling_timeout ~queue_depth () =
  let uring = Uring.create ?polling_timeout ~queue_depth () in
  let probe = Uring.get_probe uring in
  if supports_ops probe
  then Ok uring
  else (
    Uring.exit uring;
    error_s
      [%sexp "The underlying kernel does not support all the io_uring operations needed"])
;;

let exit = Uring.exit
let supports_ext_arg = Uring.supports_ext_arg
let register_eventfd = Uring.register_eventfd
let submit t = Uring.submit t
let cqe_ready t = Uring.cqe_ready t

let rec iter_completions_internal io_uring ~f count =
  match Uring.get_cqe_nonblocking io_uring with
  | Some { result; data } ->
    f ~result ~data;
    iter_completions_internal io_uring ~f (count + 1)
  | None -> count
;;

let iter_completions io_uring ~f = iter_completions_internal io_uring ~f 0

let fill_syscall_ivar ~result ~data =
  if result >= 0
  then Ivar.fill_exn data (Ok result)
  else Ivar.fill_exn data (Error (Unix.Error.of_system_int ~errno:(-result)))
;;

let fill_completions t = iter_completions t ~f:fill_syscall_ivar
let max_attempts = -1

let prepare_internal f =
  let ivar = Ivar.create () in
  let handle = { Handle.result = ivar; job = None; status = To_prepare } in
  Deferred.don't_wait_for
    (let rec submit_until_success count =
       match handle.status with
       | To_prepare ->
         (match f ivar with
          | None ->
            if count = max_attempts
            then failwith "Tried resubmitting to the Io_uring queue too many times";
            let%bind () = Async_kernel_scheduler.yield () in
            submit_until_success (count + 1)
          | Some job ->
            Handle.set_status handle Prepared_or_finished;
            Handle.set_job handle (Some job);
            return ())
       | Cancelled_early ->
         Handle.set_status handle Prepared_or_finished;
         return ()
       | Cancel_prepared _ | Prepared_or_finished ->
         raise_s
           [%sexp
             (( "Io_uring_raw syscall found in unexpected state while submitting"
              , Handle.status handle )
               : string * Status.t)]
     in
     submit_until_success 0);
  Deferred.upon (Ivar.read ivar) (fun _ ->
    Handle.set_job handle None;
    (* We need this branching in order to keep the invariant of the submit_until_success
       loop. Otherwise, there is a race that can happen: the submit_until_success job gets
       scheduled, a cancel job moves the job to Cancelled_early and fills its ivar, but
       the ivar being filled triggers this callback that moves it to Prepared_or_finished.
    *)
    match handle.status with
    | Cancelled_early | Prepared_or_finished -> ()
    | Cancel_prepared _ -> Handle.set_status handle Prepared_or_finished
    | To_prepare ->
      raise_s [%sexp "Io_uring_raw syscall ivar filled while in state To_prepare"]);
  handle
;;

let noop t = prepare_internal (Uring.noop t)
let read t ~file_offset fd buf = prepare_internal (Uring.read t ~file_offset fd buf)
let write t ~file_offset fd buf = prepare_internal (Uring.write t ~file_offset fd buf)
let readv t ~file_offset fd bufs = prepare_internal (Uring.readv t ~file_offset fd bufs)
let writev t ~file_offset fd bufs = prepare_internal (Uring.writev t ~file_offset fd bufs)
let poll_add t fd flags = prepare_internal (Uring.poll_add t fd flags)

let openat2 t ~access ~flags ~perm ~resolve ?fd filename =
  prepare_internal (Uring.openat2 t ~access ~flags ~perm ~resolve ?fd filename)
;;

let close t fd = prepare_internal (Uring.close t fd)

let link t ~follow ~target ~link_name =
  prepare_internal (Uring.linkat t ~follow ~target ~link_name)
;;

let unlink t ~dir ?fd filename = prepare_internal (Uring.unlink t ~dir ?fd filename)

let timeout t ?absolute clock timeout_ns =
  let clock = Clock.to_uring_clock clock in
  prepare_internal (Uring.timeout t ?absolute clock timeout_ns)
;;

let statx t ?fd ~mask path statx flags =
  prepare_internal (Uring.statx t ?fd ~mask path statx flags)
;;

let cancel t handle =
  let rec cancel_until_success () =
    if Ivar.is_full (Handle.result handle)
    then return ()
    else (
      match Handle.status handle with
      | Cancelled_early -> return ()
      | Cancel_prepared cancel_ivar ->
        (match%map Ivar.read cancel_ivar with
         | Ok _ -> ()
         | Error Unix.Error.ENOENT ->
           (* The job we are trying to cancel has already finished by the time the cancel
              was executed. *)
           ()
         | Error err -> raise (Unix.Unix_error (err, "cancel", "")))
      | To_prepare ->
        Handle.set_status handle Cancelled_early;
        Ivar.fill_if_empty (Handle.result handle) (Error (Unix.Error.EUNKNOWNERR 125));
        return ()
      | Prepared_or_finished ->
        let cancel_ivar = Ivar.create () in
        (* [Uring.cancel] requires that the completion wasn't collected yet. We know it
           wasn't collected because we just checked [Ivar.is_full result_ivar] earlier.
        *)
        (match Uring.cancel t (Option.value_exn handle.job) cancel_ivar with
         | None ->
           let%bind () = Async_kernel_scheduler.yield () in
           cancel_until_success ()
         | Some _cancel_job ->
           Handle.set_status handle (Cancel_prepared cancel_ivar);
           cancel_until_success ()))
  in
  cancel_until_success ()
;;

let syscall_result handle = Ivar.read (Handle.result handle)

[%%else]

include Io_uring_raw_null

[%%endif]
