open Core.Std
open Import

module File_descr = Unix.File_descr
module Scheduler = Raw_scheduler
module Fd = Raw_fd

include Fd.T

open Fd

let debug               = debug
let is_closed           = is_closed
let is_open             = is_open
let syscall             = syscall
let syscall_exn         = syscall_exn
let with_file_descr     = with_file_descr
let with_file_descr_exn = with_file_descr_exn

module Kind = struct
  include Fd.Kind

  let blocking_infer_using_stat file_descr =
    let module U = Unix in
    let st = U.fstat file_descr in
    match st.U.st_kind with
    | U.S_REG | U.S_DIR | U.S_BLK | U.S_LNK -> File
    | U.S_CHR -> Char
    | U.S_FIFO -> Fifo
    | U.S_SOCK ->
      Socket (if U.getsockopt file_descr U.SO_ACCEPTCONN then `Passive else `Active)
  ;;

  let infer_using_stat file_descr =
    In_thread.syscall_exn ~name:"fstat" (fun () -> blocking_infer_using_stat file_descr)
  ;;
end

let to_string t = Sexp.to_string_hum (sexp_of_t t)

let the_one_and_only () = Scheduler.the_one_and_only ~should_lock:true

let create kind file_descr info =
  Scheduler.create_fd (the_one_and_only ()) kind file_descr info;
;;

let create_std_descr file_descr info =
  create (Kind.blocking_infer_using_stat file_descr) file_descr info
;;

let stdin  =
  Memo.unit (fun () -> create_std_descr Unix.stdin (Info.of_string "<stdin>"))
;;

let stdout =
  Memo.unit (fun () -> create_std_descr Unix.stdout (Info.of_string "<stdout>"))
;;

let stderr =
  Memo.unit (fun () -> create_std_descr Unix.stderr (Info.of_string "<stderr>"))
;;

let close ?(should_close_file_descriptor = true) t =
  if debug then Debug.log "Fd.close" t <:sexp_of< t >>;
  let module S = State in
  match t.state with
  | S.Close_requested _ | S.Closed -> Ivar.read t.close_finished
  | S.Open ->
    set_state t (S.Close_requested (fun () ->
      if not should_close_file_descriptor then
        Deferred.unit
      else
        Monitor.protect
          ~finally:(fun () ->
            In_thread.syscall_exn ~name:"close" (fun () -> Unix.close t.file_descr))
          (fun () ->
            match t.kind with
            | Kind.Socket `Active ->
              In_thread.syscall_exn ~name:"shutdown" (fun () ->
                Unix.shutdown t.file_descr ~mode:Unix.SHUTDOWN_ALL)
            | _ -> return ())));
    let module S = Scheduler in
    let scheduler = the_one_and_only () in
    S.request_stop_watching scheduler t `Read `Closed;
    S.request_stop_watching scheduler t `Write `Closed;
    S.maybe_start_closing_fd scheduler t;
    Ivar.read t.close_finished;
;;

let close_finished t = Ivar.read t.close_finished

let with_close t ~f = Monitor.protect (fun () -> f t) ~finally:(fun () -> close t)

let with_file_descr_deferred t f =
  match inc_num_active_syscalls t with
  | `Already_closed -> return `Already_closed
  | `Ok ->
    Monitor.try_with (fun () -> f t.file_descr)
    >>| fun result ->
    Scheduler.dec_num_active_syscalls_fd (the_one_and_only ()) t;
    match result with
    | Ok x -> `Ok x
    | Error e -> `Error e
;;

let start_watching t read_or_write =
  if debug then
    Debug.log "Fd.start_watching" (t, read_or_write) (<:sexp_of< t * Read_write.Key.t >>);
  let r = the_one_and_only () in
  match Scheduler.request_start_watching r t read_or_write with
  | `Unsupported | `Already_closed | `Watching _ as x -> x
  | `Already_watching ->
    failwiths "cannot watch an fd already being watched" (t, r)
      (<:sexp_of< t * Scheduler.t >>)
;;

let ready_to_interruptible t read_or_write ~interrupt =
  if debug then
    Debug.log "Fd.ready_to_interruptible" (t, read_or_write)
      (<:sexp_of< t * Read_write.Key.t >>);
  match start_watching t read_or_write with
  | `Already_closed -> return `Closed
  | `Unsupported -> return `Ready
  | `Watching ivar ->
    upon
      (Deferred.choose
         [ Deferred.choice interrupt (fun () -> `Interrupted);
           Deferred.choice (Ivar.read ivar) (fun _ -> `Not_interrupted);
         ])
      (function
        | `Not_interrupted -> ()
        | `Interrupted ->
          if Ivar.is_empty ivar then
            Scheduler.request_stop_watching (the_one_and_only ())
              t read_or_write `Interrupted);
    Ivar.read ivar
;;

let ready_to t read_or_write =
  if debug then
    Debug.log "Fd.ready_to" (t, read_or_write) (<:sexp_of< t * Read_write.Key.t >>);
  match start_watching t read_or_write with
  | `Already_closed -> return `Closed
  | `Unsupported -> return `Ready
  | `Watching ivar ->
    Ivar.read ivar
    >>| function
      | `Bad_fd | `Closed | `Ready as x -> x
      | `Interrupted -> assert false (* impossible *)
;;

let syscall_in_thread t ~name f =
  with_file_descr_deferred t (fun file_descr ->
    In_thread.syscall ~name (fun () -> f file_descr))
  >>| function
    | `Error e ->
      failwiths "Fd.syscall_in_thread bug -- should be impossible" e (<:sexp_of< exn >>)
    | `Already_closed -> `Already_closed
    | `Ok x ->
      match x with
      | Ok x -> `Ok x
      | Error exn -> `Error exn
;;

let syscall_in_thread_exn t ~name f =
  syscall_in_thread t ~name f
  >>| function
    | `Ok x -> x
    | `Error exn -> raise exn
    | `Already_closed ->
      failwiths "Fd.syscall_in_thread_exn of a closed fd" t <:sexp_of< t >>
;;

let of_in_channel ic kind =
  create kind (Unix.descr_of_in_channel ic) (Info.of_string "<of_in_channel>")
;;

let of_out_channel oc kind =
  create kind (Unix.descr_of_out_channel oc) (Info.of_string "<of_out_channel>")
;;

let of_in_channel_auto ic =
  Kind.infer_using_stat (Unix.descr_of_in_channel ic) >>| of_in_channel ic
;;

let of_out_channel_auto oc =
  Kind.infer_using_stat (Unix.descr_of_out_channel oc) >>| of_out_channel oc
;;

let file_descr_exn t =
  if is_closed t then
    failwiths "Fd.file_descr_exn on already closed fd" t <:sexp_of< t >>
  else
    t.file_descr
;;

let to_int_exn t = File_descr.to_int (file_descr_exn t)

let replace t kind info =
  if is_closed t then
    failwiths "Fd.replace got closed fd" (t, kind, the_one_and_only ())
      (<:sexp_of< t * Kind.t * Scheduler.t >>)
  else begin
    t.kind <- kind;
    t.info <- (Info.create "replaced" (info, `previously_was t.info)
                 (<:sexp_of< Info.t * [ `previously_was of Info.t ] >>));
  end
;;
