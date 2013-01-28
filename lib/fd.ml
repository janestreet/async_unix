open Core.Std
open Import

module File_descr = Unix.File_descr
module Scheduler = Raw_scheduler
module Fd = Raw_fd

include Fd.T

open Fd

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
    In_thread.syscall_exn (fun () -> blocking_infer_using_stat file_descr)
  ;;
end

let to_string t = Sexp.to_string_hum (sexp_of_t t)

let the_one_and_only () = Scheduler.the_one_and_only ~should_lock:true

let create kind file_descr ~name =
  Scheduler.create_fd (the_one_and_only ()) kind file_descr ~name;
;;

let create_std_descr file_descr ~name =
  create (Kind.blocking_infer_using_stat file_descr) file_descr ~name
;;

let stdin  = Memo.unit (fun () -> create_std_descr ~name:"<stdin>"  Unix.stdin )
let stdout = Memo.unit (fun () -> create_std_descr ~name:"<stdout>" Unix.stdout)
let stderr = Memo.unit (fun () -> create_std_descr ~name:"<stderr>" Unix.stderr)

let close t =
  let module S = State in
  match t.state with
  | S.Replaced -> failwiths "may not close replaced fd" t <:sexp_of< t >>
  | S.Close_requested _ | S.Closed -> Ivar.read t.close_finished
  | S.Open ->
    set_state t (S.Close_requested (fun () ->
      Monitor.protect
        ~finally:(fun () -> In_thread.syscall_exn (fun () -> Unix.close t.file_descr))
        (fun () ->
          match t.kind with
          | Kind.Socket `Active ->
            In_thread.syscall_exn (fun () ->
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
  let r = the_one_and_only () in
  match Scheduler.request_start_watching r t read_or_write ~interrupt_select:true with
  | `Already_closed | `Watching _ as x -> x
  | `Already_watching ->
    failwiths "cannot watch an fd already being watched" (t, r)
      (<:sexp_of< t * Scheduler.t >>)
;;

let ready_to_interruptible t read_or_write ~interrupt =
  match start_watching t read_or_write with
  | `Already_closed -> return `Closed
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
  match start_watching t read_or_write with
  | `Already_closed -> return `Closed
  | `Watching ivar ->
    Ivar.read ivar
    >>| function
      | `Bad_fd | `Closed | `Ready as x -> x
      | `Interrupted -> assert false (* impossible *)
;;

let syscall_in_thread t f =
  with_file_descr_deferred t (fun file_descr -> In_thread.syscall (fun () -> f file_descr))
  >>| function
    | `Error _ -> assert false
    | `Already_closed -> `Already_closed
    | `Ok x ->
      match x with
      | Ok x -> `Ok x
      | Error exn -> `Error exn
;;

let syscall_in_thread_exn t f =
  syscall_in_thread t f
  >>| function
    | `Ok x -> x
    | `Error exn -> raise exn
    | `Already_closed ->
      failwiths "Fd.syscall_in_thread_exn of a closed fd" t <:sexp_of< t >>
;;

let of_in_channel ic kind =
  create kind (Unix.descr_of_in_channel ic) ~name:"<of_in_channel>"
;;

let of_out_channel oc kind =
  create kind (Unix.descr_of_out_channel oc) ~name:"<of_out_channel>"
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

let replace t kind =
  if is_closed t || t.num_active_syscalls > 0 then
    failwiths "invalid replace" (t, kind) <:sexp_of< t * Kind.t >>
  else begin
    set_state t State.Replaced;
    Scheduler.remove_fd (the_one_and_only ()) t;
    create kind t.file_descr ~name:t.name;
  end
;;
