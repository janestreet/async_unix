
open Core.Std
open Import
open File_descr_watcher_intf
open Read_write.Export

module Epoll = Linux_ext.Epoll

module Flags = struct

  include Epoll.Flags

  let in_out = in_ + out

  let of_rw = function `Read -> in_ | `Write -> out

end

type t =
  { epoll : Epoll.t;
  }
with sexp_of, fields

let invariant t : unit =
  try
    let check f = fun field -> f (Field.get field t) in
    Fields.iter
      ~epoll:(check (fun epoll ->
        Epoll.iter epoll ~f:(fun _ flags ->
          assert (List.exists Flags.([ in_; out; in_out ])
                    ~f:(fun flags' -> Flags.equal flags flags')))))
  with exn ->
    failwiths "Epoll_file_descr_watcher.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let create ~num_file_descrs =
  let epoll_create = Or_error.ok_exn Epoll.create in
  { epoll = epoll_create ~num_file_descrs ~max_ready_events:Config.epoll_max_ready_events;
  }
;;

let iter t ~f =
  Epoll.iter t.epoll ~f:(fun file_descr flags ->
    if Flags.do_intersect flags Flags.in_ then f file_descr `Read;
    if Flags.do_intersect flags Flags.out then f file_descr `Write);
;;

let set t file_descr desired =
  let actual_flags = Epoll.find t.epoll file_descr in
  let desired_flags =
    match desired.read, desired.write with
    | false, false -> None
    | true , false -> Some Flags.in_
    | false, true  -> Some Flags.out
    | true , true  -> Some Flags.in_out
  in
  match actual_flags, desired_flags with
  | None  , None   -> ()
  | None  , Some d -> Epoll.set t.epoll file_descr d
  | Some _, None   -> Epoll.remove t.epoll file_descr
  | Some a, Some d -> if not (Flags.equal a d) then Epoll.set t.epoll file_descr d
;;

module Pre = struct
  type t = unit with sexp_of
end

let pre_check _t = ()

module Check_result = struct
  type t = ([ `Ok | `Timeout ], exn) Result.t
  with sexp_of
end

let thread_safe_check t () ~timeout =
  (* From the epoll man page:

     | Specifying a timeout of -1 makes epoll_wait() wait indefinitely, while specifying a
     | timeout equal to zero makes epoll_wait() to return immediately even if no events
     | are available (return code equal to zero).  *)
  let timeout = Option.value timeout ~default:(sec (-1.)) in
  Result.try_with (fun () -> Epoll.wait t.epoll ~timeout);
;;

let post_check t check_result =
  try
    match check_result with
    (* We think 514 should be treated like EINTR. *)
    | Error (Unix.Unix_error ((Unix.EINTR | Unix.EUNKNOWNERR 514), _, _)) ->
      `Syscall_interrupted
    | Error exn -> failwiths "epoll raised unexpected exn" exn <:sexp_of< exn >>
    | Ok e ->
      match e with
      | `Timeout -> `Timeout
      | `Ok ->
        let ready = Read_write.create_both [] in
        Epoll.iter_ready t.epoll ~f:(fun file_descr flags ->
          (* A difference between select and epoll crops up here.  epoll has an implicit
             event flag for hangup (HUP), whereas select will just return that fd as
             "ready" in its appropriate fd_set.  Since we don't know if it's ready for IN
             or OUT, we have to go lookup the entry if the HUP flag is set *)
          let hup = Flags.do_intersect flags Flags.hup in
          Read_write.replace_all ready ~f:(fun read_or_write ready ->
            let bit = Flags.of_rw read_or_write in
            if Flags.do_intersect flags bit
              || (hup && Flags.do_intersect (Epoll.find_exn t.epoll file_descr) bit)
            then
              file_descr :: ready
            else
              ready));
        `Ok (Read_write.map ready ~f:(fun ready -> { Post. ready; bad = [] }))
  with exn ->
    failwiths "Epoll.post_check bug" (exn, check_result, t)
      (<:sexp_of< exn * Check_result.t * t >>)
;;
