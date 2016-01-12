open! Core.Std

include Async_kernel.Config

let file_descr_watcher =
  match file_descr_watcher with
  | Epoll | Select as x -> x
  | Epoll_if_timerfd ->
    (* Without timerfd, epoll_wait(2) timeouts would have only millisecond precision. *)
    if Result.is_ok Linux_ext.Timerfd.create
    then Epoll
    else Select

let max_num_open_file_descrs =
  match file_descr_watcher with
  | Epoll | Epoll_if_timerfd -> max_num_open_file_descrs
  | Select ->
    if Max_num_open_file_descrs.equal
         max_num_open_file_descrs
         Max_num_open_file_descrs.default
    then Max_num_open_file_descrs.create_exn 1024
    else max_num_open_file_descrs
;;

let () =
  task_id := fun () ->
    let pid       = Unix.getpid () in
    let thread_id = Thread.id (Thread.self ()) in
    [%sexp_of: [ `pid of Pid.t ] * [ `thread_id of int ]] (`pid pid, `thread_id thread_id)
;;
