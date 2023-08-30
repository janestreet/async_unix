open Core
module Signal = Core.Signal

type delivered = Signal.t Thread_safe_queue.t

type t =
  { original_dispositions_of_managed_signals : Signal.Expert.behavior Signal.Table.t
  ; delivered : (delivered[@sexp.opaque])
  ; thread_safe_notify_signal_delivered : unit -> unit
  }
[@@deriving sexp_of]

let invariant _ = ()

let create ~thread_safe_notify_signal_delivered =
  { original_dispositions_of_managed_signals = Signal.Table.create ()
  ; delivered = Thread_safe_queue.create ()
  ; thread_safe_notify_signal_delivered
  }
;;

let is_managing t signal = Hashtbl.mem t.original_dispositions_of_managed_signals signal

let manage t signal =
  let _original_disposition =
    Hashtbl.find_or_add
      t.original_dispositions_of_managed_signals
      signal
      ~default:(fun () ->
      Signal.Expert.signal
        signal
        (`Handle
          (fun _ ->
            (* Everything in this function body must be thread safe, since it is running in an
                   OCaml signal handler. *)
            Thread_safe_queue.enqueue t.delivered signal;
            t.thread_safe_notify_signal_delivered ())))
  in
  ()
;;

let iter_delivered t ~f =
  while Thread_safe_queue.length t.delivered > 0 do
    let signal = Thread_safe_queue.dequeue_exn t.delivered in
    let original_disposition =
      Hashtbl.find_exn t.original_dispositions_of_managed_signals signal
    in
    f ~original_disposition signal
  done
;;
