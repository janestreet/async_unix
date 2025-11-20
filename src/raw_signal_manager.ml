open Core
module Signal = Core.Signal

type delivered = Signal.t list Atomic.t

type t =
  { original_dispositions_of_managed_signals : Signal.Expert.behavior Signal.Table.t
  ; delivered : (delivered[@sexp.opaque])
  ; thread_safe_notify_signal_delivered : unit -> unit @@ portable
  }
[@@deriving sexp_of]

let invariant _ = ()

let create ~thread_safe_notify_signal_delivered =
  { original_dispositions_of_managed_signals = Signal.Table.create ()
  ; delivered = Atomic.make []
  ; thread_safe_notify_signal_delivered
  }
;;

let is_managing t signal = Hashtbl.mem t.original_dispositions_of_managed_signals signal

let manage
  { original_dispositions_of_managed_signals
  ; delivered
  ; thread_safe_notify_signal_delivered
  }
  signal
  =
  let _original_disposition =
    Hashtbl.find_or_add
      original_dispositions_of_managed_signals
      signal
      ~default:(fun () ->
        Signal.Expert.signal
          signal
          (Handle
             (fun _ ->
               (* Everything in this function body must be thread safe, since it is
                  running in an OCaml signal handler. *)
               Atomic.update delivered ~pure_f:(fun signals -> signal :: signals);
               thread_safe_notify_signal_delivered ())))
  in
  ()
;;

let iter_delivered t ~f =
  let signals =
    match Atomic.get t.delivered with
    | [] -> []
    | _ -> Atomic.exchange t.delivered [] |> List.rev
  in
  (* The program will crash if any of these raise, so it's not a big deal for the later
     signals in the list to get dropped. *)
  List.iter signals ~f:(fun signal ->
    let original_disposition =
      Hashtbl.find_exn t.original_dispositions_of_managed_signals signal
    in
    f ~original_disposition signal)
  [@nontail]
;;
