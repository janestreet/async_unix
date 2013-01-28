open Core.Std

module Signal = Core.Std.Signal

module Handlers = struct
  type t = { bag : (Signal.t -> unit) sexp_opaque Bag.t } with sexp_of

  let create () = { bag = Bag.create () }

  let add t handler = Bag.add t.bag handler

  let remove t handler_elt = Bag.remove t.bag handler_elt

  let deliver t signal = Bag.iter t.bag ~f:(fun handler -> handler signal)
end

type t =
  { handlers_by_signal : Handlers.t Signal.Table.t;
    delivered : (Signal.t * Handlers.t) Thread_safe_queue.t sexp_opaque;
    thread_safe_notify_signal_delivered : unit -> unit;
  }
with sexp_of

let invariant _ = ()

let create ~thread_safe_notify_signal_delivered =
  { handlers_by_signal = Signal.Table.create ();
    delivered = Thread_safe_queue.create ();
    thread_safe_notify_signal_delivered;
  }
;;

module Handler = struct
  type t = T of (Handlers.t * (Signal.t -> unit) Bag.Elt.t) list
end

type handler = Handler.t

let get_handlers t signal =
  Hashtbl.find_or_add t.handlers_by_signal signal ~default:(fun () ->
    let handlers = Handlers.create () in
    Signal.handle signal (fun _ ->
      (* Everything in this function body must be thread safe, since it is running in an
         OCaml signal handler. *)
      Thread_safe_queue.enqueue t.delivered (signal, handlers);
      t.thread_safe_notify_signal_delivered ());
    handlers)
;;

let handle_signal t signal = ignore (get_handlers t signal : Handlers.t)

let install_handler t signals handler =
  Handler.T
    (List.map signals ~f:(fun signal ->
      let handlers = get_handlers t signal in
      (handlers, Handlers.add handlers handler)))
;;

let remove_handler _t (Handler.T handler) =
  List.iter handler ~f:(fun (handlers, handler_elt) ->
    Handlers.remove handlers handler_elt)
;;

let handle_delivered t =
  Thread_safe_queue.dequeue_until_empty t.delivered (fun (signal, handlers) ->
    Handlers.deliver handlers signal)
;;
