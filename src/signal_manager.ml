open Core
module Signal = Core.Signal

module Behavior_when_no_handlers = struct
  type t =
    | No_op
    | Original_disposition
    (** approximates the default behavior by calling [shutdown] on signals that normally
        cause the process to terminate *)
  [@@deriving sexp_of]
end

module Deliver_result = struct
  type t =
    | Delivered
    | Original_disposition
end

module Handlers = struct
  type t =
    { bag : ((Signal.t -> unit)[@sexp.opaque]) Bag.t
    ; mutable behavior_when_no_handlers : Behavior_when_no_handlers.t
    }
  [@@deriving sexp_of]

  let create () =
    { bag = Bag.create (); behavior_when_no_handlers = Original_disposition }
  ;;

  let add t handler = Bag.add t.bag handler
  let remove t handler_elt = Bag.remove t.bag handler_elt

  let deliver t signal : Deliver_result.t =
    if Bag.is_empty t.bag
    then (
      match t.behavior_when_no_handlers with
      | No_op -> Delivered
      | Original_disposition -> Original_disposition)
    else (
      Bag.iter t.bag ~f:(fun handler ->
        try handler signal with
        | exn -> raise_s [%message "signal handler unexpectedly raised" (exn : exn)]);
      Delivered)
  ;;
end

module Handler = struct
  type t = T of (Handlers.t * (Signal.t -> unit) Bag.Elt.t) list
end

type handler = Handler.t

module type Signal_dispatcher = sig
  type t
  type handler

  val set_composable_handler
    :  t
    -> behavior_when_no_handlers:Behavior_when_no_handlers.t
    -> Signal.t list
    -> unit

  val install_handler : t -> Signal.t list -> (Signal.t -> unit) -> handler
  val remove_handler : t -> handler -> unit
end

module Signal_dispatcher : sig
  type t [@@deriving sexp_of]

  include Signal_dispatcher with type t := t and type handler = handler

  val create : unit -> t
  val dispatch : t -> original_disposition:Signal.Expert.behavior -> Signal.t -> unit
end = struct
  type t = { handlers_by_signal : Handlers.t Signal.Table.t } [@@deriving sexp_of]
  type nonrec handler = handler

  let create () = { handlers_by_signal = Signal.Table.create () }

  let get_handlers t signal =
    Hashtbl.find_or_add t.handlers_by_signal signal ~default:(fun () ->
      Handlers.create ())
  ;;

  let set_composable_handler
    t
    ~(behavior_when_no_handlers : Behavior_when_no_handlers.t)
    signals
    =
    List.iter signals ~f:(fun signal ->
      let handlers = (get_handlers t signal : Handlers.t) in
      match behavior_when_no_handlers with
      | No_op -> handlers.behavior_when_no_handlers <- No_op
      | Original_disposition -> ())
  ;;

  let install_handler t signals handler =
    Handler.T
      (List.map signals ~f:(fun signal ->
         let handlers = get_handlers t signal in
         handlers, Handlers.add handlers handler))
  ;;

  let remove_handler _t (Handler.T handler) =
    List.iter handler ~f:(fun (handlers, handler_elt) ->
      Handlers.remove handlers handler_elt)
  ;;

  let default_signal_handler ~original_disposition signal =
    Async_kernel.Async_kernel_scheduler.schedule (fun () ->
      match (original_disposition : Signal.Expert.behavior) with
      | Ignore -> ()
      | Handle f -> f signal
      | Default ->
        (match Signal.default_sys_behavior signal with
         | `Terminate | `Dump_core -> Shutdown.shutdown_with_signal_exn signal
         | `Stop | `Continue | `Ignore -> ()))
  ;;

  let dispatch t ~original_disposition signal =
    match Hashtbl.find t.handlers_by_signal signal with
    | None -> default_signal_handler ~original_disposition signal
    | Some handlers ->
      (match Handlers.deliver handlers signal with
       | Delivered -> ()
       | Original_disposition -> default_signal_handler ~original_disposition signal)
  ;;
end

type t =
  { raw_signal_manager : Raw_signal_manager.t [@globalized]
  ; signal_dispatcher : Signal_dispatcher.t [@globalized]
  }
[@@deriving sexp_of]

let invariant _ = ()

let create ~thread_safe_notify_signal_delivered =
  { raw_signal_manager = Raw_signal_manager.create ~thread_safe_notify_signal_delivered
  ; signal_dispatcher = Signal_dispatcher.create ()
  }
;;

let is_managing t signal = Raw_signal_manager.is_managing t.raw_signal_manager signal

let set_composable_handler t ~behavior_when_no_handlers signals =
  List.iter signals ~f:(Raw_signal_manager.manage t.raw_signal_manager);
  Signal_dispatcher.set_composable_handler
    t.signal_dispatcher
    ~behavior_when_no_handlers
    signals
;;

let manage t ~(behavior_when_no_handlers : Behavior_when_no_handlers.t) signal =
  set_composable_handler t ~behavior_when_no_handlers [ signal ]
;;

let install_handler t signals f =
  List.iter signals ~f:(Raw_signal_manager.manage t.raw_signal_manager);
  Signal_dispatcher.install_handler t.signal_dispatcher signals f
;;

let remove_handler t handler =
  Signal_dispatcher.remove_handler t.signal_dispatcher handler
;;

let handle_delivered t =
  (* The local_ annotation is there to make sure the closure can't be allocated on the
     heap *)
  let () =
    Raw_signal_manager.iter_delivered
      t.raw_signal_manager
      ~f:(fun ~original_disposition signal ->
        Signal_dispatcher.dispatch ~original_disposition t.signal_dispatcher signal)
  in
  ()
;;
