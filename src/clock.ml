open! Core.Std
open! Import
open Async_kernel.Clock_ns

let run_at    time f a = run_at    (Time_ns.of_time      time) f a
let run_after span f a = run_after (Time_ns.Span.of_span span) f a

let at    time = at    (Time_ns.of_time time)
let after span = after (Time_ns.Span.of_span span)

let with_timeout span d = with_timeout (Time_ns.Span.of_span span) d

module Event = struct
  type t = Event.t with sexp_of

  let invariant = Event.invariant

  let abort = Event.abort

  let at    time = Event.at    (Time_ns.of_time time)
  let after span = Event.after (Time_ns.Span.of_span span)

  let status t =
    match Event.status t with
    | `Happened | `Aborted as x -> x
    | `Will_happen_at time -> `Will_happen_at (Time_ns.to_time time)
  ;;
end

let at_varying_intervals ?stop f =
  at_varying_intervals ?stop (fun () -> (Time_ns.Span.of_span (f ())))
;;

let at_intervals ?start ?stop span =
  let start = Option.map start ~f:Time_ns.of_time in
  at_intervals ?start ?stop (Time_ns.Span.of_span span)
;;

let every' ?start ?stop ?continue_on_error span f =
   every' ?start ?stop ?continue_on_error (Time_ns.Span.of_span span) f
;;

let every ?start ?stop ?continue_on_error span f =
   every ?start ?stop ?continue_on_error (Time_ns.Span.of_span span) f
;;

let run_at_intervals' ?start ?stop ?continue_on_error span f =
  let start = Option.map start ~f:Time_ns.of_time in
  run_at_intervals' ?start ?stop ?continue_on_error (Time_ns.Span.of_span span) f
;;

let run_at_intervals ?start ?stop ?continue_on_error span f =
  let start = Option.map start ~f:Time_ns.of_time in
  run_at_intervals ?start ?stop ?continue_on_error (Time_ns.Span.of_span span) f
;;
