open! Core
include Int.Replace_polymorphic_compare
module Async_kernel_config = Async_kernel.Async_kernel_config
module Async_kernel_private = Async_kernel.Async_kernel_private
module Debug = Async_kernel_private.Debug

let sec = Time_ns.Span.of_sec
