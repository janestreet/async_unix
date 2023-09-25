open! Core

(** This module contains a singleton that wraps over different possible drivers of the
    [Io_uring_raw]. Currently it only supports an [Eventfd] driver or not using
    [Io_uring] at all. The underlying implementation can be picked via the
    io_uring_mode async config.
*)

val the_one_and_only : unit -> Io_uring_raw.t option
