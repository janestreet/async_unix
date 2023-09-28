open! Core
open! Import

type 'a additional_create_args = uring:Io_uring_raw.t -> 'a

include
  File_descr_watcher_intf.S
    with type 'a additional_create_args := 'a additional_create_args
