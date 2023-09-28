(** [Io_uring_raw] is Async's wrapper over the Ocaml_uring API for using io_uring.

    Each Io_uring_raw has an internal submission queue and a completion queue.
    Whenever you make a syscall it is actually only added to the submission queue and
    you receive a Handle that can be used to either get the Deferred of the underlying
    syscall or cancel the syscall entirely (cancellation is only best effort, not a guarantee).

    After some syscalls have been queued in the submission queue they can be sent to the
    kernel in a batch via [submit] which will send all or part of the submission queue.

    At this point the kernel will take care of the syscalls and whenever one is completed,
    the corresponding event is added to the completion queue.

    The completion queue can be consumed using [fill_completions] which will make the
    syscalls deferred be filled up and makes room for new syscalls to be submitted.
    There is no guarantee about the order in which syscalls are completed.

    Extra care should be taken when using this module as certain usages might cause
    starvation or even deadlocks. For instance, it could be the case that we fill up
    the queue with very slow syscalls and are unable to quickly execute cheap syscalls
    while waiting. Even worse, we might fill up the queue with blocking syscalls that
    can only be executed after an additional syscall is made - which we cannot submit.

    For further documenation on the internals of each syscall, read the analog documentation
    in external/lib/ocaml_uring. *)

open! Core
open Import

module type S = sig
  include Io_uring_types_intf.S

  type t

  module Syscall_result : sig
    type t = (int, Unix.Error.t) Result.t [@@deriving sexp_of]
  end

  module Handle : sig
    type t

    include Invariant.S with type t := t
  end

  val create : ?polling_timeout:int -> queue_depth:int -> unit -> t Or_error.t
  val supports_ext_arg : t -> bool
  val exit : t -> unit
  val register_eventfd : t -> File_descr.t -> unit
  val submit : t -> int
  val cqe_ready : t -> timeout:float -> bool
  val fill_completions : t -> int
  val noop : t -> Handle.t
  val read : t -> file_offset:Int63.t -> File_descr.t -> Cstruct.t -> Handle.t
  val write : t -> file_offset:Int63.t -> File_descr.t -> Cstruct.t -> Handle.t
  val readv : t -> file_offset:Int63.t -> File_descr.t -> Cstruct.t list -> Handle.t
  val writev : t -> file_offset:Int63.t -> File_descr.t -> Cstruct.t list -> Handle.t
  val poll_add : t -> File_descr.t -> Poll_mask.t -> Handle.t

  (** Openat2 will fail if non-zero perms are passed while no file is being created
      (i.e. when creat or tmpfile are not passed as flags) *)
  val openat2
    :  t
    -> access:[ `R | `W | `RW ]
    -> flags:Open_flags.t
    -> perm:Unix.file_perm
    -> resolve:Resolve.t
    -> ?fd:File_descr.t
    -> string
    -> Handle.t

  val close : t -> File_descr.t -> Handle.t
  val link : t -> follow:bool -> target:string -> link_name:string -> Handle.t
  val unlink : t -> dir:bool -> ?fd:File_descr.t -> string -> Handle.t
  val timeout : t -> ?absolute:bool -> Clock.t -> int64 -> Handle.t

  val statx
    :  t
    -> ?fd:File_descr.t
    -> mask:Statx.Mask.t
    -> string
    -> Statx.t
    -> Statx.Flags.t
    -> Handle.t

  val cancel : t -> Handle.t -> unit Deferred.t
  val syscall_result : Handle.t -> Syscall_result.t Deferred.t
end
