include Io_uring_raw_intf.S (** @inline *)

val syscall_result_retry_on_ECANCELED
  :  (unit -> Handle.t)
  -> Syscall_result.t Async_kernel.Deferred.t
