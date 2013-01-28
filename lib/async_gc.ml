open Core.Std

include Gc

(** [add_finalizer f x] is like [Gc.finalise f x], except that the finalizer is guaranteed
    to run as an Async job (i.e. without interrupting other Async jobs).  Unprotected use
    of [Caml.Gc.finalise] or [Core.Gc.add_finalizer] in Async programs is wrong, because
    the finalizers won't hold the async lock, and thus could interleave arbitrarily with
    async jobs. *)
let add_finalizer heap_block f =
  Raw_scheduler.(add_finalizer (the_one_and_only ~should_lock:true)) heap_block f
;;

let add_finalizer_exn heap_block f =
  Raw_scheduler.(add_finalizer_exn (the_one_and_only ~should_lock:true)) heap_block f
;;

(** [finalise] is rebound to avoid accidental use in Async programs. *)
let finalise (`In_async__use_finalize_not_finalise as x) _ = x
