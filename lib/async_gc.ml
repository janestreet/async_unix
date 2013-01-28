open Core.Std

include Gc

(** [add_finalizer f x] is like [Gc.finalise f x], except that the finalizer is guaranteed
    to run as an Async job (i.e. without interrupting other Async jobs).  Unprotected use
    of [Gc.finalise] in Async programs is wrong, because finalizers can run at any time
    and in any thread. *)
let add_finalizer f x =
  Raw_scheduler.(add_finalizer (the_one_and_only ~should_lock:true)) f x
;;

(** [finalise] is rebound to avoid accidental use in Async programs. *)
let finalise (`In_async__use_finalize_not_finalise as x) _ = x
