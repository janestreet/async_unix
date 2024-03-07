open! Import
include Writer_intf.Writer

(** [splice_result ~from t] moves all data from [from : Reader.t] to [t] one chunk at a
    time. The result becomes determined with value [`Ok] after reaching EOF on [from]
    and the final bytes have been transferred, and with values [`Consumer_left|`Error] in
    those cases where [flushed_or_failed_with_result] would return [Consumer_left|Error]
    respectively.

    The latter situation has the same race condition as [flushed_or_failed_with_result],
    so one should consider calling [set_raise_when_consumer_leaves t false] or at least
    ensure program correctness does not depend on which signal propagates first. *)
val splice_result : t -> from:Reader.t -> [ `Ok | `Consumer_left | `Error ] Deferred.t

(** [splice t ~from] is [splice_result t ~from |> Deferred.ignore_m] *)
val splice : t -> from:Reader.t -> unit Deferred.t
