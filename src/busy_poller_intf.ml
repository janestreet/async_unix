(** Busy pollers are added to the scheduler via [Scheduler.add_busy_poller]. Before each
    Async cycle, the Scheduler will call each busy poller at least once, but possibly
    multiple times if there's no Async work to do.

    It is expected that a single call to [poll] does multiple iterations.
    The busy poll loop is planned to run until [~deadline]. Pollers should use this
    parameter to decide how many iterations to run.
    In particular, if we know there is Async work to do, [~deadline] will be in the past
    and pollers are requested to do a single iteration. *)

open Core

module type S = sig
  type t

  val poll : t -> deadline:Time_stamp_counter.t -> int
  val kind : t Type_equal.Id.t
end

module type Busy_poller = sig
  module type S = S

  type poll_f := deadline:Time_stamp_counter.t -> int

  module Empty_poller : S with type t = unit
  module Extra_poller : S with type t = poll_f

  type packed = T : (module S with type t = 'a) * 'a -> packed

  val poll : packed -> deadline:Time_stamp_counter.t -> int
  val create : (module S with type t = 'a) -> 'a -> packed
  val create' : poll_f -> packed
  val empty : packed
end
