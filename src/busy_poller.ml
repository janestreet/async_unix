open Core

module type S = Busy_poller_intf.S

module Empty_poller = struct
  type t = unit

  let poll (_t : t) ~deadline:_ = 0
  let kind = Type_equal.Id.create ~name:"empty" [%sexp_of: _]
end

module Extra_poller = struct
  type t = deadline:Time_stamp_counter.t -> int

  let poll (t : t) ~deadline = t ~deadline
  let kind = Type_equal.Id.create ~name:"extra" [%sexp_of: _]
end

type packed = T : (module S with type t = 'a) * 'a -> packed

let[@inline always] poll (T ((module P), poller)) ~deadline = P.poll poller ~deadline
let create impl poller = T (impl, poller)
let create' f = create (module Extra_poller) f
let empty = create (module Empty_poller) ()
