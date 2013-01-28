(** [Read_write] is like [Dirpair], except "buy/sell" has been changed to "read/write". *)

open Core.Std

module Key : sig
  type t = [ `Read | `Write ] with sexp
  val flip : t -> t
end

type ('a, +'z) any = private { mutable read : 'a; mutable write : 'a }
with sexp

module Immutable : sig
  type 'a t = ('a, immutable) any with sexp
end

module Read_only : sig
  type 'a t = ('a, read_only) any with sexp
end

module Mutable : sig
  type 'a t = ('a, read_write) any with sexp
end

type 'a t = 'a Immutable.t with sexp

(* creation *)
val create      : read:'a -> write:'a -> ('a, _) any
val createi     : (Key.t -> 'a) -> ('a, _) any
val create_both :                  'a -> ('a, _) any
val create_fn   :        (unit -> 'a) -> ('a, _) any
val create_with : Key.t -> 'a -> zero:'a -> ('a, _) any

val copy : ('a, _) any -> ('a, _) any

(* map-like functions *)
val exists  : ('a, _) any -> f:( 'a -> bool ) -> bool
val for_all : ('a, _) any -> f:( 'a -> bool ) -> bool

val iteri : ('a, _) any -> f:(Key.t -> 'a -> unit) -> unit
val iter  : ('a, _) any -> f:(         'a -> unit) -> unit

val mapi : ('a, _) any -> f:(Key.t -> 'a -> 'b ) -> ('b, _) any
val map  : ('a, _) any -> f:(         'a -> 'b ) -> ('b, _) any

val foldi : ('a, _) any -> 'b -> f:('b -> Key.t * 'a -> 'b) -> 'b
val fold  : ('a, _) any -> 'b -> f:('b ->         'a -> 'b) -> 'b

val get : ('a, _) any -> Key.t -> 'a

val replace     : 'a Mutable.t -> Key.t -> f:(         'a -> 'a) -> unit
val replace_all : 'a Mutable.t ->          f:(Key.t -> 'a -> 'a) -> unit

(* mutation *)
val set : 'a Mutable.t -> Key.t -> 'a -> unit

module Export : sig
  type ('a, 'z) read_write_ =
    ('a, 'z) any =
    private { mutable read : 'a; mutable write : 'a }
end
