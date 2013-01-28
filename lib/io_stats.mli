open Core.Std

type t with sexp

val create : unit -> t
val update : t -> kind:Fd.Kind.t -> bytes:Int63.t -> unit
val total : t -> Int63.t
val get : t -> kind:Fd.Kind.t -> Int63.t
