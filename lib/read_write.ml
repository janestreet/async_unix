open Core.Std

module Key = struct
  type t = [ `Read | `Write ] with sexp
end

type ('a, +'z) any = { mutable read : 'a; mutable write : 'a } 
with sexp

module Immutable = struct
  type 'a t = ('a, immutable) any with sexp
end

module Read_only = struct
  type 'a t = ('a, read_only) any with sexp
end

module Mutable = struct
  type 'a t = ('a, read_write) any with sexp
end

type 'a t = 'a Immutable.t with sexp

let create ~read ~write = { read; write }

let create_both x = { read = x; write = x }

let create_fn f = { read = f (); write = f () }

let exists t ~f = f t.read || f t.write

let for_all t ~f = f t.read && f t.write

let iter t ~f = f t.read; f t.write

let map t ~f = { read = f t.read; write = f t.write }

let mapi t ~f = { read = f `Read t.read; write = f `Write t.write }

let get t key =
  match key with
  | `Read -> t.read
  | `Write -> t.write
;;

let set t key value =
  match key with 
  | `Read -> t.read <- value
  | `Write -> t.write <- value
;;

module Export = struct
  type ('a, 'z) read_write_ = ('a, 'z) any = { mutable read : 'a; mutable write : 'a } 
end
