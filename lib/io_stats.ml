open Core.Std
open Import

type t = {
  mutable total: Int63.t;
  mutable char: Int63.t;
  mutable fifo: Int63.t;
  mutable file: Int63.t;
  mutable socket: Int63.t
} with sexp

let create () =
  { total  = Int63.zero;
    char   = Int63.zero;
    fifo   = Int63.zero;
    file   = Int63.zero;
    socket = Int63.zero;
  }
;;

let update t ~kind ~bytes =
  t.total <- Int63.(t.total + bytes);
  let module K = Fd.Kind in
  match kind with
  | K.Char -> t.char <- Int63.(t.char + bytes)
  | K.Fifo -> t.fifo <- Int63.(t.fifo + bytes)
  | K.File -> t.file <- Int63.(t.file + bytes)
  | K.Socket _ -> t.socket <- Int63.(t.socket + bytes)
;;

let total t = t.total

let get t ~kind =
  let module K = Fd.Kind in
  match kind with
  | K.Char -> t.char
  | K.Fifo -> t.fifo
  | K.File -> t.file
  | K.Socket _ -> t.socket
;;
