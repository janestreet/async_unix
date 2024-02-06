open! Core
open! Import

type t =
  { output : Bigstring.t -> pos:int -> len:int -> unit
  ; flush : unit -> unit
  ; sexp : unit -> Sexp.t
  }
[@@deriving fields ~getters ~iterators:iter]

let sexp_of_t t = t.sexp ()

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let _check f = Invariant.check_field t f in
    Fields.iter ~output:ignore ~flush:ignore ~sexp:ignore)
;;

let create ~output ~flush ~sexp = { output; flush; sexp }

let of_out_channel out_channel : t =
  let bytes_buf = Bytes.of_string "" |> ref in
  create
    ~output:(fun buf ~pos ~len ->
      if len > Bytes.length !bytes_buf then bytes_buf := Bytes.create (len * 2);
      Bigstring.To_bytes.blit ~len ~src:buf ~src_pos:pos ~dst:!bytes_buf ~dst_pos:0;
      Out_channel.output out_channel ~buf:!bytes_buf ~pos:0 ~len)
    ~flush:(fun () -> Out_channel.flush out_channel)
    ~sexp:(fun () -> [%sexp { out_channel : Out_channel.t }])
;;

let output_iovec t (iovec : Bigstring.t Core_unix.IOVec.t) =
  t.output iovec.buf ~pos:iovec.pos ~len:iovec.len
;;

let flush t = t.flush ()
