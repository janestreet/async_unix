open Core
open Poly
module Unix = Core_unix
module Syscall_result = Unix.Syscall_result

let max_tries = 1000

exception Interrupted_too_many_times

let syscall_exn =
  let rec loop f n =
    if n >= max_tries
    then raise Interrupted_too_many_times
    else (
      try f () with
      | Unix.Unix_error (EINTR, _, _) -> loop f (n + 1))
  in
  fun f -> loop f 0
;;

let too_many_tries =
  Error.to_exn
    (Error.create "syscall interrupted too many times" max_tries [%sexp_of: int])
;;

let raise_too_many_tries () =
  raise_s [%sexp "syscall interrupted too many times", { max_tries : int }]
;;

let too_many_tries_error = Error too_many_tries

let syscall f =
  match syscall_exn f with
  | x -> Ok x
  | exception Interrupted_too_many_times -> too_many_tries_error
  | exception exn -> Error exn
;;

let syscall_exn f =
  try syscall_exn f with
  | Interrupted_too_many_times -> raise_too_many_tries ()
;;

let is_eintr r = Syscall_result.is_error r && Syscall_result.error_exn r = EINTR

let syscall_result =
  let rec loop a f n =
    if n >= max_tries
    then raise too_many_tries
    else (
      let r = f a in
      if not (is_eintr r) then r else loop a f (n + 1))
  in
  fun a f -> loop a f 0
;;

let syscall_result2 =
  let rec loop a b f n =
    if n >= max_tries
    then raise too_many_tries
    else (
      let r = f a b in
      if not (is_eintr r) then r else loop a b f (n + 1))
  in
  fun a b f -> loop a b f 0
;;
