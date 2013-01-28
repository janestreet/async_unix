open Core.Std

let syscall f =
  let rec loop n =
    if n >= 1000 then failwith "syscall interrupted too many times";
    match Result.try_with f with
    | Error (Unix.Unix_error (Unix.EINTR, _, _)) -> loop (n + 1)
    | x -> x
  in
  loop 0
;;
