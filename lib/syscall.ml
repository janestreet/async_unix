open Core.Std

let syscall f =
  let max_tries = 1000 in
  let rec loop n =
    if n >= max_tries then
      Error (Error.to_exn
               (Error.create "syscall interrupted too many times" max_tries
                  (<:sexp_of< int >>)))
    else
      match Result.try_with f with
      | Error (Unix.Unix_error (Unix.EINTR, _, _)) -> loop (n + 1)
      | x -> x
  in
  loop 0
;;
