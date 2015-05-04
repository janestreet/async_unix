open Core.Std

val syscall : (unit -> 'a) -> ('a, exn) Result.t
