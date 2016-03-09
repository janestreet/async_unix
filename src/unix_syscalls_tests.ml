open! Core.Std
open! Async_kernel.Std
open! Import
open! Std

open Unix_syscalls

let%test_unit "fork_exec ~env last binding takes precedence" =
  protectx ~finally:Core.Std.Unix.remove
    (Filename.temp_file "test" "fork_exec.env.last-wins")
    ~f:(fun temp_file ->
      Thread_safe.block_on_async_exn (fun () ->
        let env = [ "VAR", "first"; "VAR", "last" ] in
        Deferred.List.iter
          [ `Replace_raw (List.map env ~f:(fun (v, s) -> v ^ "=" ^ s))
          ; `Replace env
          ; `Extend env
          ]
          ~f:(fun env ->
            fork_exec () ~env ~prog:"sh" ~args:[ "sh"; "-c"; "echo $VAR > " ^ temp_file ]
            >>= waitpid_exn
            >>| fun () ->
            [%test_result: string] ~expect:"last\n" (In_channel.read_all temp_file))))
;;
