(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    flag ["c"; "compile"] & S[A"-I"; A"lib"; A"-package"; A"core"; A"-thread"]
| _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
