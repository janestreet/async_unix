(* Although this seems pointless, separating Signal_handlers (whose .mli
   references the Signal module) from Raw_signal_handlers prevents a
   dependency cycle when using ocamlbuild. *)

include Raw_signal_manager
