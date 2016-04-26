#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"async_unix"
  [ oasis_lib "async_unix"
  ; file "META" ~section:"lib"
  ]
