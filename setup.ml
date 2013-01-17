(* setup.ml generated for the first time by OASIS v0.2.0 *)

(* OASIS_START *)
(* DO NOT EDIT (digest: 7f47a529f70709161149c201ccd90f0b) *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)

#use "setup.conf";;

(* Save setup.conf values in setup.data to be used by myocamlbuild.ml *)
let _ =
  let conf_ccopt () = String.concat "\t" ccopt in
  BaseEnv.var_define "conf_ccopt" conf_ccopt

let _ =
  let conf_cclib () = String.concat "\t" cclib in
  BaseEnv.var_define "conf_cclib" conf_cclib


let () = setup()
