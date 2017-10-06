(* Create precision dependent OCaml files *)

open Base
open Stdio
open Printf

module Filename = Caml.Filename
module Scanf = Caml.Scanf

let src = "."

(* Utils
 ***********************************************************************)

let comment_re = Str.regexp "(\\* [^*]+\\*)[ \n\r\t]*"

let input_file ?(path=src) ?(comments=true) ?(prefix="") fname =
  In_channel.with_file (Filename.concat path fname) ~f:(fun ic ->
    let buf = Buffer.create 2048 in
    In_channel.iter_lines ic ~f:(fun l ->
      if String.(l <> "") then begin
        Buffer.add_string buf prefix;
        Buffer.add_string buf l
      end;
      Buffer.add_char buf '\n');
    let buf = Buffer.contents buf in
    if comments then buf
    else Str.global_replace comment_re "" buf)

let output_file ?(path=src) fname ~content =
  Out_channel.write_all (Filename.concat path fname) ~data:content

let ocaml_major, ocaml_minor =
  Scanf.sscanf Sys.ocaml_version "%i.%i" (fun v1 v2 -> v1, v2)

let has_module_type_of =
  ocaml_major > 3 || (ocaml_major = 3 && ocaml_minor >= 12)

let has_type_level_module_aliases =
  ocaml_major > 4 || (ocaml_major = 4 && ocaml_minor >= 2)

(* Generating precision dependent files
 ***********************************************************************)

let sig_module_type_of_re =
  Str.regexp ": *module type of +\\([A-Za-z0-9_]+\\)"

let inc_module_type_of_re =
  Str.regexp "^\\( *\\)include module type of +\\([A-Za-z0-9_]+\\)"

(* Replace [open Types.Vec] and [open Types.Mat] by their explicit
   definition (when [full_doc] is desired).  [Lacaml__float*] and
   [Lacaml__complex*] are thus internal modules. *)
let explicit_vec_mat s =
  let s = Str.global_replace (Str.regexp "^ *open *Lacaml__float[0-9]+ *\n")
            "" s in
  let s = Str.global_replace (Str.regexp "^ *open *Lacaml__complex[0-9]+ *\n")
            "" s in
  (* Only replace the 1st ones. *)
  let type_vec = Str.regexp " *open *Types.Vec *" in
  let s = Str.replace_first type_vec
            "  type unop =\n    \
             ?n : int ->\n    \
             ?ofsy : int -> ?incy : int -> ?y : vec ->\n    \
             ?ofsx : int -> ?incx : int -> vec\n    \
             -> vec\n\
             \n  \
             type binop =\n    \
             ?n : int ->\n    \
             ?ofsz : int -> ?incz : int -> ?z : vec ->\n    \
             ?ofsx : int -> ?incx : int -> vec ->\n    \
             ?ofsy : int -> ?incy : int -> vec\n    \
             -> vec" s in
  let s = Str.global_replace type_vec "" s in
  let type_mat = Str.regexp " *open *Types.Mat *" in
  let s = Str.replace_first type_mat
            "  type patt = [\n    \
             | `full  (* Full matrix *)\n    \
             | `utr  (* Upper triangular or trapezoidal matrix *)\n    \
             | `ltr  (* lower triangular or trapezoidal matrix *)\n    \
             | `upent of int  \
               (* Initial full rows of pentagonal matrix *)\n    \
             | `lpent of int  \
               (* Initial full columns of pentagonal matrix *)\n  \
             ]\n\
             \n  \
             type unop =\n    \
             ?patt : patt ->\n    \
             ?m : int -> ?n : int ->\n    \
             ?br : int -> ?bc : int -> ?b : mat ->\n    \
             ?ar : int -> ?ac : int -> mat\n    \
             -> mat\n\
             \n  \
             type binop =\n    \
             ?patt : patt ->\n    \
             ?m : int -> ?n : int ->\n    \
             ?cr : int -> ?cc : int -> ?c : mat ->\n    \
             ?ar : int -> ?ac : int -> mat ->\n    \
             ?br : int -> ?bc : int -> mat\n    \
             -> mat" s in
  Str.global_replace type_mat "" s


(* [full_doc] means that one wants all "include module type of" to be
   replaced with the actual .mli content to be easier to read anb search. *)
let rec substitute fname0 fname1 ?(full_doc=false) subs =
  let ml0 = input_file fname0 in
  let s = substitute_string ~full_doc ml0 subs in
  output_file fname1 ~content:(if full_doc then explicit_vec_mat s else s)

and substitute_string ~full_doc s subs =
  let s =
    List.fold_left ~f:(fun l (r,s) -> Str.global_replace r s l) ~init:s subs
  in
  (* Substitute [module type of] used alone as a sig. *)
  let s =
    if has_type_level_module_aliases then
      Str.global_replace sig_module_type_of_re "= \\1" s
    else if not has_module_type_of then
      let subst s =
        let m = string_of_mod_name ~prefix:"  " ~full_doc
                  (Str.matched_group 1 s) subs in
        String.concat [": sig\n"; m; "\nend\n"] in
      Str.global_substitute sig_module_type_of_re subst s
    else s in
  (* Substitute [module type of] if not supported of explicit doc is desired. *)
  if has_module_type_of && not full_doc then s
  else (
    let subst s =
      string_of_mod_name ~prefix:(Str.matched_group 1 s) ~full_doc
        (Str.matched_group 2 s) subs in
    Str.global_substitute inc_module_type_of_re subst s
  )

and string_of_mod_name ~prefix ~full_doc mname subs =
  let fincl = String.uncapitalize mname ^ ".mli" in
  try
    let s' = input_file fincl ~comments:false ~prefix in
    substitute_string ~full_doc s' subs
  with Sys_error _ ->
    failwith(sprintf "Trying to replace \"include module type of %s\" \
                      but the file %S does not exist"
               mname fincl)

(* [derived] is a list of (new_suffix, substitutions).  Returns the
   list of created files. *)
let derived_files ?(prefix=true) ?full_doc fnames suffix derived =
  let re = Str.regexp ("\\([a-zA-Z]*\\)" ^ suffix ^ "$") in
  let derive fname =
    if Str.string_match re fname 0 then (
      let seed = Str.matched_group 1 fname in
      if String.(seed <> "lacaml") then (
        let derive1 (new_suffix, subs) =
          let fname1 = seed ^ new_suffix in
          let fname1 = if prefix then "lacaml__" ^ fname1 else fname1 in
          substitute fname fname1 ?full_doc subs;
        in
        List.iter ~f:derive1 derived;
      )) in
  Array.iter ~f:derive fnames

let () =
  let fnames = Caml.Sys.readdir src in
  let derive ?full_doc suffix subs =
    derived_files ?full_doc fnames suffix subs in
  let r subs = List.map ~f:(fun (r,s) -> (Str.regexp r, s)) subs in
  let num_type n = (Str.regexp "num_type\\( *[^= ]\\)", n ^ "\\1") in
  let num_type_float = num_type "float" in
  let num_type_complex = num_type "Complex.t" in

  let float32 =
    r [
      "NPREC", "S";
      "NBPREC", "S";
      "numberxx", "float32";
      "num_type_arg", "(float [@unboxed])";
    ]

  and float64 =
    r [
      "NPREC", "D";
      "NBPREC", "D";
      "numberxx", "float64";
      "num_type_arg", "(float [@unboxed])";
    ]

  and complex32 =
    r [
      "NPREC", "C";
      "NBPREC", "S";
      "numberxx", "complex32";
      "num_type_arg", "num_type";
    ]

  and complex64 =
    r [
      "NPREC", "Z";
      "NBPREC", "D";
      "numberxx", "complex64";
      "num_type_arg", "num_type";
    ]
  in

  derive "_SDCZ.mli" [("4_S.mli", num_type_float :: float32);
                      ("4_D.mli", num_type_float :: float64);
                      ("4_C.mli", num_type_complex :: complex32);
                      ("4_Z.mli", num_type_complex :: complex64) ];
  derive "_SDCZ.ml"  [("4_S.ml", float32);   ("4_D.ml", float64);
                      ("4_C.ml", complex32); ("4_Z.ml", complex64) ];

  let float32 =
    r [
      "FPREC", "S";
      "floatxx", "float32";
      "num_type_arg", "(float [@unboxed])";
    ]

  and float64 =
    r [
      "FPREC", "D";
      "floatxx", "float64";
      "num_type_arg", "(float [@unboxed])";
    ]

  and complex32 =
    r [
      "CPREC", "C";  "CBPREC", "S";
      "floatxx", "float32";
      "complexxx", "complex32";
      "num_type_arg", "num_type";
    ]

  and complex64 =
    r [
      "CPREC", "Z";
      "CBPREC", "D";
      "floatxx", "float64";
      "complexxx", "complex64";
      "num_type_arg", "num_type";
    ]
  in

  derive "_SD.mli" [("2_S.mli", float32); ("2_D.mli", float64)];
  derive "_SD.ml"  [("2_S.ml",  float32); ("2_D.ml", float64)];
  derive "SD.ml"   [("S.ml", float32);     ("D.ml", float64)];
  derive "SD.mli"  [("S.mli", num_type_float :: float32);
                    ("D.mli", num_type_float :: float64)] ~full_doc:true;
  derive "_CZ.mli" [("2_C.mli", complex32); ("2_Z.mli", complex64)];
  derive "_CZ.ml"  [("2_C.ml",  complex32); ("2_Z.ml",  complex64)];
  derive "CZ.ml"   [("C.ml", complex32);  ("Z.ml", complex64)];
  derive "CZ.mli"  [("C.mli", num_type_complex :: complex32);
                    ("Z.mli", num_type_complex :: complex64)] ~full_doc:true


(* lacaml.mli
 ***********************************************************************)

let () =
  (* Will also resolve the "module type of" *)
  substitute "lacaml.mli.h" "lacaml.mli" []
