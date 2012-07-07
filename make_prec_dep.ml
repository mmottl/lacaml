(* Create the precision dependent files for the OCaml code.
   Special rules for the C code are implemented in myocamlbuild.ml
 *)

#load "str.cma";;
open Printf

let lib = "lib"

(* Utils
 ***********************************************************************)

let comment_re = Str.regexp "(\\* [^*]+\\*)[ \n\r\t]*"

let input_file ?(path=lib) ?(comments=true) ?(prefix="") fname =
  let fh = open_in (Filename.concat path fname) in
  let buf = Buffer.create 2048 in
  try
    while true do
      let l = input_line fh in (* or exn *)
      if l <> "" then (Buffer.add_string buf prefix;
                      Buffer.add_string buf l );
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    close_in fh;
    let buf = Buffer.contents buf in
    if comments then buf
    else Str.global_replace comment_re "" buf

let output_file ?(path=lib) fname ~content =
  let fh = open_out (Filename.concat path fname) in
  output_string fh content;
  close_out fh

let ocaml_major, ocaml_minor =
  Scanf.sscanf Sys.ocaml_version "%i.%i" (fun v1 v2 -> v1, v2)

let has_module_type_of =
  ocaml_major > 3 || (ocaml_major = 3 && ocaml_minor >= 12)


(* Generating precision dependent files & mlpack
 ***********************************************************************)

let module_type_of_re =
  Str.regexp "^\\( *\\)include module type of +\\([A-Za-z0-9]+_[SDCZ]\\)"

let substitute fname0 fname1 subs =
  let ml0 = input_file fname0 in
  let ml0 = List.fold_left (fun l (r,s) -> Str.global_replace r s l) ml0 subs in
  let ml0 =
    if has_module_type_of then ml0
    else (
      (* Substitute the [module type of] which are not supported yet. *)
      let subst s =
        let mname = Str.matched_group 2 s in
        let fincl = String.uncapitalize mname ^ ".mli" in
        try
          input_file fincl ~comments:false ~prefix:(Str.matched_group 1 s)
        with Sys_error _ ->
          failwith(sprintf "Trying to replace \"include module type of %s\" \
                            in %S but the file %S does not exist"
                           mname fname0 fincl) in
      Str.global_substitute module_type_of_re subst ml0
    ) in
  output_file fname1 ~content:ml0

(* [derived] is a list of (new_suffix, substitutions).  Returns the
   list of created files. *)
let derived_files ?(prefix=true) fnames suffix derived =
  let re = Str.regexp("\\([a-zA-Z]*\\)" ^ suffix ^ "$") in
  let derive l fname =
    if Str.string_match re fname 0 then (
      let seed = Str.matched_group 1 fname in
      if seed <> "lacaml" then (
        let derive1 l (new_suffix, subs) =
          let fname1 = seed ^ new_suffix in
          let fname1 = if prefix then "lacaml_" ^ fname1 else fname1 in
          substitute fname fname1 subs;
          fname1 :: l
        in
        List.fold_left derive1 l derived;
      ) else l
    ) else l in
  Array.fold_left derive [] fnames

let () =
  let fnames = Sys.readdir lib in
  let mods = ref [] in
  let derive ?(add=false) ?prefix suffix subs =
    let l = derived_files ?prefix fnames suffix subs in
    if add then mods := l :: !mods in
  let r subs = List.map (fun (r,s) -> (Str.regexp r, s)) subs in

  let float32 = r ["NPREC", "S";  "NBPREC", "S";
                   "Numberxx", "Float32";  "numberxx", "float32"]
  and float64 = r ["NPREC", "D"; "NBPREC", "D";
                   "Numberxx", "Float64";  "numberxx", "float64"]
  and complex32 = r ["NPREC", "C"; "NBPREC", "S";
                     "Numberxx", "Complex32";  "numberxx", "complex32"]
  and complex64 = r ["NPREC", "Z"; "NBPREC", "D";
                     "Numberxx", "Complex64"; "numberxx", "complex64"]
  in
  derive "_SDCZ.mli" [("4_S.mli", float32);   ("4_D.mli", float64);
                      ("4_C.mli", complex32); ("4_Z.mli", complex64) ];
  derive "_SDCZ.ml"  [("4_S.ml", float32);   ("4_D.ml", float64);
                      ("4_C.ml", complex32); ("4_Z.ml", complex64) ] ~add:true;

  let float32 = r["FPREC", "S";  "Floatxx", "Float32";  "floatxx", "float32"]
  and float64 = r["FPREC", "D";  "Floatxx", "Float64";  "floatxx", "float64"]
  and complex32 = r["CPREC", "C";  "CBPREC", "S";
                    "Floatxx", "Float32";      "floatxx", "float32";
                    "Complexxx", "Complex32";  "complexxx", "complex32"]
  and complex64 = r["CPREC", "Z";  "CBPREC", "D";
                    "Floatxx", "Float64";      "floatxx", "float64";
                    "Complexxx", "Complex64";  "complexxx", "complex64"]
  in
  derive "_SD.mli" [("2_S.mli", float32); ("2_D.mli", float64) ];
  derive "_SD.ml"  [("2_S.ml",  float32); ("2_D.ml", float64) ] ~add:true;
  derive "SD.ml"   [("s.ml", float32);     ("d.ml", float64)] ~add:true
         ~prefix:false;
  derive "SD.mli"  [("s.mli", float32);    ("d.mli", float64)] ~prefix:false;
  derive "_CZ.mli" [("2_C.mli", complex32); ("2_Z.mli", complex64)];
  derive "_CZ.ml"  [("2_C.ml",  complex32); ("2_Z.ml",  complex64)] ~add:true;
  derive "CZ.ml"   [("c.ml", complex32);  ("z.ml", complex64)] ~add:true
         ~prefix:false;
  derive "CZ.mli"  [("c.mli", complex32); ("z.mli", complex64)] ~prefix:false;

  (* Create lacaml.mlpack *)
  let fh = open_out (Filename.concat lib "lacaml.mlpack") in
  output_string fh "# Generated by the post-configure script \
                    make_prec_dep.ml\n";
  output_string fh "Common\n\
                    Utils\n\
                    Float32\n\
                    Float64\n\
                    Complex32\n\
                    Complex64\n\
                    Io\n\
                    Real_io\n\
                    Complex_io\n\
                    Version\n";
  List.iter (fun m ->
             let m = String.capitalize(Filename.chop_extension m) in
             output_string fh m;
             output_char fh '\n')
            (List.flatten !mods);
  close_out fh;
  (try Sys.remove (Filename.concat lib "lacaml.mllib") with _ -> ())


(* lacaml.mli
 ***********************************************************************)

(* Replace all [include module type of] to have a self contained
   interface that is easier to search. *)
let include_re =
  Str.regexp "^\\( *\\)include module type of +\\([A-Za-z0-9_]+_[SDCZ]\\|\
              Io\\|Common\\|[SDCZ]\\|Real_io\\|Complex_io\\)$"
let open_ba_re = Str.regexp " *open Bigarray *[\n\r\t]?"
let prec_re = Str.regexp " *open *\\(Float[0-9]+\\|Complex[0-9]+\\) *[\n\r\t]*"

let rec substitute_mli ?comments ?(prefix="") fname =
  let content = input_file ?comments ~prefix fname in
  Str.global_substitute include_re (subst ~prefix ~fname) content

and subst ~prefix ~fname s =
  let mname = Str.matched_group 2 s in
  let fincl = String.uncapitalize mname ^ ".mli" in
  let m =
    try substitute_mli fincl ~comments:false
                       ~prefix:(Str.matched_group 1 s)
    with Sys_error _ ->
      failwith(sprintf "Substituting \"include module type of %s\" in %S but \
                        the file %S does not exist" mname fname fincl) in
  (* "open Bigarray" already present in the main file *)
  let m = Str.global_replace open_ba_re "" m in
  Str.global_replace prec_re "" m

let () =
  output_file "lacaml.mli" ~content:(substitute_mli "lacaml_SDCZ.mli")

