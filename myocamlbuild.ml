(* OASIS_START *)
(* DO NOT EDIT (digest: 8fd8da14601100561d745f586bc6d700) *)
module OASISGettext = struct
(* # 21 "/Users/mmottl/local/darwin11.3.0/src/oasis-0.3.0/src/oasis/OASISGettext.ml" *)

  let ns_ str =
    str

  let s_ str =
    str

  let f_ (str : ('a, 'b, 'c, 'd) format4) =
    str

  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""

  let init =
    []

end

module OASISExpr = struct
(* # 21 "/Users/mmottl/local/darwin11.3.0/src/oasis-0.3.0/src/oasis/OASISExpr.ml" *)



  open OASISGettext

  type test = string 

  type flag = string 

  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string
    

  type 'a choices = (t * 'a) list 

  let eval var_get t =
    let rec eval' =
      function
        | EBool b ->
            b

        | ENot e ->
            not (eval' e)

        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)

        | EOr (e1, e2) ->
            (eval' e1) || (eval' e2)

        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")

        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t

  let choose ?printer ?name var_get lst =
    let rec choose_aux =
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then
              vl
            else
              choose_aux tl
        | [] ->
            let str_lst =
              if lst = [] then
                s_ "<empty>"
              else
                String.concat
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with
                | Some nm ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)

end


# 117 "myocamlbuild.ml"
module BaseEnvLight = struct
(* # 21 "/Users/mmottl/local/darwin11.3.0/src/oasis-0.3.0/src/base/BaseEnvLight.ml" *)

  module MapString = Map.Make(String)

  type t = string MapString.t

  let default_filename =
    Filename.concat
      (Sys.getcwd ())
      "setup.data"

  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line =
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer =
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer;
                Stream.junk lexer;
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith
          (Printf.sprintf
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end

  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute
          buff
          (fun var ->
             try
               var_expand (MapString.find var env)
             with Not_found ->
               failwith
                 (Printf.sprintf
                    "No variable %s defined when trying to expand %S."
                    var
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)

  let var_choose lst env =
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


# 215 "myocamlbuild.ml"
module MyOCamlbuildFindlib = struct
(* # 21 "/Users/mmottl/local/darwin11.3.0/src/oasis-0.3.0/src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml" *)

  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin

  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read

  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings

  let split s ch =
    let x = 
      ref [] 
    in
    let rec go s =
      let pos = 
        String.index s ch 
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x

  let split_nl s = split s '\n'

  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s

  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());

          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

      | _ -> 
          ()

end

module MyOCamlbuildBase = struct
(* # 21 "/Users/mmottl/local/darwin11.3.0/src/oasis-0.3.0/src/plugins/ocamlbuild/MyOCamlbuildBase.ml" *)

  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)



  open Ocamlbuild_plugin
  module OC = Ocamlbuild_pack.Ocaml_compiler

  type dir = string 
  type file = string 
  type name = string 
  type tag = string 

(* # 56 "/Users/mmottl/local/darwin11.3.0/src/oasis-0.3.0/src/plugins/ocamlbuild/MyOCamlbuildBase.ml" *)

  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir * file list) list; 
        flags:     (tag list * (spec OASISExpr.choices)) list;
        (* Replace the 'dir: include' from _tags by a precise interdepends in
         * directory.
         *)
        includes:  (dir * dir list) list; 
      } 

  let env_filename =
    Pathname.basename 
      BaseEnvLight.default_filename

  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 

  let tag_libstubs nm =
    "use_lib"^nm^"_stubs"

  let nm_libstubs nm =
    nm^"_stubs"

  let dispatch t e = 
    let env = 
      BaseEnvLight.load 
        ~filename:env_filename 
        ~allow_empty:true
        ()
    in
      match e with 
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try 
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]

        | After_rules -> 
            (* Declare OCaml libraries *)
            List.iter 
              (function
                 | nm, [] ->
                     ocaml_lib nm
                 | nm, dir :: tl ->
                     ocaml_lib ~dir:dir (dir^"/"^nm);
                     List.iter 
                       (fun dir -> 
                          List.iter
                            (fun str ->
                               flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                            ["compile"; "infer_interface"; "doc"])
                       tl)
              t.lib_ocaml;

            (* Declare directories dependencies, replace "include" in _tags. *)
            List.iter 
              (fun (dir, include_dirs) ->
                 Pathname.define_context dir include_dirs)
              t.includes;

            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("-l"^(nm_libstubs lib)); A"-cclib";
                        A("-l"^(nm_libstubs lib))]);

                   flag ["link"; "library"; "ocaml"; "native"; tag_libstubs lib]
                     (S[A"-cclib"; A("-l"^(nm_libstubs lib))]);
                        
                   flag ["link"; "program"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("dll"^(nm_libstubs lib))]);

                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep ["link"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   dep  ["compile"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"] 
                     headers;

                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib] 
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;

              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec = 
                   BaseEnvLight.var_choose cond_specs env
                 in
                   flag tags & spec)
              t.flags
        | _ -> 
            ()

  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]

end


# 476 "myocamlbuild.ml"
open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml =
       [("lacaml", ["lib"]); ("lacaml_top", ["lib"])];
     lib_c =
       [
          ("lacaml",
            "lib",
            ["lib/f2c.h"; "lib/lacaml_macros.h"; "lib/utils_c.h"])
       ];
     flags =
       [
          (["oasis_library_lacaml_ccopt"; "compile"],
            [
               (OASISExpr.EBool true,
                 S [A "-ccopt"; A "-O2"; A "-ccopt"; A "-DPIC"]);
               (OASISExpr.ETest ("system", "mingw"),
                 S
                   [
                      A "-ccopt";
                      A "-O2";
                      A "-ccopt";
                      A "-DPIC";
                      A "-ccopt";
                      A "-DEXTERNAL_EXP10"
                   ]);
               (OASISExpr.ETest ("system", "macosx"),
                 S
                   [
                      A "-ccopt";
                      A "-O2";
                      A "-ccopt";
                      A "-DPIC";
                      A "-ccopt";
                      A "-DEXTERNAL_EXP10";
                      A "-ccopt";
                      A "-framework";
                      A "-ccopt";
                      A "vecLib"
                   ]);
               (OASISExpr.EAnd
                  (OASISExpr.ETest ("system", "macosx"),
                    OASISExpr.ETest ("system", "mingw")),
                 S
                   [
                      A "-ccopt";
                      A "-O2";
                      A "-ccopt";
                      A "-DPIC";
                      A "-ccopt";
                      A "-DEXTERNAL_EXP10";
                      A "-ccopt";
                      A "-framework";
                      A "-ccopt";
                      A "vecLib";
                      A "-ccopt";
                      A "-DEXTERNAL_EXP10"
                   ])
            ]);
          (["oasis_library_lacaml_cclib"; "link"],
            [
               (OASISExpr.EBool true,
                 S [A "-cclib"; A "-lblas"; A "-cclib"; A "-llapack"]);
               (OASISExpr.ETest ("system", "macosx"),
                 S [A "-cclib"; A "-framework"; A "-cclib"; A "vecLib"])
            ]);
          (["oasis_library_lacaml_cclib"; "ocamlmklib"; "c"],
            [
               (OASISExpr.EBool true, S [A "-lblas"; A "-llapack"]);
               (OASISExpr.ETest ("system", "macosx"),
                 S [A "-framework"; A "vecLib"])
            ])
       ];
     includes =
       [
          ("examples/svd", ["lib"]);
          ("examples/qr", ["lib"]);
          ("examples/lin_reg", ["lib"]);
          ("examples/lin_eq_comp", ["lib"]);
          ("examples/lin_eq", ["lib"]);
          ("examples/eig", ["lib"]);
          ("examples/blas", ["lib"])
       ];
     }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

# 567 "myocamlbuild.ml"
(* OASIS_STOP *)
# 502 "myocamlbuild.ml"

let rec split_on is_delim s i0 i i1 =
  if i >= i1 then [String.sub s i0 (i1 - i0)]
  else if is_delim s.[i] then
    String.sub s i0 (i - i0) :: skip is_delim s (i + 1) i1
  else
    split_on is_delim s i0 (i + 1) i1
and skip is_delim s i i1 =
  if i >= i1 then []
  else if is_delim s.[i] then skip is_delim s (i + 1) i1
  else split_on is_delim s i (i + 1) i1

let split_on_spaces s = skip (fun c -> c = ' ') s 0 (String.length s)
let split_on_tabs s = skip (fun c -> c = '\t') s 0 (String.length s)

let env = BaseEnvLight.load() (* setup.data *)
let ocamlfind = BaseEnvLight.var_get "ocamlfind" env
let stdlib = BaseEnvLight.var_get "standard_library" env

let a l = List.map (fun s -> A s) l
let conf_ccopt = a(split_on_tabs(BaseEnvLight.var_get "conf_ccopt" env))
let conf_cclib = a(split_on_tabs(BaseEnvLight.var_get "conf_cclib" env))

let replace1 want_tags spec ((tags, specs) as ts) =
  let all_tags = List.fold_left (fun a t -> a && List.mem t tags) true want_tags in
  if all_tags then (tags, [(OASISExpr.EBool true, S spec)])
  else ts

let replace tags spec l = List.map (replace1 tags spec) l

let prefix_each p l =
  List.fold_right (fun a l' -> p :: a :: l') l []

let package_default =
  (* Act on conf.ml *)
  let flags = package_default.MyOCamlbuildBase.flags in
  let flags = match conf_ccopt with
    | [] -> flags
    | _ -> replace ["oasis_library_lacaml_ccopt"]
                  (prefix_each (A "-ccopt") conf_ccopt) flags in
  let flags = match conf_cclib with
    | [] -> flags
    | _ ->
      let flags = replace ["oasis_library_lacaml_cclib"; "ocamlmklib"]
                          conf_cclib flags in
      replace ["oasis_library_lacaml_cclib"; "link"]
              (prefix_each (A "-cclib") conf_cclib) flags in
  { package_default with MyOCamlbuildBase.flags = flags }

let () =
  let additional_rules = function
    | After_rules ->
        flag ["compile"; "ocaml"] (S [A "-strict-sequence" ]);

        pflag ["compile"; "ocaml"] "I" (fun x -> S [A "-I"; A x]);

        (* Files included, tailored with macros. *)
        dep ["compile"; "c"]
            ["lib"/"fold_col.c"; "lib"/"fold2_col.c";
            "lib"/"vec_map.c"; "lib"/"vec_combine.c"; "lib"/"vec_sort.c"];

        (* Special rules for precision dependent C code. *)
        let lacaml_cc desc ~prod ~dep flags =
          rule ("Lacaml: " ^ desc) ~prod ~dep
              (fun env _build ->
                let f = env dep and o = env prod in
                let tags = tags_of_pathname f ++ "compile" ++ "c"
                          ++ "oasis_library_lacaml_ccopt" in

                let add_ccopt f l = A"-ccopt" :: f :: l in
                let flags = List.fold_right add_ccopt flags [] in
                (* unfortunately -o is not respected for C files, use -ccopt. *)
                let cmd = [A ocamlfind; A"ocamlc"; A"-ccopt"; A("-o " ^ o)]
                          @ flags @ [T tags; A"-c"; P f] in
                Seq[Cmd(S(cmd))]
              ) in
        lacaml_cc "simple of SD" ~prod:"%2_S_c.o" ~dep:"%_SD_c.c" [];
        lacaml_cc "double of SD" ~prod:"%2_D_c.o" ~dep:"%_SD_c.c"
                  [A"-DLACAML_DOUBLE"];
        lacaml_cc "simple of CZ" ~prod:"%2_C_c.o" ~dep:"%_CZ_c.c"
                  [A"-DLACAML_COMPLEX"];
        lacaml_cc "double of CZ" ~prod:"%2_Z_c.o" ~dep:"%_CZ_c.c"
                  [A"-DLACAML_COMPLEX"; A"-DLACAML_DOUBLE"];

        lacaml_cc "simple of SDCZ" ~prod:"%4_S_c.o" ~dep:"%_SDCZ_c.c" [];
        lacaml_cc "double of SDCZ" ~prod:"%4_D_c.o" ~dep:"%_SDCZ_c.c"
                  [A"-DLACAML_DOUBLE"];
        lacaml_cc "complex32 of SDCZ" ~prod:"%4_C_c.o" ~dep:"%_SDCZ_c.c"
                  [A"-DLACAML_COMPLEX"];
        lacaml_cc "complex64 of SDCZ" ~prod:"%4_Z_c.o" ~dep:"%_SDCZ_c.c"
                  [A"-DLACAML_COMPLEX"; A"-DLACAML_DOUBLE"];

    | _ -> ()
  in
  dispatch
    (MyOCamlbuildBase.dispatch_combine [
         MyOCamlbuildBase.dispatch_default package_default;
         additional_rules ])
