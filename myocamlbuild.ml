(* OASIS_START *)
(* OASIS_STOP *)

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
        flag ["compile"; "ocaml"] (S [A "-strict-sequence"]);

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
  dispatch (
    MyOCamlbuildBase.dispatch_combine
      [MyOCamlbuildBase.dispatch_default package_default; additional_rules])
