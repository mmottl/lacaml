open Base
open Stdio

let split_ws str = List.filter (String.split ~on:' ' str) ~f:(String.(<>) "")

let () =
  let module C = Configurator in
  C.main ~name:"lacaml" (fun c ->
    let cflags =
      let cflags =
        match Caml.Sys.getenv "LACAML_CFLAGS" with
        | alt_cflags -> split_ws alt_cflags
        | exception Not_found -> []
      in
      "-std=c99" :: cflags
    in
    let libs_override = ref false in
    let libs =
      match Caml.Sys.getenv "LACAML_LIBS" with
      | alt_libs -> libs_override := true; split_ws alt_libs
      | exception Not_found -> ["-lblas"; "-llapack"]
    in
    let conf =
      let default : C.Pkg_config.package_conf = { cflags; libs } in
      let var = C.ocaml_config_var c "system" in
      let ext_exp10 = "-DEXTERNAL_EXP10" in
      Option.value_map var ~default ~f:(function
        | "linux" | "linux_elf" ->
            let cflags = "-std=gnu99" :: cflags in
            { cflags; libs }
        | "macosx" ->
            let cflags = ext_exp10 :: cflags in
            let libs =
              if !libs_override then libs
              else ["-framework"; "Accelerate"]
            in
            { cflags; libs }
        | "freebsd" ->
            let cflags = ext_exp10 :: cflags in
            { cflags; libs }
        | "mingw64" ->
            let cflags = "-DWIN32" :: ext_exp10 :: cflags in
            { cflags; libs }
        | _ -> default)
    in
    let write_sexp file sexp =
      Out_channel.write_all file ~data:(Sexp.to_string sexp)
    in
    write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
    write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs))
