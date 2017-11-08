open Base
open Stdio

let split_ws str = String.(split str ~on:' ' |> List.filter ~f:((<>) ""))

let () =
  let module C = Configurator in
  C.main ~name:"lacaml" (fun c ->
    let cflags =
      match Caml.Sys.getenv "LACAML_CFLAGS" with
      | alt_cflags -> split_ws alt_cflags
      | exception Not_found -> []
    in
    let libs, libs_override =
      match Caml.Sys.getenv "LACAML_LIBS" with
      | alt_libs -> split_ws alt_libs, true
      | exception Not_found -> ["-lblas"; "-llapack"], false
    in
    let conf =
      (* [exp10] is a GNU compiler extension so we have to provide our own
         external implementation by default unless we know that our platform is
         using the GNU compiler. *)
      let default : C.Pkg_config.package_conf =
        { cflags = "-DEXTERNAL_EXP10" :: "-std=c99" :: cflags; libs } in
      Option.value_map (C.ocaml_config_var c "system") ~default ~f:(function
        | "linux" | "linux_elf" -> { cflags = "-std=gnu99" :: cflags; libs }
        | "macosx" when not libs_override ->
            { default with libs = "-framework" :: "Accelerate" :: libs }
        | "mingw64" -> { cflags = "-DWIN32" :: default.cflags; libs }
        | _ -> default)
    in
    let write_sexp file sexp =
      Out_channel.write_all file ~data:(Sexp.to_string sexp)
    in
    write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
    write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs))
