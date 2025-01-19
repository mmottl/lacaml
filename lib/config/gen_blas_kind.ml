external zdot_is_procedure : unit -> bool = "lacaml_zdot_is_procedure_stub"
external zdot_is_function : unit -> bool = "lacaml_zdot_is_function_stub"

let () =
  if Array.length Sys.argv = 1 then
    let module C = Configurator.V1 in
    let blas_kind_flags =
      if Sys.command "./gen_blas_kind.exe procedure" = 0 then
        [ "-DZDOT_IS_PROCEDURE" ]
      else if Sys.command "./gen_blas_kind.exe function" = 0 then
        [ "-DZDOT_IS_FUNCTION" ]
      else failwith "Could not determine correct zdot calling convention"
    in
    C.Flags.write_sexp "blas_kind_flags.sexp" blas_kind_flags
  else
    let test =
      match Sys.argv.(1) with
      | "procedure" -> zdot_is_procedure
      | "function" -> zdot_is_function
      | _ -> assert false
    in
    exit (if test () then 0 else 1)
