let () =
  let module C = Configurator.V1 in
  let blas_kind_flags =
    if Discover_utils.Detect_blas_kind.zdot_is_procedure ()
    then ["-DZDOT_IS_PROCEDURE"]
    else ["-DZDOT_IS_FUNCTION"]
  in
  C.Flags.write_sexp "blas_kind_flags.sexp" blas_kind_flags
