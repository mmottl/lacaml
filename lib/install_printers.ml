let printers =
  [
    "Lacaml.Io.pp_top_rfvec";
    "Lacaml.Io.pp_top_rcvec";
    "Lacaml.Io.pp_top_rivec";
    "Lacaml.Io.pp_top_fmat";
    "Lacaml.Io.pp_top_cmat";
    "Lacaml.Io.pp_top_imat";
  ]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let set_contexts () =
  let cmd =
    "Lacaml.Io.Context.set_dim_defaults (Some (Lacaml.Io.Context.create 3));;"
  in
  eval_string cmd

let () =
  if not (install_printers printers && set_contexts ()) then
    Format.eprintf "Problem installing LACAML-printers@."
