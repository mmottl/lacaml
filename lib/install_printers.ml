let printers =
  [
    "Lacaml.Io.Toplevel.pp_rfvec";
    "Lacaml.Io.Toplevel.pp_rcvec";
    "Lacaml.Io.Toplevel.pp_rivec";
    "Lacaml.Io.Toplevel.pp_fmat";
    "Lacaml.Io.Toplevel.pp_cmat";
    "Lacaml.Io.Toplevel.pp_imat";
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

let set_defaults () =
  let cmd =
    "let lsc = \
      Lacaml.Io.Context.set_dim_defaults (Some (Lacaml.Io.Context.create 3)); \
      Lacaml.Io.Toplevel.lsc;;"
  in
  eval_string cmd

let () =
  if not (install_printers printers && set_defaults ()) then
    Format.eprintf "Problem installing LACAML-printers@."
