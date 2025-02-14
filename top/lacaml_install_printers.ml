let printers =
  [
    "Lacaml.Io.Toplevel.pp_rfvec";
    "Lacaml.Io.Toplevel.pp_rcvec";
    "Lacaml.Io.Toplevel.pp_rivec";
    "Lacaml.Io.Toplevel.pp_fmat";
    "Lacaml.Io.Toplevel.pp_cmat";
    "Lacaml.Io.Toplevel.pp_imat";
  ]

let eval_string ?(print_outcome = false) ?(err_formatter = Format.err_formatter)
    str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let () =
  if not (install_printers printers) then
    Format.eprintf "Problem installing LACAML-printers@."
