(* File: lacaml.ml

   Copyright (C) 2010-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/an/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

#load "str.cma"

(** Generate the lacaml.mli file from the various interfaces. *)
let file_out_mli = Sys.argv.(1)
let file_in_mli = Sys.argv.(2)

let comment_re = Str.regexp "(\\* [^*]+\\*)[ \n\r\t]*"

let input_file ?(comments=true) fname =
  let fh = open_in fname in
  let len = in_channel_length fh in
  let buf = String.create len in
  really_input fh buf 0 len;
  close_in fh;
  if comments then buf
  else Str.global_replace comment_re "" buf


let mli = input_file file_in_mli

let include_re =
  Str.regexp "^ *include +\\([A-Za-z0-9]+_[SDCZ]\\|Io\\|Common\\)"
let open_ba_re = Str.regexp "open Bigarray"
let prec_re = Str.regexp " *open *\\(Float[0-9]+\\|Complex[0-9]+\\)[ \n\r\t]*"

let mli =
  let subst s =
    let mname = Str.matched_group 1 s in
    let m = input_file (String.uncapitalize mname ^ ".mli") ~comments:false in
    (* "open Bigarray" already present in the main file *)
    let m = Str.global_replace open_ba_re "" m in
    Str.global_replace prec_re "" m in
  Str.global_substitute include_re subst mli


(* Output the resulting interface *)
let () =
  let fh = open_out file_out_mli in
  output_string fh mli;
  close_out fh
