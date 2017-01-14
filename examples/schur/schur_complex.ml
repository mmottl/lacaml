(* File: schur.ml

   Copyright (C) 2015-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Florent Hoareau
     email: h.florent@gmail.com
     WWW: none

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Format
open Lacaml.Io

let () = Random.self_init ()

(* GEES DEMO *)

let () =
  let open Lacaml.Z in
  let n = 5 in

  let a =
    Mat.random
      ~re_from:(-500.) ~re_range:1000.
      ~im_from:(-500.) ~im_range:1000. n n
  in
  let a_res = lacpy a in

  printf "@[<2>Complex matrix A =@\n@\n%a@]@\n@." pp_cmat a;

  let select_function z = Complex.(z.re < 0.) in
  let sdim, w, vs = gees ~sort:(`Select_custom select_function) a_res in

  printf "@[<2>Schur form of matrix A =@\n@\n%a@]@\n@\n" pp_cmat a_res;
  printf "@[<2>Transformation matrix U =@\n@\n%a@]@\n@." pp_cmat vs;
  printf "@[<2>Eigenvalues (%d selected):@\n" sdim;

  for i = 1 to Vec.dim w do
    let { Complex.re; im } = w.{i} in
    printf "@\n%f + %f*i" re im
  done;

  printf "@]@."
