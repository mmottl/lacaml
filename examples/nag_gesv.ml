(* File: nag_gesv.ml

   Copyright © 2013-

   Christophe Troestler email: Christophe.Troestler@umons.ac.be WWW:
   http://www.umh.ac.be/math/an/

   This library is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA *)

(* Example from http://www.nag.com/lapack-ex/node5.html *)
open Format
open Lacaml.D
open Lacaml.Io

let a =
  Mat.of_array
    [|
      [| 1.80; 2.88; 2.05; -0.89 |];
      [| 5.25; -2.95; -0.95; -3.80 |];
      [| 1.58; -2.69; -2.90; -1.04 |];
      [| -1.11; -0.66; -0.59; 0.80 |];
    |]

let b = Vec.of_array [| 9.52; 24.35; 0.77; -6.22 |]

let () =
  (* Solves [a * x = b]. Solution in [x] which must initially hold [b]. *)
  let x = copy b in
  let ipiv = Lacaml.Common.create_int32_vec (Mat.dim1 a) in
  gesv a (Mat.from_col_vec x) ~ipiv;
  (* Print solution and details of factorisation. *)
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;
  printf "Details of factorization: @[%a@]@\n" pp_fmat a;
  printf "Pivot indices: @[%a@]@\n" pp_rivec ipiv
