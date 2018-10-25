(* File: nag_spsv.ml

   Copyright (C) 2013-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://www.umh.ac.be/math/an/

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

(* Example from http://www.nag.com/lapack-ex/node17.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Symmetric matrix stored in packed format.  Only the upper triangle
   is used by default by [spsv]. *)
let ap = Vec.of_array [| -1.81;                     (* col 1 *)
                          2.06;  1.15;              (* col 2 *)
                          0.63;  1.87; -0.21;       (* col 3 *)
                         -1.15;  4.20;  3.87;  2.07 (* col 4 *) |]

let b = Vec.of_array [| 0.96;  6.07;  8.38;  9.50 |]


let () =
  let x = copy b in
  let n = truncate(sqrt(float(2 * Vec.dim ap))) in
  let ipiv = Lacaml.Common.create_int32_vec n in
  spsv ap (Mat.from_col_vec x) ~ipiv;  (* [ipiv] is optional *)
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;

  (* Store [ap] in a full matrix for display. *)
  let a = Mat.make n n nan in
  for j = 1 to n do
    for i = 1 to j do
      a.{i,j} <- ap.{i + ((j-1) * j) / 2}
    done;
  done;
  (* Print the matrix but not the NaN. *)
  pp_float_el_default := (fun fm x -> if (x: float) = x then fprintf fm "%G" x);
  printf "Details of the factorization: @[%a@]@\n" pp_fmat a;
  printf "Pivot indices: @[%a@]@\n" pp_rivec ipiv
