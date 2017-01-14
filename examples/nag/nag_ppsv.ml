(* File: nag_ppsv.ml

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

(* Example from http://www.nag.com/lapack-ex/node13.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Symmetric positive definite matrix stored in packed format.  Only
   the upper triangle is stored (default for [ppsv]). *)
let ap = Vec.of_array [|  4.16;                       (* col 1 *)
                         -3.12;   5.03;               (* col 2 *)
                          0.56;  -0.83;   0.76;       (* col 3 *)
                         -0.10;   1.18;   0.34;  1.18 (* col 4 *) |]

let b = Vec.of_array [| 8.70; -13.35;  1.89; -4.14 |]


let () =
  let x = copy b in
  ppsv ap (Mat.from_col_vec x);
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;
  (* Store [ap] in a full matrix for display. *)
  let n = truncate(sqrt(float(2 * Vec.dim ap))) in
  let a = Mat.make n n nan in
  for j = 1 to n do
    for i = 1 to j do
      a.{i,j} <- ap.{i + ((j-1) * j) / 2}
    done;
  done;
  (* Print the matrix but not the NaN. *)
  pp_float_el_default := (fun fm x -> if (x: float) = x then fprintf fm "%G" x);
  printf "Cholesky factor U: @[%a@]@\n" pp_fmat a
