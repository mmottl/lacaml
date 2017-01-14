(* File: nag_sysv.ml

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

(* Example from http://www.nag.com/lapack-ex/node19.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Symmetric matrix.  [sysv] only reference the upper triangle by default. *)
let a = Mat.of_array [| [| -1.81;  2.06;  0.63; -1.15 |];
                        [|   nan;  1.15;  1.87;  4.20 |];
                        [|   nan;   nan; -0.21;  3.87 |];
                        [|   nan;   nan;   nan;  2.07 |] |]

let b = Vec.of_array [| 0.96;  6.07;  8.38;  9.50 |]

let () =
  let x = copy b in
  let ipiv = Lacaml.Common.create_int32_vec (Mat.dim1 a) in
  sysv a (Mat.from_col_vec x) ~ipiv;    (* [ipiv] is optional *)
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;

  (* Print the matrix but not the NaN entries. *)
  pp_float_el_default := (fun fm x -> if (x: float) = x then fprintf fm "%G" x);
  printf "Details of the factorization: @[%a@]@\n" pp_fmat a;
  printf "Pivot indices: @[%a@]@\n" pp_rivec ipiv
