(* File: nag_ptsv.ml

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

(* Example from http://www.nag.com/lapack-ex/node15.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Symmetric positive definite tridiagonal matrix. [d] is the diagonal and [e]
   the sub-diagonal. *)
let d = Vec.of_array [| 4.0; 10.0; 29.0; 25.0; 5.0 |]
let e = Vec.of_array [| -2.0; -6.0; 15.0; 8.0 |]
let b = Vec.of_array [| 6.0; 9.0; 2.0; 14.0; 7.0 |]

let () =
  let x = copy b in
  ptsv d e (Mat.from_col_vec x);
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;
  printf "Diagonal elements of the diagonal matrix D: @[%a@]@\n" pp_rfvec d;
  printf "Sub-diagonal elements of the Cholesky factor L: @[%a@]@\n" pp_rfvec e
