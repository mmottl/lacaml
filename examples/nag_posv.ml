(* File: nag_posv.ml

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

(* Example from http://www.nag.com/lapack-ex/node11.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Symmetric positive definite matrix.  By default, [posv] only uses
   the upper triangular part of the matrix. *)
let a = Mat.of_array [| [| 4.16;  -3.12;   0.56;  -0.10 |];
                        [|  nan;   5.03;  -0.83;   1.18 |];
                        [|  nan;    nan;   0.76;   0.34 |];
                        [|  nan;    nan;    nan;   1.18 |] |]

let b = Vec.of_array [| 8.70; -13.35;  1.89; -4.14 |]

let () =
  let x = copy b in
  posv a (Mat.from_col_vec x);
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;
  printf "Cholesky factor U: @[%a@]@\n" pp_fmat a
