(* File: nag_pbsv.ml

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

(* Example from http://www.nag.com/lapack-ex/node9.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Symmetric positive definite band matrix. Only the upper triangle of the
   matrix is stored (default behavior of [pbsv]). Each line correspond to a
   diagonal, the columns being the same as the original matrix. *)
let ab =
  Mat.of_array [| [| nan; 2.68; -2.39; -2.22 |]; [| 5.49; 5.63; 2.60; 5.17 |] |]

let b = Vec.of_array [| 22.09; 9.31; -5.24; 11.83 |]

let () =
  let x = copy b in
  pbsv ab (Mat.from_col_vec x);
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;
  printf "Cholesky factor U (each line is a diagonal):@\n  @[%a@]@\n" pp_fmat ab
