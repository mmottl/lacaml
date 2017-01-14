(* File: nag_gbsv.ml

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

(* Example from http://www.nag.com/lapack-ex/node3.html *)
open Format
open Lacaml.D
open Lacaml.Io

let a = Mat.of_array [| [| -0.23;   2.54;  -3.66;   0.   |];
                        [| -6.98;   2.46;  -2.73;  -2.13 |];
                        [|  0.;     2.56;   2.46;   4.07 |];
                        [|  0.;     0.;    -4.78;  -3.82 |] |]
let kl = 1 and ku = 2

let b = Vec.of_array [| 4.42;  27.13;  -6.14;  10.50 |]

let () =
  (* The matrix [ab] must possess enough rows to hols its factorization. *)
  let ab = Mat.create (2 * kl + ku + 1) (Mat.dim2 a) in
  (* The initial matrix [a] must be stored in rows [lk+1] to
     [2*kl+ku+1], in band storage. *)
  let n = Mat.dim2 a in
  for j = 1 to n do
    for i = max 1 (j - ku) to min n (j + kl) do
      ab.{kl+ku+1+i-j, j} <- a.{i,j}
    done;
  done;
  (* Solve [a * x = b]. Solution in [x] (which must hold the RHS initially). *)
  let x = copy b in
  gbsv ab kl ku (Mat.from_col_vec x);
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;
