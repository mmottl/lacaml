(* File: nag_gtsv.ml

   Copyright (C) 2013-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://www.umh.ac.be/math/an/

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

(* Example from http://www.nag.com/lapack-ex/node7.html *)
open Format
open Lacaml.D
open Lacaml.Io

(* Tridiagonal matrix. Upper, main, and lower diagonals. *)
let du = Vec.of_array [|        2.1;  -1.0;   1.9;   8.0 |]
let d  = Vec.of_array [| 3.0;   2.3;  -5.0;  -0.9;   7.1 |]
let dl = Vec.of_array [| 3.4;   3.6;   7.0;  -6.0        |]

let b= Vec.of_array [| 2.7; -0.5;  2.6;  0.6;  2.7 |]

let () =
  let x = copy b in
  gtsv dl d du (Mat.from_col_vec x);
  printf "Solution: X = @[%a@]@\n" pp_rfvec x;

