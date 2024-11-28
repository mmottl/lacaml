(* File: lin_eq.ml

   Copyright © 2004-

   Markus Mottl email: markus.mottl@gmail.com WWW: http://www.ocaml.info

   Christophe Troestler email: Christophe.Troestler@umons.ac.be WWW:
   http://www.umh.ac.be/math/an/

   Oleg Trott email: ot14@columbia.edu WWW: http://www.columbia.edu/~ot14

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

open Format
open Bigarray
open Lacaml.D
open Lacaml.Io

let () =
  let n = 5 in

  let a = Mat.random ~from:(-500.) ~range:1000. n n in
  let b = Mat.make_mvec n 1. in

  printf "@[<2>General matrix A =@\n@\n@[%a@]@]@\n@\n" pp_fmat a;
  printf "@[<2>Right hand side:@\n@\n@[%a@]@]@\n@\n" pp_rfvec (Mat.col b 1);

  let a_copy = lacpy a in
  let b_copy = lacpy b in
  gesv a_copy b_copy;

  let sol = Mat.col b_copy 1 in
  printf "@[<2>Solving general matrix:@\n@\n";
  printf "Sol: X = @[%a@]@\n" pp_rfvec sol;
  printf "A X    = %a@]@\n@\n" pp_rfvec (gemv a sol);

  Array2.blit a a_copy;
  Array2.blit b b_copy;
  let kl = 1 in
  let ku = 2 in
  gbsv a_copy kl ku b_copy;
  let sol = Mat.col b_copy 1 in
  let a' = Mat.create (kl + ku + 1) n in
  (* [a'] is the real band matrix of the system, the first [kl] rows of [a] are
     ignored. *)
  for i = 1 to kl + ku + 1 do
    for j = 1 to n do
      a'.{i, j} <- a.{kl + i, j}
    done
  done;
  printf "@[<2>Matrix A with band (kl = 1, ku = 2):@\n@\n";
  printf "Sol:   X = @[%a@]@\n" pp_rfvec sol;
  printf "     A X = %a@]@\n@\n" pp_rfvec (gbmv a' kl ku sol);

  for i = 1 to n do
    a.{1, i} <- 1.;
    (* 1 upper diag => kd = 1 *)
    a.{2, i} <- 4.
    (* main diag *)
    (* the other rows of [a] are irrelevant *)
  done;
  Array2.blit a a_copy;
  Array2.blit b b_copy;
  let kd = 1 in
  pbsv a_copy ~kd b_copy;
  let sol = Mat.col b_copy 1 in
  printf "@[<2>Symmetric positive definite matrix (not displayed):@\n@\n";
  printf "Sol:   X = @[%a@]@\n" pp_rfvec sol;
  printf "     A X = %a@]@\n" pp_rfvec (sbmv a ~k:kd sol)
