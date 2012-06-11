(* File: lin_eq_comp.ml

   Copyright (C) 2004-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://www.umh.ac.be/math/an/

     Oleg Trott
     email: ot14@columbia.edu
     WWW: http://www.columbia.edu/~ot14

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

(* $Id: lin_eq_comp.ml,v 1.9 2006/01/22 18:34:33 mottl Exp $ *)

open Format

open Lacaml.D
open Lacaml.Io

let () = Random.self_init ()

let pp_space ppf = pp_print_string ppf " "

let () =
  let ar =
    [|
      [| 1.; 2.; 2.; |];
      [| 4.; 4.; 2.; |];
      [| 5.; 6.; 4.; |];
    |] in
  let a = Mat.of_array ar in

  printf "@[<2>General matrix A = @[%a@]@]@\n@\n" pp_fmat a;

  let lu = lacpy a in
  let ipiv = getrf lu in
  printf "lu(A) = @[%a@]@\n@\n" pp_fmat lu;

  printf "ipiv = @[%a@]@\n@\n" pp_rivec ipiv;

  let inv = lacpy lu in
  let _ipiv = getri inv in
  printf "inv(A) = @[%a@]@\n@\n" pp_fmat inv;
  let aainv = gemm a inv in
  printf "A*inv(A) = @[%a@]@\n@\n" pp_fmat aainv;

  let normM = lange ~norm:`M a in
  let normO = lange ~norm:`O a in
  let normI = lange ~norm:`I a in
  let normF = lange ~norm:`F a in

  printf "norm(A,M) = %g@\n" normM;
  printf "norm(A,O) = %g@\n" normO;
  printf "norm(A,I) = %g@\n" normI;
  printf "norm(A,F) = %g@\n" normF;

  let rcondO = gecon ~norm:`O ~anorm:normO lu in
  let rcondI = gecon ~norm:`I ~anorm:normI lu in

  printf "rcond(A,O) = %g@\n" rcondO;
  printf "rcond(A,I) = %g@\n@\n" rcondI;


  let b_ar =
    [|
      [| 21.; 32.; 41.; |];
      [| 32.; 54.; 71.; |];
      [| 41.; 71.; 94.; |];
    |] in

  let b = Mat.of_array b_ar in

  printf "@[<2>Symmetric matrix B = @[%a@]@]@\n@\n" pp_fmat b;

  let chol = lacpy b in
  potrf chol;
  printf "chol(B) = @[%a@]@\n@\n" pp_fmat chol;

  let inv = lacpy chol in
  potri ~factorize:false inv;
  printf "inv(B) = @[%a@]@\n@\n" pp_fmat inv;

  let normM = lange ~norm:`M b in
  let normO = lange ~norm:`O b in
  let normI = lange ~norm:`I b in
  let normF = lange ~norm:`F b in

  printf "norm(A,M) = %g@\n" normM;
  printf "norm(A,O) = %g@\n" normO;
  printf "norm(A,I) = %g@\n" normI;
  printf "norm(A,F) = %g@\n" normF;

  let rcondO = pocon ~anorm:normO chol in

  printf "rcond(B,O) = %g@\n" rcondO
