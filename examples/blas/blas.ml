(* File: blas.ml

   Copyright (C) 2004-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

(* $Id: blas.ml,v 1.12 2006/01/22 18:34:33 mottl Exp $ *)

open Format
open Bigarray

open Lacaml.Impl.D
open Lacaml.Io

let () =
  let x = Vec.of_array [| 1.; 2.; 4.; 2. |] in
  let y = Vec.of_array [| 0.; 3.; 7.; 2.; 3. |] in
  let alpha = 3.0 in

  printf "x = @[%a@]@\n@\n" pp_rfvec x;
  printf "norm_2(x)  = %g@\n" (nrm2 x);
  printf "norm_1(x)  = %g@\n" (asum x);
  printf "norm_oo(x) = %g  (element %i)@\n@\n" (amax x) (iamax x);

  printf "y = @[%a@]@\n@\n" pp_rfvec y;

  printf "y <- y + %g * x@\n@\n" alpha;
  axpy ~alpha ~x y;
  printf "y = @[%a@]@\n@\n" pp_rfvec y;

  printf "x <- %g * x@\n@\n" alpha;
  scal alpha x;
  printf "x = @[%a@]@\n@\n" pp_rfvec x;

  printf "y <- x@\n@\n";
  printf "y = @[%a@]@\n@\n" pp_rfvec (copy ~y x);

  let a =
    Mat.of_array
      [|
        [| 2.; 3. |];
        [| 1.; -5. |];
      |] in
  let b =
    Mat.of_array
      [|
        [| 4.; 3.; 6. |];
        [| 1.; -2.; 3. |];
      |] in

  let c = gemm ~transa:`T a b in

  printf "a = @[%a@]@\n@\n" pp_fmat a;
  printf "b = @[%a@]@\n@\n" pp_fmat b;
  printf "c = a'*b = @[%a@]@\n" pp_fmat c
