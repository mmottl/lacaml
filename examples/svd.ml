(* File: svd.ml

   Copyright © 2004-2005

   Egbert Ammicht email: eammicht@lucent.com

   Markus Mottl email: markus.mottl@gmail.com WWW: http://www.ocaml.info

   Liam Stewart email: liam@cs.toronto.edu WWW: http://www.cs.toronto.edu/~liam

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
open Lacaml.Io

(* REAL GESVD DEMO *)

let () =
  let open Lacaml.D in
  let n = 3 in
  let a = Mat.hilbert n in
  printf "@[<2>General real matrix A = @[%a@]@]@\n@\n" pp_fmat a;
  let s, u, vt = gesvd a in
  printf "@[<2>Singular values S = @[%a@]@]@\n@\n" pp_rfvec s;
  printf "@[<2>Matrix U = @[%a@]@]@\n@\n" pp_fmat u;
  printf "@[<2>Matrix VT = @[%a@]@]@\n" pp_fmat vt

(* COMPLEX GESVD DEMO *)

let () =
  let open Lacaml.C in
  let a = Mat.random 3 2 in
  printf "@\n%s@\n@\n" (String.make 80 '-');
  printf "@[<2>General complex matrix A =@\n@\n  @[%a@]@]@\n@\n" pp_cmat a;
  let s, u, vt = gesvd a in
  printf "@[<2>Singular values S = @[%a@]@]@\n@\n" pp_rfvec s;
  printf "@[<2>Matrix U =@\n@\n  @[%a@]@]@\n@\n" pp_cmat u;
  printf "@[<2>Matrix VT =@\n@\n  @[%a@]@]@\n" pp_cmat vt
