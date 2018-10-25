(* File: eig.ml

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
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Format

open Lacaml.Io

(* SYEV DEMO *)

let () =
  let open Lacaml.D in
  let n = 5 in

  let a = Mat.random ~from:(-500.) ~range:1000. n n in
  let a_copy = lacpy a in

  printf "@[<2>Symmetric real matrix A =@\n@\n@[%a@]@]@\n@\n" pp_fmat a;

  let w = syev a_copy in
  printf "\
    @[<2>Eigenvalues: W = @[%a@]@]@\n@\n\
    ----------------------------------------------------------------------@\n\
    @\n" pp_rfvec w


(* GEEV DEMO *)

let () =
  let open Lacaml.D in
  let n = 5 in

  let a = Mat.make0 n n in

  for i = 1 to n - 1 do
    a.{i, i + 1} <- 1.0
  done;

  (* All unperturbed eigenvalues are zero *)
  a.{n, 1} <- 1e-5; (* perturbation, try this: n >> 1 *)

  let a_copy = lacpy a in

  printf "@[<2>General real matrix A =@\n@\n@[%a@]@]@\n@\n" pp_fmat a;

  let _, wr, wi, right = geev ~vl:None a_copy in

  printf "@[<2>Eigenvalues: WR =@\n@\n@[%a@]@]@\n@\n" pp_rfvec wr;
  printf "@[<2>Eigenvalues: WI =@\n@\n@[%a@]@]@\n@\n" pp_rfvec wi;

  printf "\
    @[<2>Matrix VR =@\n@\n@[%a@]@]@\n@\n\
    ----------------------------------------------------------------------@\n\
    @\n" pp_fmat right


(* CGEEV DEMO *)

let () =
  let open Lacaml.C in
  let n = 3 in

  let a =
    Mat.random
      ~re_from:(-500.) ~re_range:1000.
      ~im_from:(-500.) ~im_range:1000.
      n n in
  let a_copy = lacpy a in

  printf "@[<2>General complex matrix A =@\n@\n@[%a@]@]@\n@\n" pp_cmat a;

  let _, w, _ = geev a_copy in
  printf "@[<2>Eigenvalues: W =@\n@\n@[%a@]@]@\n" pp_rcvec w
