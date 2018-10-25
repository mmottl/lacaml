(* File: lin_reg.ml

   Copyright (C) 2001-2005

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

open Lacaml.D
open Lacaml.Io

let () =
  let m = 5 in
  let n = 3 in

  let data_mat = Mat.create m n in
  let data_mat_copy = Mat.create m n in
  let res_len = max 1 (max m n) in
  let res_mat = Mat.create_mvec res_len in
  let res_mat_copy = Mat.create_mvec res_len in

  for i = 1 to m do
    let v_ref = ref 0.0 in

    for j = 1 to n do
      let randf = Random.float 200.0 -. 100.0 in
      v_ref := !v_ref +. float j *. randf;
      data_mat.{i, j} <- randf;
      data_mat_copy.{i, j} <- randf;
    done;

    let v = !v_ref in
    res_mat.{i, 1} <- v;
    res_mat_copy.{i, 1} <- v;
  done;

  printf
    "\
      @[<2>Predictor variables:\n\
        @\n\
        %a@]\n\
      @\n\
      @[<2>Response variable:\n\
        @\n\
        %a@]@\n\
      @\n"
    pp_fmat data_mat
    pp_rfvec (Mat.col res_mat 1);

  let rank = gelsd data_mat res_mat in

  printf
    "\
      @[<2>Regression weights:\n\
        @\n\
        %a@]\n\
      @\n\
      Rank: %d@\n@\n"
    pp_rfvec (Mat.col res_mat 1)
    rank;

  let y = gemv data_mat_copy (Mat.col res_mat 1) in
  let b = Mat.col res_mat_copy 1 in

  printf
    "\
      @[<2>Check result (must be close to 0):\n\
        @\n\
        %a@]@\n"
    pp_rfvec (Vec.sub y b)
