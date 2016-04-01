(* File: mat_SD.ml

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

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

open Bigarray
open Lacaml_mat4_FPREC
open Lacaml_utils
open Lacaml_floatxx

let hilbert n =
  let mat = create n n in
  for col = 1 to n do
    for row = 1 to n do
      mat.{row, col} <- 1. /. float (row + col - 1)
    done
  done;
  mat

let hankel n =
  if n = 0 then empty
  else
    let mat = make n n 0. in
    let n1 = n + 1 in
    let rec loop c r =
      mat.{r, c} <- float (c + r - 1);
      if c + r >= n1 then
        if c = n then mat
        else loop (c + 1) 1
      else loop c (r + 1) in
    loop 1 1

let pascal n =
  let mat = make n n 1. in
  for r = 2 to n do
    for c = 2 to n do
      mat.{r, c} <- mat.{r - 1, c} +. mat.{r, c - 1}
    done
  done;
  mat

let rosser_ar =
  [|
    [|  611.;  196.; -192.;  407.;   -8.;  -52.;  -49.;   29.; |];
    [|  196.;  899.;  113.; -192.;  -71.;  -43.;   -8.;  -44.; |];
    [| -192.;  113.;  899.;  196.;   61.;   49.;    8.;   52.; |];
    [|  407.; -192.;  196.;  611.;    8.;   44.;   59.;  -23.; |];
    [|   -8.;  -71.;   61.;    8.;  411.; -599.;  208.;  208.; |];
    [|  -52.;  -43.;   49.;   44.; -599.;  411.;  208.;  208.; |];
    [|  -49.;   -8.;    8.;   59.;  208.;  208.;   99.; -911.; |];
    [|   29.;  -44.;   52.;  -23.;  208.;  208.; -911.;   99.; |];
  |]

let rosser () = Array2.of_array prec fortran_layout rosser_ar

let toeplitz (v : vec) =
  let len = Array1.dim v in
  if len = 0 then empty
  else if len = 1 then make 1 1 v.{1}
  else (
    if len mod 2 <> 1 then
      invalid_arg "toeplitz: v has even number of elements";
    let n = (len + 1) / 2 in
    let mat = create n n in
    let rec loop r c i =
      mat.{r, c} <- v.{i};
      if r = n then
        if c = n then loop 1 2 (i + 1)
        else loop (n - c) 1 (i + 1)
      else
        if c = n then
          if r = 1 then mat
          else loop 1 (n - r + 2) (i + 1)
        else loop (r + 1) (c + 1) i in
    loop n 1 1)

let vandermonde (v : vec) =
  let n = Array1.dim v in
  if n = 0 then empty
  else if n = 1 then make 1 1 1.0
  else (
    let mat = create n n in
    for i = 1 to n do mat.{i, 1} <- 1.0 done;
    for i = 1 to n do mat.{i, 2} <- v.{i} done;
    for pow = 2 to n - 1 do
      let fpow = float pow in
      for i = 1 to n do mat.{i, pow + 1} <- v.{i} ** fpow done
    done;
    mat)

let wilkinson n =
  if n < 3 then invalid_arg "wilkinson: n < 3";
  if n mod 2 <> 1 then invalid_arg "wilkinson: n is an even number";
  let mat = make n n 0.0 in
  for i = 2 to n do
    let i_1 = i - 1 in
    mat.{i, i_1} <- 1.0;
    mat.{i_1, i} <- 1.0
  done;
  let n_2 = n / 2 in
  let n1_2 = n_2 + 1 in
  for i = 1 to n_2 do mat.{i, i} <- float (n1_2 - i) done;
  for i = n1_2 + 1 to n do mat.{i, i} <- float (i - n1_2) done;
  mat

let random ?rnd_state ?(from = -1.) ?(range = 2.) m n =
  let mat = create m n in
  let state =
    match rnd_state with
    | None -> Random.get_state ()
    | Some state -> state in
  for row = 1 to m do
    for col = 1 to n do
      mat.{row, col} <- Random.State.float state range +. from
    done
  done;
  if rnd_state = None then Random.set_state state;
  mat


(* Unary matrix operations *)

external direct_abs :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECabs_mat_stub_bc" "lacaml_FPRECabs_mat_stub"

let abs ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.abs" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_abs ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_sqr :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECsqr_mat_stub_bc" "lacaml_FPRECsqr_mat_stub"

let sqr ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.sqr" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_sqr ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_sqrt :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECsqrt_mat_stub_bc" "lacaml_FPRECsqrt_mat_stub"

let sqrt ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.sqrt" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_sqrt ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_exp :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECexp_mat_stub_bc" "lacaml_FPRECexp_mat_stub"

let exp ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.exp" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_exp ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_log :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPREClog_mat_stub_bc" "lacaml_FPREClog_mat_stub"

let log ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.log" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_log ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_sin :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECsin_mat_stub_bc" "lacaml_FPRECsin_mat_stub"

let sin ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.sin" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_sin ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_cos :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECcos_mat_stub_bc" "lacaml_FPRECcos_mat_stub"

let cos ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.cos" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_cos ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_tan :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECtan_mat_stub_bc" "lacaml_FPRECtan_mat_stub"

let tan ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.tan" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_tan ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_tanh :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_FPRECtanh_mat_stub_bc" "lacaml_FPRECtanh_mat_stub"

let tanh ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.tanh" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  direct_tanh ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b


(* Ternary matrix operations *)

external direct_cpab :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  cr : int ->
  cc : int ->
  c : mat ->
  unit = "lacaml_FPRECcpab_stub_bc" "lacaml_FPRECcpab_stub"

let cpab ?m ?n
      ?(cr = 1) ?(cc = 1) c ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.FPREC.Mat.cpab" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  check_dim_mat loc c_str cr cc c m n;
  direct_cpab ~m ~n ~cr ~cc ~c ~ar ~ac ~a ~br ~bc ~b

external direct_cmab :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  cr : int ->
  cc : int ->
  c : mat ->
  unit = "lacaml_FPRECcmab_stub_bc" "lacaml_FPRECcmab_stub"

let cmab ?m ?n
      ?(cr = 1) ?(cc = 1) c ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.FPREC.Mat.cmab" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  check_dim_mat loc c_str cr cc c m n;
  direct_cmab ~m ~n ~cr ~cc ~c ~ar ~ac ~a ~br ~bc ~b
