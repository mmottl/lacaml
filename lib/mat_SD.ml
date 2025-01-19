(* File: mat_SD.ml

   Copyright © 2001-

   Markus Mottl <markus.mottl@gmail.com>

   Christophe Troestler <Christophe.Troestler@umons.ac.be>

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

open Bigarray
open Mat4_FPREC
open Utils
open Floatxx

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
      if c + r >= n1 then if c = n then mat else loop (c + 1) 1
      else loop c (r + 1)
    in
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
    [| 611.; 196.; -192.; 407.; -8.; -52.; -49.; 29. |];
    [| 196.; 899.; 113.; -192.; -71.; -43.; -8.; -44. |];
    [| -192.; 113.; 899.; 196.; 61.; 49.; 8.; 52. |];
    [| 407.; -192.; 196.; 611.; 8.; 44.; 59.; -23. |];
    [| -8.; -71.; 61.; 8.; 411.; -599.; 208.; 208. |];
    [| -52.; -43.; 49.; 44.; -599.; 411.; 208.; 208. |];
    [| -49.; -8.; 8.; 59.; 208.; 208.; 99.; -911. |];
    [| 29.; -44.; 52.; -23.; 208.; 208.; -911.; 99. |];
  |]

let rosser () = Array2.of_array prec fortran_layout rosser_ar

let toeplitz (v : vec) =
  let len = Array1.dim v in
  if len = 0 then empty
  else if len = 1 then make 1 1 v.{1}
  else (
    if len mod 2 <> 1 then invalid_arg "toeplitz: v has even number of elements";
    let n = (len + 1) / 2 in
    let mat = create n n in
    let rec loop r c i =
      mat.{r, c} <- v.{i};
      if r = n then if c = n then loop 1 2 (i + 1) else loop (n - c) 1 (i + 1)
      else if c = n then if r = 1 then mat else loop 1 (n - r + 2) (i + 1)
      else loop (r + 1) (c + 1) i
    in
    loop n 1 1)

let vandermonde (v : vec) =
  let n = Array1.dim v in
  if n = 0 then empty
  else if n = 1 then make 1 1 1.0
  else
    let mat = create n n in
    for i = 1 to n do
      mat.{i, 1} <- 1.0
    done;
    for i = 1 to n do
      mat.{i, 2} <- v.{i}
    done;
    for pow = 2 to n - 1 do
      let fpow = float pow in
      for i = 1 to n do
        mat.{i, pow + 1} <- v.{i} ** fpow
      done
    done;
    mat

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
  for i = 1 to n_2 do
    mat.{i, i} <- float (n1_2 - i)
  done;
  for i = n1_2 + 1 to n do
    mat.{i, i} <- float (i - n1_2)
  done;
  mat

let random ?rnd_state ?(from = -1.) ?(range = 2.) m n =
  let mat = create m n in
  let state =
    match rnd_state with None -> Random.get_state () | Some state -> state
  in
  for row = 1 to m do
    for col = 1 to n do
      mat.{row, col} <- Random.State.float state range +. from
    done
  done;
  if rnd_state = None then Random.set_state state;
  mat

(* Unary matrix operations *)

let unop direct loc =
  let loc = "Lacaml.FPREC.Mat." ^ loc in
  fun ?patt ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a ->
    let m = get_dim1_mat loc a_str a ar m_str m in
    let n = get_dim2_mat loc a_str a ac n_str n in
    let b = get_mat loc b_str create br bc b m n in
    let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
    direct ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b;
    b

external direct_abs :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECabs_mat_stub_bc" "lacaml_FPRECabs_mat_stub"

let abs = unop direct_abs "abs"

external direct_signum :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsignum_mat_stub_bc" "lacaml_FPRECsignum_mat_stub"

let signum = unop direct_signum "signum"

external direct_sqr :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsqr_mat_stub_bc" "lacaml_FPRECsqr_mat_stub"

let sqr = unop direct_sqr "sqr"

external direct_sqrt :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsqrt_mat_stub_bc" "lacaml_FPRECsqrt_mat_stub"

let sqrt = unop direct_sqrt "sqrt"

external direct_cbrt :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECcbrt_mat_stub_bc" "lacaml_FPRECcbrt_mat_stub"

let cbrt = unop direct_cbrt "cbrt"

external direct_exp :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECexp_mat_stub_bc" "lacaml_FPRECexp_mat_stub"

let exp = unop direct_exp "exp"

external direct_exp2 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECexp2_mat_stub_bc" "lacaml_FPRECexp2_mat_stub"

let exp2 = unop direct_exp2 "exp2"

external direct_expm1 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECexpm1_mat_stub_bc" "lacaml_FPRECexpm1_mat_stub"

let expm1 = unop direct_expm1 "expm1"

external direct_log :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPREClog_mat_stub_bc" "lacaml_FPREClog_mat_stub"

let log = unop direct_log "log"

external direct_log10 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPREClog10_mat_stub_bc" "lacaml_FPREClog10_mat_stub"

let log10 = unop direct_log10 "log10"

external direct_log2 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPREClog2_mat_stub_bc" "lacaml_FPREClog2_mat_stub"

let log2 = unop direct_log2 "log2"

external direct_log1p :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPREClog1p_mat_stub_bc" "lacaml_FPREClog1p_mat_stub"

let log1p = unop direct_log1p "log1p"

external direct_sin :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsin_mat_stub_bc" "lacaml_FPRECsin_mat_stub"

let sin = unop direct_sin "sin"

external direct_cos :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECcos_mat_stub_bc" "lacaml_FPRECcos_mat_stub"

let cos = unop direct_cos "cos"

external direct_tan :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECtan_mat_stub_bc" "lacaml_FPRECtan_mat_stub"

let tan = unop direct_tan "tan"

external direct_asin :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECasin_mat_stub_bc" "lacaml_FPRECasin_mat_stub"

let asin = unop direct_asin "asin"

external direct_acos :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECacos_mat_stub_bc" "lacaml_FPRECacos_mat_stub"

let acos = unop direct_acos "acos"

external direct_atan :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECatan_mat_stub_bc" "lacaml_FPRECatan_mat_stub"

let atan = unop direct_atan "atan"

external direct_sinh :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsinh_mat_stub_bc" "lacaml_FPRECsinh_mat_stub"

let sinh = unop direct_sinh "sinh"

external direct_cosh :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECcosh_mat_stub_bc" "lacaml_FPRECcosh_mat_stub"

let cosh = unop direct_cosh "cosh"

external direct_tanh :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECtanh_mat_stub_bc" "lacaml_FPRECtanh_mat_stub"

let tanh = unop direct_tanh "tanh"

external direct_asinh :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECasinh_mat_stub_bc" "lacaml_FPRECasinh_mat_stub"

let asinh = unop direct_asinh "asinh"

external direct_acosh :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECacosh_mat_stub_bc" "lacaml_FPRECacosh_mat_stub"

let acosh = unop direct_acosh "acosh"

external direct_atanh :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECatanh_mat_stub_bc" "lacaml_FPRECatanh_mat_stub"

let atanh = unop direct_atanh "atanh"

external direct_floor :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECfloor_mat_stub_bc" "lacaml_FPRECfloor_mat_stub"

let floor = unop direct_floor "floor"

external direct_ceil :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECceil_mat_stub_bc" "lacaml_FPRECceil_mat_stub"

let ceil = unop direct_ceil "ceil"

external direct_round :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECround_mat_stub_bc" "lacaml_FPRECround_mat_stub"

let round = unop direct_round "round"

external direct_trunc :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECtrunc_mat_stub_bc" "lacaml_FPRECtrunc_mat_stub"

let trunc = unop direct_trunc "trunc"

external direct_erf :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECerf_mat_stub_bc" "lacaml_FPRECerf_mat_stub"

let erf = unop direct_erf "erf"

external direct_erfc :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECerfc_mat_stub_bc" "lacaml_FPRECerfc_mat_stub"

let erfc = unop direct_erfc "erfc"

external direct_logistic :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPREClogistic_mat_stub_bc" "lacaml_FPREClogistic_mat_stub"

let logistic = unop direct_logistic "logistic"

external direct_relu :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECrelu_mat_stub_bc" "lacaml_FPRECrelu_mat_stub"

let relu = unop direct_relu "relu"

external direct_softplus :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsoftplus_mat_stub_bc" "lacaml_FPRECsoftplus_mat_stub"

let softplus = unop direct_softplus "softplus"

external direct_softsign :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  unit = "lacaml_FPRECsoftsign_mat_stub_bc" "lacaml_FPRECsoftsign_mat_stub"

let softsign = unop direct_softsign "softsign"

(* Binary matrix operations *)

let binop direct loc =
  let loc = "Lacaml.FPREC.Mat." ^ loc in
  fun ?patt ?m ?n ?(cr = 1) ?(cc = 1) ?c ?(ar = 1) ?(ac = 1) a ?(br = 1)
      ?(bc = 1) b ->
    let m = get_dim1_mat loc a_str a ar m_str m in
    let n = get_dim2_mat loc a_str a ac n_str n in
    check_dim_mat loc b_str br bc b m n;
    let c = get_mat loc c_str create cr cc c m n in
    let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
    direct ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c;
    c

external direct_pow :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPRECpow_mat_stub_bc" "lacaml_FPRECpow_mat_stub"

let pow = binop direct_pow "pow"

external direct_atan2 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPRECatan2_mat_stub_bc" "lacaml_FPRECatan2_mat_stub"

let atan2 = binop direct_atan2 "atan2"

external direct_hypot :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPREChypot_mat_stub_bc" "lacaml_FPREChypot_mat_stub"

let hypot = binop direct_hypot "hypot"

external direct_min2 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPRECmin2_mat_stub_bc" "lacaml_FPRECmin2_mat_stub"

let min2 = binop direct_min2 "min2"

external direct_max2 :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPRECmax2_mat_stub_bc" "lacaml_FPRECmax2_mat_stub"

let max2 = binop direct_max2 "max2"

external direct_sum_prod :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  (float[@unboxed])
  = "lacaml_FPRECsum_prod_mat_stub_bc" "lacaml_FPRECsum_prod_mat_stub"

let sum_prod ?patt ?m ?n ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.FPREC.Mat.sum_prod" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_sum_prod ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b

(* Ternary matrix operations *)

let cqab direct loc =
  let loc = "Lacaml.FPREC.Mat." ^ loc in
  fun ?patt ?m ?n ?(cr = 1) ?(cc = 1) c ?(ar = 1) ?(ac = 1) a ?(br = 1)
      ?(bc = 1) b ->
    let m = get_dim1_mat loc a_str a ar m_str m in
    let n = get_dim2_mat loc a_str a ac n_str n in
    check_dim_mat loc b_str br bc b m n;
    check_dim_mat loc c_str cr cc c m n;
    let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
    direct ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c

external direct_cpab :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPRECcpab_stub_bc" "lacaml_FPRECcpab_stub"

let cpab = cqab direct_cpab "cpab"

external direct_cmab :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  br:(int[@untagged]) ->
  bc:(int[@untagged]) ->
  b:mat ->
  cr:(int[@untagged]) ->
  cc:(int[@untagged]) ->
  c:mat ->
  unit = "lacaml_FPRECcmab_stub_bc" "lacaml_FPRECcmab_stub"

let cmab = cqab direct_cmab "cmab"

(* Misc functions *)

external direct_log_sum_exp :
  pkind:Mat_patt.kind ->
  pinit:(int[@untagged]) ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  (float[@unboxed])
  = "lacaml_FPREClog_sum_exp_mat_stub_bc" "lacaml_FPREClog_sum_exp_mat_stub"

let log_sum_exp ?patt ?m ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.FPREC.Mat.log_sum_exp" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_log_sum_exp ~pkind ~pinit ~m ~n ~ar ~ac ~a
