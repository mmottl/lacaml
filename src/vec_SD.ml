(* File: vec_SD.ml

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/anum/

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

open Lacaml_vec4_FPREC
open Lacaml_utils
open Lacaml_floatxx

let random ?rnd_state ?(from = -1.) ?(range = 2.) n =
  let vec = create n in
  let state =
    match rnd_state with
    | None -> Random.get_state ()
    | Some state -> state in
  for row = 1 to n do
    vec.{row} <- Random.State.float state range +. from
  done;
  if rnd_state = None then Random.set_state state;
  vec

let get_y_vec ~loc ~ofsy ~incy ~n y = get_vec loc y_str y ofsy incy n create
let get_z_vec ~loc ~ofsz ~incz ~n z = get_vec loc z_str z ofsz incz n create


(* Unary vector operations *)

let int_abs = abs

let unop direct loc =
  let loc = "Lacaml.FPREC.Vec." ^ loc in
  fun ?n ?ofsy ?incy ?y ?ofsx ?incx x ->
    let ofsx, incx = get_vec_geom loc x_str ofsx incx in
    let ofsy, incy = get_vec_geom loc y_str ofsy incy in
    let n = get_dim_vec loc x_str ofsx incx x n_str n in
    let y = get_y_vec ~loc ~ofsy ~incy ~n y in
    direct ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
    y

external direct_abs :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECabs_stub_bc" "lacaml_FPRECabs_stub"

let abs = unop direct_abs "abs"

external direct_signum :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsignum_stub_bc" "lacaml_FPRECsignum_stub"

let signum = unop direct_signum "signum"

external direct_sqr :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsqr_stub_bc" "lacaml_FPRECsqr_stub"

let sqr = unop direct_sqr "sqr"

external direct_sqrt :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsqrt_stub_bc" "lacaml_FPRECsqrt_stub"

let sqrt = unop direct_sqrt "sqrt"

external direct_cbrt :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECcbrt_stub_bc" "lacaml_FPRECcbrt_stub"

let cbrt = unop direct_cbrt "cbrt"

external direct_exp :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECexp_stub_bc" "lacaml_FPRECexp_stub"

let exp = unop direct_exp "exp"

external direct_exp2 :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECexp2_stub_bc" "lacaml_FPRECexp2_stub"

let exp2 = unop direct_exp2 "exp2"

external direct_expm1 :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECexpm1_stub_bc" "lacaml_FPRECexpm1_stub"

let expm1 = unop direct_expm1 "expm1"

external direct_log :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPREClog_stub_bc" "lacaml_FPREClog_stub"

let log = unop direct_log "log"

external direct_log10 :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPREClog10_stub_bc" "lacaml_FPREClog10_stub"

let log10 = unop direct_log10 "log10"

external direct_log2 :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPREClog2_stub_bc" "lacaml_FPREClog2_stub"

let log2 = unop direct_log2 "log2"

external direct_log1p :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPREClog1p_stub_bc" "lacaml_FPREClog1p_stub"

let log1p = unop direct_log1p "log1p"

external direct_sin :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsin_stub_bc" "lacaml_FPRECsin_stub"

let sin = unop direct_sin "sin"

external direct_cos :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECcos_stub_bc" "lacaml_FPRECcos_stub"

let cos = unop direct_cos "cos"

external direct_tan :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECtan_stub_bc" "lacaml_FPRECtan_stub"

let tan = unop direct_tan "tan"

external direct_asin :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECasin_stub_bc" "lacaml_FPRECasin_stub"

let asin = unop direct_asin "asin"

external direct_acos :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECtan_stub_bc" "lacaml_FPRECacos_stub"

let acos = unop direct_acos "acos"

external direct_atan :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECatan_stub_bc" "lacaml_FPRECatan_stub"

let atan = unop direct_atan "atan"

external direct_sinh :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsinh_stub_bc" "lacaml_FPRECsinh_stub"

let sinh = unop direct_sinh "sinh"

external direct_cosh :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECcosh_stub_bc" "lacaml_FPRECcosh_stub"

let cosh = unop direct_cosh "cosh"

external direct_tanh :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECtanh_stub_bc" "lacaml_FPRECtanh_stub"

let tanh = unop direct_tanh "tanh"

external direct_asinh :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECasinh_stub_bc" "lacaml_FPRECasinh_stub"

let asinh = unop direct_asinh "asinh"

external direct_acosh :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECacosh_stub_bc" "lacaml_FPRECacosh_stub"

let acosh = unop direct_acosh "acosh"

external direct_atanh :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECatanh_stub_bc" "lacaml_FPRECatanh_stub"

let atanh = unop direct_atanh "atanh"

external direct_floor :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECfloor_stub_bc" "lacaml_FPRECfloor_stub"

let floor = unop direct_floor "floor"

external direct_ceil :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECceil_stub_bc" "lacaml_FPRECceil_stub"

let ceil = unop direct_ceil "ceil"

external direct_round :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECround_stub_bc" "lacaml_FPRECround_stub"

let round = unop direct_round "round"

external direct_trunc :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECtrunc_stub_bc" "lacaml_FPRECtrunc_stub"

let trunc = unop direct_trunc "trunc"

external direct_erf :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECerf_stub_bc" "lacaml_FPRECerf_stub"

let erf = unop direct_erf "erf"

external direct_erfc :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECerfc_stub_bc" "lacaml_FPRECerfc_stub"

let erfc = unop direct_erfc "erfc"

external direct_logistic :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPREClogistic_stub_bc" "lacaml_FPREClogistic_stub"

let logistic = unop direct_logistic "logistic"

external direct_relu :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECrelu_stub_bc" "lacaml_FPRECrelu_stub"

let relu = unop direct_relu "relu"

external direct_softplus :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsoftplus_stub_bc" "lacaml_FPRECsoftplus_stub"

let softplus = unop direct_softplus "softplus"

external direct_softsign :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsoftsign_stub_bc" "lacaml_FPRECsoftsign_stub"

let softsign = unop direct_softsign "softsign"


(* Binary vector operations *)

let binop direct loc =
  let loc = "Lacaml.FPREC.Vec." ^ loc in
  fun ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y ->
    let ofsz, incz = get_vec_geom loc z_str ofsz incz in
    let ofsx, incx = get_vec_geom loc x_str ofsx incx in
    let ofsy, incy = get_vec_geom loc y_str ofsy incy in
    let n = get_dim_vec loc x_str ofsx incx x n_str n in
    check_vec loc y_str y (ofsy + (n - 1) * int_abs incy);
    let z = get_z_vec ~loc ~ofsz ~incz ~n z in
    direct ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y;
    z

external direct_pow :
  n : int ->
  ofsz : int ->
  incz : int ->
  z : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  unit = "lacaml_FPRECpow_stub_bc" "lacaml_FPRECpow_stub"

let pow = binop direct_pow "pow"

external direct_atan2 :
  n : int ->
  ofsz : int ->
  incz : int ->
  z : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  unit = "lacaml_FPRECatan2_stub_bc" "lacaml_FPRECatan2_stub"

let atan2 = binop direct_atan2 "atan2"

external direct_hypot :
  n : int ->
  ofsz : int ->
  incz : int ->
  z : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  unit = "lacaml_FPREChypot_stub_bc" "lacaml_FPREChypot_stub"

let hypot = binop direct_hypot "hypot"

external direct_min2 :
  n : int ->
  ofsz : int ->
  incz : int ->
  z : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  unit = "lacaml_FPRECmin2_stub_bc" "lacaml_FPRECmin2_stub"

let min2 = binop direct_min2 "min2"

external direct_max2 :
  n : int ->
  ofsz : int ->
  incz : int ->
  z : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  unit = "lacaml_FPRECmax2_stub_bc" "lacaml_FPRECmax2_stub"

let max2 = binop direct_max2 "max2"


(* Misc functions *)

external direct_log_sum_exp :
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  num_type = "lacaml_FPREClog_sum_exp_vec_stub"

let log_sum_exp =
  let loc = "Lacaml.D.Vec.log_sum_exp" in
  fun ?n ?ofsx ?incx x ->
    let ofsx, incx = get_vec_geom loc x_str ofsx incx in
    let n = get_dim_vec loc x_str ofsx incx x n_str n in
    direct_log_sum_exp ~n ~ofsx ~incx ~x
