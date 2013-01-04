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
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Lacaml_vec4_FPREC
open Utils
open Floatxx

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

external direct_reci :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECreci_stub_bc" "lacaml_FPRECreci_stub"

let vec_reci_loc = "Vec.reci"

let reci ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_reci_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_reci_loc y_str ofsy incy in
  let n = get_dim_vec vec_reci_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_reci_loc ~ofsy ~incy ~n y in
  direct_reci ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

external direct_sqr :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsqr_stub_bc" "lacaml_FPRECsqr_stub"

let vec_sqr_loc = "Vec.sqr"

let sqr ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sqr_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_sqr_loc y_str ofsy incy in
  let n = get_dim_vec vec_sqr_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_sqr_loc ~ofsy ~incy ~n y in
  direct_sqr ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

external direct_sqrt :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsqrt_stub_bc" "lacaml_FPRECsqrt_stub"

let vec_sqrt_loc = "Vec.sqrt"

let sqrt ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sqrt_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_sqrt_loc y_str ofsy incy in
  let n = get_dim_vec vec_sqrt_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_sqrt_loc ~ofsy ~incy ~n y in
  direct_sqrt ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

external direct_exp :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECexp_stub_bc" "lacaml_FPRECexp_stub"

let vec_exp_loc = "Vec.exp"

let exp ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_exp_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_exp_loc y_str ofsy incy in
  let n = get_dim_vec vec_exp_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_exp_loc ~ofsy ~incy ~n y in
  direct_exp ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

external direct_log :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPREClog_stub_bc" "lacaml_FPREClog_stub"

let vec_log_loc = "Vec.log"

let log ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_log_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_log_loc y_str ofsy incy in
  let n = get_dim_vec vec_log_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_log_loc ~ofsy ~incy ~n y in
  direct_log ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

external direct_sin :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsin_stub_bc" "lacaml_FPRECsin_stub"

let vec_sin_loc = "Vec.sin"

let sin ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sin_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_sin_loc y_str ofsy incy in
  let n = get_dim_vec vec_sin_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_sin_loc ~ofsy ~incy ~n y in
  direct_sin ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

external direct_cos :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECcos_stub_bc" "lacaml_FPRECcos_stub"

let vec_cos_loc = "Vec.cos"

let cos ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_cos_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_cos_loc y_str ofsy incy in
  let n = get_dim_vec vec_cos_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_cos_loc ~ofsy ~incy ~n y in
  direct_cos ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y
