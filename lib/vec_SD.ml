(* File: vec_SD.ml

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
open Vec4_FPREC
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

external direct_sqr :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsqr_stub_bc" "lacaml_FPRECsqr_stub"

let vec_sqr_str = "Vec.sqr"

let get_y_params ~ofsy ~incy ~n y =
  let min_dim_y = ofsy + (n - 1) * abs incy in
  match y with
  | Some y -> check_vec vec_sqr_str y_str y min_dim_y; y, ofsy, incy
  | None -> create min_dim_y, 1, 1

let sqr ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sqr_str x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_sqr_str y_str ofsy incy in
  let n = get_dim_vec vec_sqr_str x_str ofsx incx x n_str n in
  let y, ofsy, incy = get_y_params ~ofsy ~incy ~n y in
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

let vec_sqrt_str = "Vec.sqrt"

let sqrt ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sqrt_str x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_sqrt_str y_str ofsy incy in
  let n = get_dim_vec vec_sqrt_str x_str ofsx incx x n_str n in
  let y, ofsy, incy = get_y_params ~ofsy ~incy ~n y in
  direct_sqrt ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y
