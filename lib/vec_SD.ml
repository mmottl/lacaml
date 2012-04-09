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

(* SORT *)

external direct_sort_incr :
  cmp : (float -> float -> int) -> (* not used, ususal order *)
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsort_incr"

external direct_sort_incr_perm :
  cmp : (float -> float -> int) -> (* not used, ususal order *)
  n : int ->
  ofsp : int ->
  incp : int ->
  p : (int, int_elt, fortran_layout) Array1.t ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsort_incr_perm_bc" "lacaml_FPRECsort_incr_perm"

external direct_sort_decr :
  cmp : (float -> float -> int) -> (* not used, usual decreasing order *)
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsort_decr"

external direct_sort_decr_perm :
  cmp : (float -> float -> int) -> (* not used, ususal order *)
  n : int ->
  ofsp : int ->
  incp : int ->
  p : (int, int_elt, fortran_layout) Array1.t ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsort_decr_perm_bc" "lacaml_FPRECsort_decr_perm"

external direct_sort :
  cmp : (float -> float -> int) ->
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsort"

external direct_sort_perm :
  cmp : (float -> float -> int) -> (* not used, ususal order *)
  n : int ->
  ofsp : int ->
  incp : int ->
  p : (int, int_elt, fortran_layout) Array1.t ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_FPRECsort_perm_bc" "lacaml_FPRECsort_perm"

let vec_sort_str = "Vec.sort"
let p_str = "p"
let dummy_cmp _ _ = 0

let sort ?cmp ?(decr = false) ?n ?ofsp ?incp ?p ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sort_str x_str ofsx incx in
  let n = get_dim_vec vec_sort_str x_str ofsx incx x n_str n in
  match p with
  | None ->
      (match cmp with
       | None ->
           if decr then direct_sort_decr ~cmp:dummy_cmp ~n ~ofsx ~incx ~x
           else direct_sort_incr ~cmp:dummy_cmp ~n ~ofsx ~incx ~x
       | Some cmp ->
           let cmp = if decr then (fun x1 x2 -> cmp x2 x1) else cmp in
           direct_sort ~cmp ~n ~ofsx ~incx ~x
      )
  | Some p ->
      let ofsp, incp = get_vec_geom vec_sort_str p_str ofsp incp in
      check_vec vec_sort_str p_str p n;
      (match cmp with
       | None ->
           if decr then direct_sort_decr_perm ~cmp:dummy_cmp ~n
                                              ~ofsp ~incp ~p ~ofsx ~incx ~x
           else direct_sort_incr_perm ~cmp:dummy_cmp ~n
                                      ~ofsp ~incp ~p ~ofsx ~incx ~x
       | Some cmp ->
           let cmp = if decr then (fun x1 x2 -> cmp x2 x1) else cmp in
           direct_sort_perm ~cmp ~n ~ofsp ~incp ~p ~ofsx ~incx ~x
      )
