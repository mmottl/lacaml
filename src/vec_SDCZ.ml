(* File: vec_SDCZ.ml

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
open Numberxx
open Common
open Utils

(* Creation of vectors and dimension accessor *)

let create = vec_create
let get_y_vec ~loc ~ofsy ~incy ~n y = get_vec loc y_str y ofsy incy n create
let get_z_vec ~loc ~ofsz ~incz ~n z = get_vec loc z_str z ofsz incz n create

external direct_fill :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  a:num_type_arg ->
  unit = "lacaml_NPRECfill_vec_stub_bc" "lacaml_NPRECfill_vec_stub"

let make n a =
  let x = create n in
  direct_fill ~n ~ofsx:1 ~incx:1 ~x ~a;
  x

let make0 n = make n zero

let init n f =
  let v = create n in
  for i = 1 to n do
    v.{i} <- f i
  done;
  v

let dim (vec : vec) = Array1.dim vec
let has_zero_dim (vec : vec) = dim vec = 0

let of_array ar =
  let n = Array.length ar in
  let v = create n in
  for i = 1 to n do
    v.{i} <- ar.(i - 1)
  done;
  v

let to_array (v : vec) =
  let n = dim v in
  if n = 0 then [||]
  else
    let ar = Array.make n v.{1} in
    for i = 2 to n do
      ar.(i - 1) <- v.{i}
    done;
    ar

let of_list l =
  let n = List.length l in
  let v = create n in
  let coll ix el =
    v.{ix} <- el;
    ix + 1
  in
  ignore (List.fold_left coll 1 l);
  v

let to_list (v : vec) =
  let rec loop i acc = if i = 0 then acc else loop (i - 1) (v.{i} :: acc) in
  loop (dim v) []

let append (v1 : vec) (v2 : vec) =
  let n1 = dim v1 in
  let n2 = dim v2 in
  let n = n1 + n2 in
  let res = create n in
  for i = 1 to n1 do
    res.{i} <- v1.{i}
  done;
  for i = 1 to n2 do
    res.{i + n1} <- v2.{i}
  done;
  res

let rec coll_size n = function [] -> n | v :: vs -> coll_size (dim v + n) vs

external direct_copy :
  n:(int[@untagged]) ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

let concat vs =
  let y = create (coll_size 0 vs) in
  let coll ofsy x =
    let n = dim x in
    direct_copy ~n ~ofsy ~incy:1 ~y ~ofsx:1 ~incx:1 ~x;
    ofsy + n
  in
  ignore (List.fold_left coll 1 vs);
  y

let empty = create 0

external direct_linspace :
  y:vec -> a:num_type_arg -> b:num_type_arg -> n:(int[@untagged]) -> unit
  = "lacaml_NPREClinspace_stub_bc" "lacaml_NPREClinspace_stub"

let linspace ?y a b n =
  let y =
    match y with
    | Some y ->
        check_vec "Vec.linspace" y_str y n;
        y
    | None -> create n
  in
  direct_linspace ~y ~a ~b ~n;
  y

external direct_logspace :
  y:vec ->
  a:num_type_arg ->
  b:num_type_arg ->
  base:(float[@unboxed]) ->
  n:(int[@untagged]) ->
  unit = "lacaml_NPREClogspace_stub_bc" "lacaml_NPREClogspace_stub"

let logspace ?y a b ?(base = 10.0) n =
  if base <= 0.0 then invalid_arg "Vec.logspace: base <= 0.0";
  let y =
    match y with
    | Some y ->
        check_vec "Vec.logspace" y_str y n;
        y
    | None -> create n
  in
  direct_logspace ~y ~a ~b ~base ~n;
  y

(* Iterators over vectors *)

let vec_map_str = "Vec.map"

let map f ?n ?ofsy ?incy ?y ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_map_str x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_map_str y_str ofsy incy in
  let n = get_dim_vec vec_map_str x_str ofsx incx x n_str n in
  let min_dim_y = ofsy + ((n - 1) * abs incy) in
  let y =
    match y with
    | Some y ->
        check_vec vec_map_str y_str y min_dim_y;
        y
    | None -> create min_dim_y
  in
  let start, stop = get_vec_start_stop ~incx ~ofsx ~n in
  let i_ref = ref start in
  let j_ref = ref (if incy > 0 then ofsy else min_dim_y) in
  while !i_ref <> stop do
    y.{!j_ref} <- f x.{!i_ref};
    i_ref := !i_ref + incx;
    j_ref := !j_ref + incy
  done;
  y

let vec_iter_str = "Vec.iter"

let iter f ?n ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_iter_str x_str ofsx incx in
  let n = get_dim_vec vec_iter_str x_str ofsx incx x n_str n in
  let start, stop = get_vec_start_stop ~incx ~ofsx ~n in
  let i_ref = ref start in
  while !i_ref <> stop do
    let i = !i_ref in
    f x.{i};
    i_ref := i + incx
  done

let vec_iteri_str = "Vec.iteri"

let iteri f ?n ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_iteri_str x_str ofsx incx in
  let n = get_dim_vec vec_iteri_str x_str ofsx incx x n_str n in
  let start, stop = get_vec_start_stop ~incx ~ofsx ~n in
  let i_ref = ref start in
  while !i_ref <> stop do
    let i = !i_ref in
    f i x.{i};
    i_ref := i + incx
  done

let vec_fold_str = "Vec.fold"

let fold f acc ?n ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_fold_str x_str ofsx incx in
  let n = get_dim_vec vec_fold_str x_str ofsx incx x n_str n in
  let start, stop = get_vec_start_stop ~incx ~ofsx ~n in
  let rec loop acc i =
    if i = stop then acc else loop (f acc x.{i}) (i + incx)
  in
  loop acc start

(* Operations on one vector *)

(* REV *)

let rev (x : vec) =
  let n = dim x in
  let res = create n in
  let n1 = n + 1 in
  for i = 1 to n do
    res.{n1 - i} <- x.{i}
  done;
  res

(* FILL *)

let vec_fill_str = "Vec.fill"

let fill ?n ?ofsx ?incx x a =
  let ofsx, incx = get_vec_geom vec_fill_str x_str ofsx incx in
  let n = get_dim_vec vec_fill_str x_str ofsx incx x n_str n in
  direct_fill ~n ~ofsx ~incx ~x ~a

(* MAX *)

external direct_max :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  num_type_arg = "lacaml_NPRECmax_stub_bc" "lacaml_NPRECmax_stub"

let vec_max_str = "Vec.max"

let max ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_max_str x_str ofsx incx in
  let n = get_dim_vec vec_max_str x_str ofsx incx x n_str n in
  direct_max ~n ~ofsx ~incx ~x

(* MIN *)

let vec_min_str = "Vec.min"

external direct_min :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  num_type_arg = "lacaml_NPRECmin_stub_bc" "lacaml_NPRECmin_stub"

let min ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_min_str x_str ofsx incx in
  let n = get_dim_vec vec_min_str x_str ofsx incx x n_str n in
  direct_min ~n ~ofsx ~incx ~x

(* SUM *)

external direct_sum :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  num_type_arg = "lacaml_NPRECsum_vec_stub_bc" "lacaml_NPRECsum_vec_stub"

let vec_sum_str = "Vec.sum"

let sum ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sum_str x_str ofsx incx in
  let n = get_dim_vec vec_sum_str x_str ofsx incx x n_str n in
  direct_sum ~n ~ofsx ~incx ~x

(* PROD *)

let vec_prod_str = "Vec.prod"

external direct_prod :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  num_type_arg = "lacaml_NPRECprod_stub_bc" "lacaml_NPRECprod_stub"

let prod ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_prod_str x_str ofsx incx in
  let n = get_dim_vec vec_prod_str x_str ofsx incx x n_str n in
  direct_prod ~n ~ofsx ~incx ~x

(* ADD_CONST *)

external direct_add_const :
  c:num_type_arg ->
  n:(int[@untagged]) ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECadd_const_vec_stub_bc" "lacaml_NPRECadd_const_vec_stub"

let vec_add_const_str = "Vec.add_const"

let add_const c ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_add_const_str x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_add_const_str y_str ofsy incy in
  let n = get_dim_vec vec_add_const_str x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_add_const_str ~ofsy ~incy ~n y in
  direct_add_const ~c ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

(* SQR_NRM2 *)

external direct_sqr_nrm2 :
  stable:bool ->
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  (float[@unboxed]) = "lacaml_NPRECsqr_nrm2_stub_bc" "lacaml_NPRECsqr_nrm2_stub"

let sqr_nrm2 ?(stable = false) ?n ?ofsx ?incx x =
  let loc = "Vec.sqr_nrm2" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_sqr_nrm2 ~stable ~n ~ofsx ~incx ~x

(* SSQR *)

external direct_ssqr :
  n:(int[@untagged]) ->
  c:num_type_arg ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  num_type_arg = "lacaml_NPRECssqr_stub_bc" "lacaml_NPRECssqr_stub"

external direct_ssqr_zero :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  num_type_arg = "lacaml_NPRECssqr_zero_stub_bc" "lacaml_NPRECssqr_zero_stub"

let vec_ssqr_str = "Vec.ssqr"

let ssqr ?n ?c ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_ssqr_str x_str ofsx incx in
  let n = get_dim_vec vec_ssqr_str x_str ofsx incx x n_str n in
  match c with
  | None -> direct_ssqr_zero ~n ~ofsx ~incx ~x
  | Some c ->
      if c = zero then direct_ssqr_zero ~n ~ofsx ~incx ~x
      else direct_ssqr ~n ~c ~ofsx ~incx ~x

(* SORT *)

external direct_sort_incr :
  cmp:(num_type -> num_type -> int) ->
  (* not used, ususal order *)
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECsort_incr_bc" "lacaml_NPRECsort_incr"

external direct_sort_incr_perm :
  cmp:(num_type -> num_type -> int) ->
  (* not used, ususal order *)
  n:(int[@untagged]) ->
  ofsp:(int[@untagged]) ->
  incp:(int[@untagged]) ->
  p:int_vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECsort_incr_perm_bc" "lacaml_NPRECsort_incr_perm"

external direct_sort_decr :
  cmp:(num_type -> num_type -> int) ->
  (* not used, usual decreasing order *)
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECsort_decr_bc" "lacaml_NPRECsort_decr"

external direct_sort_decr_perm :
  cmp:(num_type -> num_type -> int) ->
  (* not used, ususal order *)
  n:(int[@untagged]) ->
  ofsp:(int[@untagged]) ->
  incp:(int[@untagged]) ->
  p:int_vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECsort_decr_perm_bc" "lacaml_NPRECsort_decr_perm"

external direct_sort :
  cmp:(num_type -> num_type -> int) ->
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECsort_bc" "lacaml_NPRECsort"

external direct_sort_perm :
  cmp:(num_type -> num_type -> int) ->
  (* not used, ususal order *)
  n:(int[@untagged]) ->
  ofsp:(int[@untagged]) ->
  incp:(int[@untagged]) ->
  p:int_vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECsort_perm_bc" "lacaml_NPRECsort_perm"

let vec_sort_str = "Vec.sort"
let p_str = "p"
let dummy_cmp _ _ = 0

let sort ?cmp ?(decr = false) ?n ?ofsp ?incp ?p ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sort_str x_str ofsx incx in
  let n = get_dim_vec vec_sort_str x_str ofsx incx x n_str n in
  match p with
  | None -> (
      match cmp with
      | None ->
          if decr then direct_sort_decr ~cmp:dummy_cmp ~n ~ofsx ~incx ~x
          else direct_sort_incr ~cmp:dummy_cmp ~n ~ofsx ~incx ~x
      | Some cmp ->
          let cmp = if decr then fun x1 x2 -> cmp x2 x1 else cmp in
          direct_sort ~cmp ~n ~ofsx ~incx ~x)
  | Some p -> (
      let ofsp, incp = get_vec_geom vec_sort_str p_str ofsp incp in
      check_vec vec_sort_str p_str p n;
      match cmp with
      | None ->
          if decr then
            direct_sort_decr_perm ~cmp:dummy_cmp ~n ~ofsp ~incp ~p ~ofsx ~incx
              ~x
          else
            direct_sort_incr_perm ~cmp:dummy_cmp ~n ~ofsp ~incp ~p ~ofsx ~incx
              ~x
      | Some cmp ->
          let cmp = if decr then fun x1 x2 -> cmp x2 x1 else cmp in
          direct_sort_perm ~cmp ~n ~ofsp ~incp ~p ~ofsx ~incx ~x)

(* NEG *)

external direct_neg :
  n:(int[@untagged]) ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECneg_stub_bc" "lacaml_NPRECneg_stub"

let vec_neg_str = "Vec.neg"

let neg ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_neg_str x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_neg_str y_str ofsy incy in
  let n = get_dim_vec vec_neg_str x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_neg_str ~ofsy ~incy ~n y in
  direct_neg ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

(* RECI *)

external direct_reci :
  n:(int[@untagged]) ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  unit = "lacaml_NPRECreci_stub_bc" "lacaml_NPRECreci_stub"

let vec_reci_loc = "Vec.reci"

let reci ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_reci_loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_reci_loc y_str ofsy incy in
  let n = get_dim_vec vec_reci_loc x_str ofsx incx x n_str n in
  let y = get_y_vec ~loc:vec_reci_loc ~ofsy ~incy ~n y in
  direct_reci ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y

(* Operations on two vectors *)

(* ADD *)

external direct_add :
  n:(int[@untagged]) ->
  ofsz:(int[@untagged]) ->
  incz:(int[@untagged]) ->
  z:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  unit = "lacaml_NPRECadd_stub_bc" "lacaml_NPRECadd_stub"

let vec_add_str = "Vec.add"

let add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_add_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_add_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_add_str y_str ofsy incy in
  let n = get_dim_vec vec_add_str x_str ofsx incx x n_str n in
  check_vec vec_add_str y_str y (ofsy + ((n - 1) * abs incy));
  let z = get_z_vec ~loc:vec_add_str ~ofsz ~incz ~n z in
  direct_add ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y;
  z

(* SUB *)

external direct_sub :
  n:(int[@untagged]) ->
  ofsz:(int[@untagged]) ->
  incz:(int[@untagged]) ->
  z:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  unit = "lacaml_NPRECsub_stub_bc" "lacaml_NPRECsub_stub"

let vec_sub_str = "Vec.sub"

let sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_sub_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_sub_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_sub_str y_str ofsy incy in
  let n = get_dim_vec vec_sub_str x_str ofsx incx x n_str n in
  check_vec vec_sub_str y_str y (ofsy + ((n - 1) * abs incy));
  let z = get_z_vec ~loc:vec_sub_str ~ofsz ~incz ~n z in
  direct_sub ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y;
  z

(* MUL *)

external direct_mul :
  n:(int[@untagged]) ->
  ofsz:(int[@untagged]) ->
  incz:(int[@untagged]) ->
  z:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  unit = "lacaml_NPRECmul_stub_bc" "lacaml_NPRECmul_stub"

let vec_mul_str = "Vec.mul"

let mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_mul_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_mul_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_mul_str y_str ofsy incy in
  let n = get_dim_vec vec_mul_str x_str ofsx incx x n_str n in
  check_vec vec_mul_str y_str y (ofsy + ((n - 1) * abs incy));
  let z = get_z_vec ~loc:vec_mul_str ~ofsz ~incz ~n z in
  direct_mul ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y;
  z

(* DIV *)

external direct_div :
  n:(int[@untagged]) ->
  ofsz:(int[@untagged]) ->
  incz:(int[@untagged]) ->
  z:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  unit = "lacaml_NPRECdiv_stub_bc" "lacaml_NPRECdiv_stub"

let vec_div_str = "Vec.div"

let div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_div_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_div_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_div_str y_str ofsy incy in
  let n = get_dim_vec vec_div_str x_str ofsx incx x n_str n in
  check_vec vec_div_str y_str y (ofsy + ((n - 1) * abs incy));
  let z = get_z_vec ~loc:vec_div_str ~ofsz ~incz ~n z in
  direct_div ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y;
  z

(* ZPXY *)

external direct_zpxy :
  n:(int[@untagged]) ->
  ofsz:(int[@untagged]) ->
  incz:(int[@untagged]) ->
  z:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  unit = "lacaml_NPRECzpxy_stub_bc" "lacaml_NPRECzpxy_stub"

let vec_zpxy_str = "Vec.zpxy"

let zpxy ?n ?ofsz ?incz z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_zpxy_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_zpxy_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_zpxy_str y_str ofsy incy in
  let n = get_dim_vec vec_zpxy_str x_str ofsx incx x n_str n in
  check_vec vec_zpxy_str y_str y (ofsy + ((n - 1) * abs incy));
  check_vec vec_zpxy_str z_str z (ofsz + ((n - 1) * abs incz));
  direct_zpxy ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y

(* ZMXY *)

external direct_zmxy :
  n:(int[@untagged]) ->
  ofsz:(int[@untagged]) ->
  incz:(int[@untagged]) ->
  z:vec ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  unit = "lacaml_NPRECzmxy_stub_bc" "lacaml_NPRECzmxy_stub"

let vec_zmxy_str = "Vec.zmxy"

let zmxy ?n ?ofsz ?incz z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_zmxy_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_zmxy_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_zmxy_str y_str ofsy incy in
  let n = get_dim_vec vec_zmxy_str x_str ofsx incx x n_str n in
  check_vec vec_zmxy_str y_str y (ofsy + ((n - 1) * abs incy));
  check_vec vec_zpxy_str z_str z (ofsz + ((n - 1) * abs incz));
  direct_zmxy ~n ~ofsz ~incz ~z ~ofsx ~incx ~x ~ofsy ~incy ~y

(* SSQR_DIFF *)

external direct_ssqr_diff :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  num_type_arg = "lacaml_NPRECssqr_diff_stub_bc" "lacaml_NPRECssqr_diff_stub"

let vec_ssqr_diff_str = "Vec.ssqr_diff"

let ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y =
  let ofsx, incx = get_vec_geom vec_ssqr_diff_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_ssqr_diff_str y_str ofsy incy in
  let n = get_dim_vec vec_ssqr_diff_str x_str ofsx incx x n_str n in
  check_vec vec_ssqr_diff_str y_str y (ofsy + ((n - 1) * abs incy));
  direct_ssqr_diff ~n ~ofsx ~incx ~x ~ofsy ~incy ~y
