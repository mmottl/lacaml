(* File: vec_SDCZ.ml

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
open Printf
open Utils
open Numberxx

(* Creation of vectors and dimension accessor *)

let create n = Array1.create prec fortran_layout n

let make n x =
  let v = create n in
  Array1.fill v x;
  v

let make0 n = make n zero

let init n f =
  let v = create n in
  for i = 1 to n do v.{i} <- f i done;
  v

let dim (v : vec) = Array1.dim v

let of_array ar =
  let n = Array.length ar in
  let v = create n in
  for i = 1 to n do
    v.{i} <- ar.(i - 1);
  done;
  v

let to_array v =
  let n = dim v in
  if n = 0 then [||]
  else
    let ar = Array.make n v.{1} in
    for i = 2 to n do
      ar.(i - 1) <- v.{i};
    done;
    ar

let of_list l =
  let n = List.length l in
  let v = create n in
  let coll ix el = v.{ix} <- el; ix + 1 in
  ignore (List.fold_left coll 1 l);
  v

let to_list v =
  let n = dim v in
  let res = ref [] in
  for i = n downto 1 do
    res := v.{i} :: !res
  done;
  !res

let append v1 v2 =
  let n1 = dim v1 in
  let n2 = dim v2 in
  let n = n1 + n2 in
  let res = create n in
  for i = 1 to n1 do res.{i} <- v1.{i} done;
  for i = 1 to n2 do res.{i + n1} <- v2.{i} done;
  res

let rec coll_size n = function
  | [] -> n
  | v :: vs -> coll_size (dim v + n) vs

external direct_copy :
  int -> (* N *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *) ->
  unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

let concat vs =
  let res = create (coll_size 0 vs) in
  let coll ofsy v =
    let n = dim v in
    direct_copy n ofsy 1 res 1 1 v;
    ofsy + n
  in
  ignore (List.fold_left coll 1 vs);
  res

let empty = create 0

external direct_linspace :
  vec ->      (* Y *)
  num_type -> (* A *)
  num_type -> (* B *)
  int         (* N *) ->
  unit = "lacaml_NPREClinspace_stub"

let linspace ?y a b n =
  let y =
    match y with
    | Some y -> check_vec "Vec.linspace" y_str y n; y
    | None -> create n in
  direct_linspace y a b n;
  y

external direct_logspace :
  vec ->      (* Y *)
  num_type -> (* A *)
  num_type -> (* B *)
  float ->    (* BASE *)
  int         (* N *) ->
  unit = "lacaml_NPREClogspace_stub"

let logspace ?y a b ?(base = 10.0) n =
  if base <= 0.0 then invalid_arg "Vec.logspace: base <= 0.0";
  let y =
    match y with
    | Some y -> check_vec "Vec.logspace" y_str y n; y
    | None -> create n in
  direct_logspace y a b base n;
  y


(* Iterators over vectors *)

let get_i_ref_last ~incx ~ofsx ~n =
  if incx > 0 then ref ofsx, ofsx + n * incx
  else ref (ofsx - (n - 1) * incx), ofsx + incx

let vec_map_str = "Vec.map"

let map f ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_map_str x_str ofsx incx in
  let ofsy, incy = get_vec_geom vec_map_str y_str ofsy incy in
  let n = get_dim_vec vec_map_str x_str ofsx incx x n_str n in
  let min_dim_y = ofsy + (n - 1) * abs incy in
  let y, ofsy, incy =
    match y with
    | Some y -> check_vec vec_map_str y_str y min_dim_y; y, ofsy, incy
    | None -> create min_dim_y, 1, 1 in
  let i_ref, last_i = get_i_ref_last ~incx ~ofsx ~n in
  let j_ref = ref(if incy > 0 then ofsy else min_dim_y) in
  while !i_ref <> last_i do
    y.{!j_ref} <- f x.{!i_ref};
    i_ref := !i_ref + incx;
    j_ref := !j_ref + incy;
  done;
  y

let vec_iter_str = "Vec.iter"

let iter f ?n ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_iter_str x_str ofsx incx in
  let n = get_dim_vec vec_iter_str x_str ofsx incx x n_str n in
  let i_ref, last_i = get_i_ref_last ~incx ~ofsx ~n in
  while !i_ref <> last_i do
    let i = !i_ref in
    f x.{i};
    i_ref := i + incx;
  done

let vec_iteri_str = "Vec.iteri"

let iteri f ?n ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_iteri_str x_str ofsx incx in
  let n = get_dim_vec vec_iteri_str x_str ofsx incx x n_str n in
  let i_ref, last_i = get_i_ref_last ~incx ~ofsx ~n in
  while !i_ref <> last_i do
    let i = !i_ref in
    f i x.{i};
    i_ref := i + incx;
  done

let vec_fold_str = "Vec.fold"

let fold f acc ?n ?ofsx ?incx (x : vec) =
  let ofsx, incx = get_vec_geom vec_fold_str x_str ofsx incx in
  let n = get_dim_vec vec_fold_str x_str ofsx incx x n_str n in
  let start, last =
    if incx > 0 then ofsx, ofsx + n * incx
    else ofsx - (n - 1)*incx, ofsx + incx
  in
  let rec loop acc i =
    if i = last then acc
    else loop (f acc x.{i}) (i + incx)
  in
  loop acc start


(* Operations on one vector *)

(* REV *)

let rev x =
  let n = dim x in
  let res = create n in
  let n1 = n + 1 in
  for i = 1 to n do
    res.{n1 - i} <- x.{i}
  done;
  res

(* MAX *)

external direct_max :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  num_type = "lacaml_NPRECmax_stub"

let vec_max_str = "Vec.max"

let max ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_max_str x_str ofsx incx in
  let n = get_dim_vec vec_max_str x_str ofsx incx x n_str n in
  direct_max n ofsx incx x

(* MIN *)

let vec_min_str = "Vec.min"

external direct_min :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  num_type = "lacaml_NPRECmin_stub"

let min ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_min_str x_str ofsx incx in
  let n = get_dim_vec vec_min_str x_str ofsx incx x n_str n in
  direct_min n ofsx incx x

(* SUM *)

external direct_sum :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  num_type = "lacaml_NPRECsum_stub"

let vec_sum_str = "Vec.sum"

let sum ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_sum_str x_str ofsx incx in
  let n = get_dim_vec vec_sum_str x_str ofsx incx x n_str n in
  direct_sum n ofsx incx x

(* PROD *)

let vec_prod_str = "Vec.prod"

external direct_prod :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  num_type = "lacaml_NPRECprod_stub"

let prod ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_prod_str x_str ofsx incx in
  let n = get_dim_vec vec_prod_str x_str ofsx incx x n_str n in
  direct_prod n ofsx incx x

(* SQR_NRM2 *)

external direct_sqr_nrm2 :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  float = "lacaml_NPRECsqr_nrm2_stub"

let sqr_nrm2 ?n ?ofsx ?incx x =
  let loc = "Vec.sqr_nrm2" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_sqr_nrm2 n ofsx incx x

(* SSQR *)

external direct_ssqr :
  int -> (* N *)
  num_type -> (* C *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  num_type = "lacaml_NPRECssqr_stub"

external direct_ssqr_zero :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *) ->
  num_type = "lacaml_NPRECssqr_zero_stub"

let vec_ssqr_str = "Vec.ssqr"

let ssqr ?n ?c ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom vec_ssqr_str x_str ofsx incx in
  let n = get_dim_vec vec_ssqr_str x_str ofsx incx x n_str n in
  match c with
  | None -> direct_ssqr_zero n ofsx incx x
  | Some c ->
      if c = zero then direct_ssqr_zero n ofsx incx x
      else direct_ssqr n c ofsx incx x

(* SORT *)


(* NEG *)

external direct_neg :
  int -> (* N *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec -> (* X *)
  unit = "lacaml_NPRECneg_stub_bc" "lacaml_NPRECneg_stub"

let vec_neg_str = "Vec.neg"

let neg ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let ofsy, incy = get_vec_geom vec_neg_str y_str ofsy incy
  and ofsx, incx = get_vec_geom vec_neg_str x_str ofsx incx in
  let n = get_dim_vec vec_neg_str x_str ofsx incx x n_str n in
  let y, ofsy, incy =
    let min_dim_y = ofsy + (n - 1) * abs incy in
    match y with
    | Some y -> check_vec vec_map_str y_str y min_dim_y; y, ofsy, incy
    | None -> create min_dim_y, 1, 1 in
  direct_neg n ofsy incy y ofsx incx x;
  y

(* Operations on two vectors *)

(* ADD *)

external direct_add :
  int -> (* N *)
  int -> (* OFSZ *)
  int -> (* INCZ *)
  vec -> (* Z *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec -> (* X *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec    (* Y *) ->
  unit = "lacaml_NPRECadd_stub_bc" "lacaml_NPRECadd_stub"

let vec_add_str = "Vec.add"

let add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_add_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_add_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_add_str y_str ofsy incy in
  let n = get_dim_vec vec_add_str x_str ofsx incx x n_str n in
  check_vec vec_add_str y_str y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec vec_add_str z_str z min_dim_z; z, ofsz, incz
    | None -> create min_dim_z, 1, 1 in
  direct_add n ofsz incz z ofsx incx x ofsy incy y;
  z


(* SUB *)

external direct_sub :
  int -> (* N *)
  int -> (* OFSZ *)
  int -> (* INCZ *)
  vec -> (* Z *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec -> (* X *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec    (* Y *) ->
  unit = "lacaml_NPRECsub_stub_bc" "lacaml_NPRECsub_stub"

let vec_sub_str = "Vec.sub"

let sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_sub_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_sub_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_sub_str y_str ofsy incy in
  let n = get_dim_vec vec_sub_str x_str ofsx incx x n_str n in
  check_vec vec_sub_str y_str y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec vec_sub_str z_str z min_dim_z; z, ofsz, incz
    | None -> create min_dim_z, 1, 1 in
  direct_sub n ofsz incz z ofsx incx x ofsy incy y;
  z


(* MUL *)

external direct_mul :
  int -> (* N *)
  int -> (* OFSZ *)
  int -> (* INCZ *)
  vec -> (* Z *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec -> (* X *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec    (* Y *) ->
  unit = "lacaml_NPRECmul_stub_bc" "lacaml_NPRECmul_stub"

let vec_mul_str = "Vec.mul"

let mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_mul_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_mul_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_mul_str y_str ofsy incy in
  let n = get_dim_vec vec_mul_str x_str ofsx incx x n_str n in
  check_vec vec_mul_str y_str y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec vec_mul_str z_str z min_dim_z; z, ofsz, incz
    | None -> create min_dim_z, 1, 1 in
  direct_mul n ofsz incz z ofsx incx x ofsy incy y;
  z

(* DIV *)

external direct_div :
  int -> (* N *)
  int -> (* OFSZ *)
  int -> (* INCZ *)
  vec -> (* Z *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec -> (* X *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec    (* Y *) ->
  unit = "lacaml_NPRECdiv_stub_bc" "lacaml_NPRECdiv_stub"

let vec_div_str = "Vec.div"

let div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom vec_div_str z_str ofsz incz
  and ofsx, incx = get_vec_geom vec_div_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_div_str y_str ofsy incy in
  let n = get_dim_vec vec_div_str x_str ofsx incx x n_str n in
  check_vec vec_div_str y_str y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec vec_div_str z_str z min_dim_z; z, ofsz, incz
    | None -> create min_dim_z, 1, 1 in
  direct_div n ofsz incz z ofsx incx x ofsy incy y;
  z

(* SSQR_DIFF *)

external direct_ssqr_diff :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec -> (* X *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec    (* Y *) ->
  num_type = "lacaml_NPRECssqr_diff_stub_bc" "lacaml_NPRECssqr_diff_stub"

let vec_ssqr_diff_str = "Vec.ssqr_diff"

let ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y =
  let ofsx, incx = get_vec_geom vec_ssqr_diff_str x_str ofsx incx
  and ofsy, incy = get_vec_geom vec_ssqr_diff_str y_str ofsy incy in
  let n = get_dim_vec vec_ssqr_diff_str x_str ofsx incx x n_str n in
  check_vec vec_ssqr_diff_str y_str y (ofsy + (n - 1) * abs incy);
  direct_ssqr_diff n ofsx incx x ofsy incy y
