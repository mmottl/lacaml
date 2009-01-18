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
  for i = 1 to n do
    v.{i} <- f i
  done;
  v

let dim v = Array1.dim v

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
  vec (* X *)
  -> unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

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
  int         (* N *)
  -> unit = "lacaml_NPREClinspace_stub"

let linspace ?y a b n =
  let y =
    match y with
    | Some y -> check_vec "Vec.linspace" "y" y n; y
    | None -> create n in
  direct_linspace y a b n;
  y

external direct_logspace :
  vec ->      (* Y *)
  num_type -> (* A *)
  num_type -> (* B *)
  float ->    (* BASE *)
  int         (* N *)
  -> unit = "lacaml_NPREClogspace_stub"

let logspace ?y a b ?(base = 10.0) n =
  if base <= 0.0 then invalid_arg "Vec.logspace: base <= 0.0";
  let y =
    match y with
    | Some y -> check_vec "Vec.logspace" "y" y n; y
    | None -> create n in
  direct_logspace y a b base n;
  y


(* Iterators over vectors *)

let get_i_ref_last ~incx ~ofsx ~n =
  if incx > 0 then ref ofsx, ofsx + n * incx
  else ref (ofsx - (n - 1) * incx), ofsx + incx

let iter f ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.iter" "x" ofsx incx in
  let n = get_dim_vec "Vec.iter" "x" ofsx incx x "n" n in
  let i_ref, last_i = get_i_ref_last ~incx ~ofsx ~n in
  while !i_ref <> last_i do
    let i = !i_ref in
    f x.{i};
    i_ref := i + incx;
  done

let iteri f ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.iteri" "x" ofsx incx in
  let n = get_dim_vec "Vec.iteri" "x" ofsx incx x "n" n in
  let i_ref, last_i = get_i_ref_last ~incx ~ofsx ~n in
  while !i_ref <> last_i do
    let i = !i_ref in
    f i x.{i};
    i_ref := i + incx;
  done

external direct_fold :
  num_type -> (* INIT *)
  int ->      (* N *)
  int ->      (* OFSX *)
  int ->      (* INCX *)
  vec ->      (* X *)
  (num_type -> num_type -> num_type)
  -> num_type = "lacaml_NPRECfold_stub_bc" "lacaml_NPRECfold_stub"

let fold f a ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.fold" "x" ofsx incx in
  let n = get_dim_vec "Vec.fold" "x" ofsx incx x "n" n in
  direct_fold a n ofsx incx x f


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
  vec    (* X *)
  -> num_type = "lacaml_NPRECmax_stub"

let max ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.max" "x" ofsx incx in
  let n = get_dim_vec "Vec.max" "x" ofsx incx x "n" n in
  direct_max n ofsx incx x

(* MIN *)

external direct_min :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *)
  -> num_type = "lacaml_NPRECmin_stub"

let min ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.min" "x" ofsx incx in
  let n = get_dim_vec "Vec.min" "x" ofsx incx x "n" n in
  direct_min n ofsx incx x

(* SUM *)

external direct_sum :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *)
  -> num_type = "lacaml_NPRECsum_stub"

let sum ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.sum" "x" ofsx incx in
  let n = get_dim_vec "Vec.sum" "x" ofsx incx x "n" n in
  direct_sum n ofsx incx x

(* PROD *)

external direct_prod :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *)
  -> num_type = "lacaml_NPRECprod_stub"

let prod ?n ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.prod" "x" ofsx incx in
  let n = get_dim_vec "Vec.prod" "x" ofsx incx x "n" n in
  direct_prod n ofsx incx x

(* SSQR *)

external direct_ssqr :
  int -> (* N *)
  num_type -> (* C *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *)
  -> num_type = "lacaml_NPRECssqr_stub"

external direct_ssqr_zero :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec    (* X *)
  -> num_type = "lacaml_NPRECssqr_zero_stub"

let ssqr ?n ?c ?ofsx ?incx x =
  let ofsx, incx = get_vec_geom "Vec.ssqr" "x" ofsx incx in
  let n = get_dim_vec "Vec.ssqr" "x" ofsx incx x "n" n in
  match c with
  | None -> direct_ssqr_zero n ofsx incx x
  | Some c ->
      if c = zero then direct_ssqr_zero n ofsx incx x
      else direct_ssqr n c ofsx incx x

(* MAP *)

(* SORT *)


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
  vec    (* Y *)
  -> unit = "lacaml_NPRECadd_stub_bc" "lacaml_NPRECadd_stub"

let add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom "Vec.add" "z" ofsz incz
  and ofsx, incx = get_vec_geom "Vec.add" "x" ofsx incx
  and ofsy, incy = get_vec_geom "Vec.add" "y" ofsy incy in
  let n = get_dim_vec "Vec.add" "x" ofsx incx x "n" n in
  check_vec "Vec.add" "y" y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec "Vec.add" "z" z min_dim_z; z, ofsz, incz
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
  vec    (* Y *)
  -> unit = "lacaml_NPRECsub_stub_bc" "lacaml_NPRECsub_stub"

let sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom "Vec.sub" "z" ofsz incz
  and ofsx, incx = get_vec_geom "Vec.sub" "x" ofsx incx
  and ofsy, incy = get_vec_geom "Vec.sub" "y" ofsy incy in
  let n = get_dim_vec "Vec.sub" "x" ofsx incx x "n" n in
  check_vec "Vec.sub" "y" y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec "Vec.sub" "z" z min_dim_z; z, ofsz, incz
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
  vec    (* Y *)
  -> unit = "lacaml_NPRECmul_stub_bc" "lacaml_NPRECmul_stub"

let mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom "Vec.mul" "z" ofsz incz
  and ofsx, incx = get_vec_geom "Vec.mul" "x" ofsx incx
  and ofsy, incy = get_vec_geom "Vec.mul" "y" ofsy incy in
  let n = get_dim_vec "Vec.mul" "x" ofsx incx x "n" n in
  check_vec "Vec.mul" "y" y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec "Vec.mul" "z" z min_dim_z; z, ofsz, incz
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
  vec    (* Y *)
  -> unit = "lacaml_NPRECdiv_stub_bc" "lacaml_NPRECdiv_stub"

let div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y =
  let ofsz, incz = get_vec_geom "Vec.div" "z" ofsz incz
  and ofsx, incx = get_vec_geom "Vec.div" "x" ofsx incx
  and ofsy, incy = get_vec_geom "Vec.div" "y" ofsy incy in
  let n = get_dim_vec "Vec.div" "x" ofsx incx x "n" n in
  check_vec "Vec.div" "y" y (ofsy + (n - 1) * abs incy);
  let z, ofsz, incz =
    let min_dim_z = ofsz + (n - 1) * abs incz in
    match z with
    | Some z -> check_vec "Vec.div" "z" z min_dim_z; z, ofsz, incz
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
  vec    (* Y *)
  -> num_type = "lacaml_NPRECssqr_diff_stub_bc" "lacaml_NPRECssqr_diff_stub"

let ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y =
  let ofsx, incx = get_vec_geom "Vec.ssqr_diff" "x" ofsx incx
  and ofsy, incy = get_vec_geom "Vec.ssqr_diff" "y" ofsy incy in
  let n = get_dim_vec "Vec.ssqr_diff" "x" ofsx incx x "n" n in
  check_vec "Vec.ssqr_diff" "y" y (ofsy + (n - 1) * abs incy);
  direct_ssqr_diff n ofsx incx x ofsy incy y
