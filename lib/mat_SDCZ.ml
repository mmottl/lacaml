(* File: mat_SDCZ.ml

   Copyright (C) 2002-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
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
open Lacaml_numberxx
open Lacaml_utils

(* Creation of matrices *)

let create m n = Array2.create prec fortran_layout m n

let make m n x =
  let mat = create m n in
  Array2.fill mat x;
  mat

let make0 m n = make m n zero

external direct_copy :
  int -> (* N *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

let copy ?m ?n ?(yr = 1) ?(yc = 1) ?y ?(xr = 1) ?(xc = 1) x =
  let loc = "Mat.copy" in
  let x_name = "x" in
  let m = get_dim1_mat loc x_name x xr "xr" m in
  let n = get_dim2_mat loc x_name x xc "xc" n in
  let y, yr, yc =
    match y with
    | Some y ->
        let y_name = "y" in
        check_dim1_mat loc y_name y yr "yr" m;
        check_dim2_mat loc y_name y yc "yc" n;
        y, yr, yc
    | None -> create m n, 1, 1 in
  for i = 0 to n - 1 do
    let src = Array2.slice_right x (xc + i) in
    let dst = Array2.slice_right y (yc + i) in
    direct_copy m yr 1 dst xr 1 src
  done;
  y

let of_array ar = Array2.of_array prec fortran_layout ar

let init_rows m n f =
  let mat = create m n in
  for row = 1 to m do
    for col = 1 to n do
      mat.{row, col} <- f row col
    done
  done;
  mat

let init_cols m n f =
  let mat = create m n in
  for col = 1 to n do
    for row = 1 to m do
      mat.{row, col} <- f row col
    done
  done;
  mat

let create_mvec m = create m 1

let make_mvec m x = make m 1 x

let mvec_of_array ar =
  let n = Array.length ar in
  let mat = create_mvec n in
  if n = 0 then mat
  else
    let vec = Array2.slice_right mat 1 in
    for row = 1 to n do
      vec.{row} <- ar.(row - 1)
    done;
    mat

let mvec_to_array mat =
  let n = Array2.dim1 mat in
  if n = 0 then [||]
  else
    let vec = Array2.slice_right mat 1 in
    let ar = Array.make n vec.{1} in
    for row = 2 to n do
      ar.(row - 1) <- vec.{row}
    done;
    ar

let from_col_vec vec = reshape_2 (genarray_of_array1 vec) (Array1.dim vec) 1
let from_row_vec vec = reshape_2 (genarray_of_array1 vec) 1 (Array1.dim vec)

let empty = create 0 0

let identity n =
  let mat = make n n zero in
  for i = 1 to n do
    mat.{i, i} <- one
  done;
  mat

let of_diag vec =
  let n = Array1.dim vec in
  let mat = make n n zero in
  for i = 1 to n do mat.{i, i} <- vec.{i} done;
  mat

let dim1 m = Array2.dim1 m

let dim2 m = Array2.dim2 m

let to_array mat =
  let d1 = dim1 mat in
  let d2 = dim2 mat in
  if d1 = 0 then [||]
  else if d2 = 0 then
    Array.make d1 [||]
  else
    let ar = Array.make_matrix d1 d2 mat.{1, 1}in
    for col = 1 to d2 do
      let vec = Array2.slice_right mat col in
      let col_1 = col - 1 in
      for row = 1 to d1 do
        ar.(row - 1).(col_1) <- vec.{row}
      done;
    done;
    ar

let diag mat =
  let m = dim1 mat in
  let n = dim2 mat in
  let n_diag = min m n in
  let vec = Array1.create prec fortran_layout n_diag in
  for i = 1 to n_diag do vec.{i} <- mat.{i, i} done;
  vec

let col mat c = Array2.slice_right mat c

let copy_row ?vec mat r =
  let n = dim2 mat in
  let vec =
    match vec with
    | Some vec ->
        if Array1.dim vec < n then
          failwith ("copy_row: dim(vec) < " ^ string_of_int n);
        vec
    | None -> Array1.create prec fortran_layout n in
  for c = 1 to n do vec.{c} <- mat.{r, c} done;
  vec

let of_col_vecs ar =
  let n = Array.length ar in
  if n = 0 then create 0 0
  else
    let m = Array1.dim ar.(0) in
    let mat = create m n in
    for c = 1 to n do
      let vec = ar.(c - 1) in
      if Array1.dim vec <> m then
        failwith "of_col_vecs: vectors not of same length";
      direct_copy m 1 1 (col mat c) 1 1 vec
    done;
    mat

let to_col_vecs mat =
  let dim2 = dim2 mat in
  if dim2 = 0 then [||]
  else
    let ar = Array.make dim2 (col mat 1) in
    for i = 2 to dim2 do
      ar.(i - 1) <- col mat i
    done;
    ar

let transpose mat =
  let rows = dim1 mat in
  let cols = dim2 mat in
  let res = create cols rows in
  for col = 1 to cols do
    let vec = Array2.slice_right mat col in
    for row = 1 to rows do
      res.{col, row} <- vec.{row}
    done
  done;
  res

external direct_map :
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat -> (* B *)
  (num_type -> num_type)
  -> unit = "lacaml_NPRECmap_stub_bc" "lacaml_NPRECmap_stub"

let map f ?m ?n ?(cr = 1) ?(cc = 1) ?c ?(ar = 1) ?(ac = 1) a =
  let loc = "Mat.map" in
  let m = get_dim1_mat loc "a" a ar "m" m in
  let n = get_dim2_mat loc "a" a ac "n" n in
  let c, cr, cc =
    match c with
    | None -> create m n, 1, 1
    | Some c ->
        check_dim1_mat loc "c" c cr "m" m;
        check_dim2_mat loc "c" c cc "n" n;
        c, cr, cc in
  direct_map m n ar ac a cr cc c f;
  c

let fold_cols coll ?n ?(ac = 1) acc a =
  let loc = "Mat.fold_cols" in
  let n = get_dim2_mat loc "a" a ac "n" n in
  let acc_ref = ref acc in
  for i = 0 to n - 1 do
    let col = Array2.slice_right a (ac + i) in
    acc_ref := coll !acc_ref col
  done;
  !acc_ref
