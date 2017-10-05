(* File: mat_SDCZ.ml

   Copyright (C) 2002-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

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

open Bigarray
open Lacaml__numberxx
open Lacaml__utils

(* Creation of matrices *)

let create m n = Array2.create prec fortran_layout m n

let make m n x =
  let mat = create m n in
  Array2.fill mat x;
  mat

let make0 m n = make m n zero

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
  for row = 1 to n do mat.{row, 1} <- ar.(row - 1) done;
  mat

let dim1 (mat : mat) = Array2.dim1 mat
let dim2 (mat : mat) = Array2.dim2 mat
let has_zero_dim (mat : mat) = dim1 mat = 0 || dim2 mat = 0

let mvec_to_array mat =
  if dim2 mat <> 1 then failwith "mvec_to_array: more than one column"
  else
    let n = dim1 mat in
    if n = 0 then [||]
    else
      let ar = Array.make n mat.{1, 1} in
      for row = 2 to n do ar.(row - 1) <- mat.{row, 1} done;
      ar

let from_col_vec vec = reshape_2 (genarray_of_array1 vec) (Array1.dim vec) 1
let from_row_vec vec = reshape_2 (genarray_of_array1 vec) 1 (Array1.dim vec)

let empty = create 0 0

let identity n =
  let mat = make n n zero in
  for i = 1 to n do mat.{i, i} <- one done;
  mat

let of_diag ?n ?(br = 1) ?(bc = 1) ?b ?ofsx ?incx (x : vec) =
  let loc = "Lacaml.NPREC.Mat.of_diag" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  let b = get_mat loc b_str make0 br bc b n n in
  let ofsx_ref = ref ofsx in
  for i = 0 to n - 1 do
    b.{br + i, bc + i} <- x.{!ofsx_ref};
    ofsx_ref := !ofsx_ref + incx
  done;
  b

let to_array mat =
  let m = dim1 mat in
  let n = dim2 mat in
  if m = 0 then [||]
  else if n = 0 then Array.make m [||]
  else
    let ar = Array.make_matrix m n mat.{1, 1} in
    for row = 1 to m do
      let row_ar = ar.(row - 1) in
      for col = 1 to n do row_ar.(col - 1) <- mat.{row, col} done;
    done;
    ar

let col (mat : mat) c = Array2.slice_right mat c

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

external direct_copy :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

let of_col_vecs ar =
  let n = Array.length ar in
  if n = 0 then empty
  else
    let m = Array1.dim ar.(0) in
    let mat = create m n in
    for c = 1 to n do
      let vec = ar.(c - 1) in
      if Array1.dim vec <> m then
        failwith "of_col_vecs: vectors not of same length";
      if m > 0 then
        direct_copy ~n:m ~ofsy:1 ~incy:1 ~y:(col mat c) ~ofsx:1 ~incx:1 ~x:vec
    done;
    mat

let to_col_vecs mat =
  let n = dim2 mat in
  if n = 0 then [||]
  else
    let ar = Array.make n (col mat 1) in
    for i = 2 to n do ar.(i - 1) <- col mat i done;
    ar

let of_col_vecs_list = function
  | [] -> empty
  | (vec :: _) as lst ->
      let n = List.length lst in
      let m = Array1.dim vec in
      let mat = create m n in
      let rec loop i = function
        | [] -> mat
        | vec :: t ->
            if Array1.dim vec <> m then
              failwith "of_col_vecs_list: vectors not of same length";
            if m > 0 then
              direct_copy
                ~n:m ~ofsy:1 ~incy:1 ~y:(col mat i) ~ofsx:1 ~incx:1 ~x:vec;
            loop (i + 1) t
      in
      loop 1 lst

let to_col_vecs_list mat =
  let rec loop i acc = if i < 1 then acc else loop (i - 1) (col mat i :: acc) in
  loop (dim2 mat) []

let of_list = function
  | [] -> empty
  | (h :: t) as lst ->
      let m = List.length lst in
      let n = List.length h in
      List.iter (fun l ->
        if List.length l <> n then
          failwith "of_list: vectors not of same length") t;
      let mat = create m n in
      let rec loop_cols i j = function
        | [] -> ()
        | el :: cols -> mat.{i, j} <- el; loop_cols i (j + 1) cols
      in
      let rec loop_rows i = function
        | [] -> mat
        | cols :: rows -> loop_cols i 1 cols; loop_rows (i + 1) rows
      in
      loop_rows 1 lst

let to_list mat =
  let m = dim1 mat in
  let n = dim2 mat in
  let row_to_list r =
    let rec l j a = if j < 1 then a else l (j - 1) (mat.{r, j} :: a) in
    l n []
  in
  let rec loop i acc =
    if i < 1 then acc
    else loop (i - 1) (row_to_list i :: acc)
  in
  loop m []

let as_vec mat =
  let gen = genarray_of_array2 mat in
  reshape_1 gen (dim1 mat * dim2 mat)

external direct_swap :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_NPRECswap_mat_stub_bc" "lacaml_NPRECswap_mat_stub"

let swap ?patt ?m ?n ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.swap" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_swap ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b

external direct_transpose_copy :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_NPRECtranspose_copy_stub_bc" "lacaml_NPRECtranspose_copy_stub"

let transpose_copy ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.transpose_copy" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b n m in
  direct_transpose_copy ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

let detri ?(up = true) ?n ?(ar = 1) ?(ac = 1) (a : mat) =
  let loc = "Lacaml.NPREC.Mat.detri" in
  let n = get_n_of_square loc a_str ar ac a n in
  if up then
    for c = 1 to n - 1 do
      let ar_c = ar + c in
      let ac_c = ac + c in
      for r = 0 to c - 1 do a.{ar_c, ac + r} <- a.{ar + r, ac_c} done
    done
  else
    for c = 1 to n - 1 do
      let ar_c = ar + c in
      let ac_c = ac + c in
      for r = 0 to c - 1 do a.{ar + r, ac_c} <- a.{ar_c, ac + r} done
    done

let packed ?(up = true) ?n ?(ar = 1) ?(ac = 1) (a : mat) =
  let loc = "Lacaml.NPREC.Mat.packed" in
  let n = get_n_of_square loc a_str ar ac a n in
  let dst = Array1.create prec fortran_layout ((n * n + n) / 2) in
  let pos_ref = ref 1 in
  if up then
    for c = 1 to n do
      for r = 1 to c do
        let pos = !pos_ref in
        dst.{pos} <- a.{r, c};
        pos_ref := pos + 1;
      done
    done
  else
    for c = 1 to n do
      for r = c to n do
        let pos = !pos_ref in
        dst.{pos} <- a.{r, c};
        pos_ref := pos + 1;
      done
    done;
  dst

let unpacked ?(up = true) ?n (src : vec) =
  let loc = "Lacaml.NPREC.Mat.unpacked" in
  let n = get_unpacked_dim loc ?n (Array1.dim src) in
  let a = make0 n n in
  let pos_ref = ref 1 in
  if up then
    for c = 1 to n do
      for r = 1 to c do
        let pos = !pos_ref in
        a.{r, c} <- src.{pos};
        pos_ref := pos + 1;
      done
    done
  else
    for c = 1 to n do
      for r = c to n do
        let pos = !pos_ref in
        a.{r, c} <- src.{pos};
        pos_ref := pos + 1;
      done
    done;
  a

let copy_diag ?n ?(ofsy = 1) ?(incy = 1) ?y ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.copy_diag" in
  let n1 = get_dim1_mat loc a_str a ar n_str n in
  let n2 = get_dim2_mat loc a_str a ac n_str n in
  let n_diag = min n1 n2 in
  let y = get_vec loc y_str y ofsy incy n_diag vec_create in
  let ofsy_ref = ref ofsy in
  for i = 0 to n_diag - 1 do
    y.{!ofsy_ref} <- a.{ar + i, ac + i};
    ofsy_ref := !ofsy_ref + incy
  done;
  y

let trace mat =
  let m = dim1 mat in
  let n = dim2 mat in
  let n_diag = min m n in
  let rec loop i trace =
    if i = 0 then trace
    else loop (i - 1) (add trace mat.{i, i})
  in
  loop n_diag zero

external direct_scal_mat :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  alpha : num_type ->
  ar : int ->
  ac : int ->
  a : mat ->
  unit = "lacaml_NPRECscal_mat_stub_bc" "lacaml_NPRECscal_mat_stub"

let scal ?patt ?m ?n alpha ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.scal" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_scal_mat ~pkind ~pinit ~m ~n ~alpha ~ar ~ac ~a

external direct_scal_cols :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ofs : int ->
  alphas : vec ->
  unit = "lacaml_NPRECscal_cols_stub_bc" "lacaml_NPRECscal_cols_stub"

let scal_cols ?patt ?m ?n ?(ar = 1) ?(ac = 1) a ?ofs alphas =
  let loc = "Lacaml.NPREC.Mat.scal_cols" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let ofs = get_vec_ofs loc alphas_str ofs in
  ignore (get_dim_vec loc alphas_str ofs 1 alphas n_str (Some n));
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_scal_cols ~pkind ~pinit ~m ~n ~ar ~ac ~a ~ofs ~alphas

external direct_scal_rows :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ofs : int ->
  alphas : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  unit = "lacaml_NPRECscal_rows_stub_bc" "lacaml_NPRECscal_rows_stub"

let scal_rows ?patt ?m ?n ?ofs alphas ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.scal_rows" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let ofs = get_vec_ofs loc alphas_str ofs in
  ignore (get_dim_vec loc alphas_str ofs 1 alphas n_str (Some m));
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_scal_rows ~pkind ~pinit ~m ~n ~ofs ~alphas ~ar ~ac ~a

let vec_create n = Array1.create prec fortran_layout n

external direct_syrk_trace :
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  num_type = "lacaml_NPRECsyrk_trace_stub"

let syrk_trace ?n ?k ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.syrk_trace" in
  let n = get_dim1_mat loc a_str a ar n_str n in
  let k = get_dim2_mat loc a_str a ac k_str k in
  direct_syrk_trace ~n ~k ~ar ~ac ~a


(* Operations on one matrix *)

external direct_fill :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  x : num_type ->
  unit = "lacaml_NPRECfill_mat_stub_bc" "lacaml_NPRECfill_mat_stub"

let fill ?patt ?m ?n ?(ar = 1) ?(ac = 1) a x =
  let loc = "Lacaml.NPREC.Mat.fill" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_fill ~pkind ~pinit ~m ~n ~ar ~ac ~a ~x

external direct_sum :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  num_type = "lacaml_NPRECsum_mat_stub_bc" "lacaml_NPRECsum_mat_stub"

let sum ?patt ?m ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.sum" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_sum ~pkind ~pinit ~m ~n ~ar ~ac ~a

external direct_add_const :
  c : num_type ->
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_NPRECadd_const_mat_stub_bc" "lacaml_NPRECadd_const_mat_stub"

let add_const c ?patt ?m ?n
      ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.add_const" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let b = get_mat loc b_str create br bc b m n in
  direct_add_const ~c ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_neg :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_NPRECneg_mat_stub_bc" "lacaml_NPRECneg_mat_stub"

let neg ?patt ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.neg" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let b = get_mat loc b_str create br bc b m n in
  direct_neg ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_reci :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_NPRECreci_mat_stub_bc" "lacaml_NPRECreci_mat_stub"

let reci ?patt ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.reci" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let b = get_mat loc b_str create br bc b m n in
  direct_reci ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

external direct_syrk_diag :
  trans : char ->
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ofsy : int ->
  y : vec ->
  alpha : num_type ->
  beta : num_type ->
  unit = "lacaml_NPRECsyrk_diag_stub_bc" "lacaml_NPRECsyrk_diag_stub"

let syrk_diag ?n ?k ?(beta = zero) ?(ofsy = 1) ?y
  ?(trans = `N) ?(alpha = one) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.Mat.syrk_diag" in
  let n = get_rows_mat_tr loc a_str a ar ac trans n_str n in
  let k = get_cols_mat_tr loc a_str a ar ac trans k_str k in
  let y = get_vec loc y_str y ofsy 1 n vec_create in
  let trans = get_trans_char trans in
  direct_syrk_diag ~trans ~n ~k ~ar ~ac ~a ~ofsy ~y ~alpha ~beta;
  y


(* Operations on two matrices *)

external direct_mat_add :
  pkind : Mat_patt.kind ->
  pinit : int ->
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
  unit = "lacaml_NPRECadd_mat_stub_bc" "lacaml_NPRECadd_mat_stub"

let add ?patt ?m ?n
      ?(cr = 1) ?(cc = 1) ?c ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.add" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let c = get_mat loc c_str create cr cc c m n in
  direct_mat_add ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c;
  c

external direct_mat_sub :
  pkind : Mat_patt.kind ->
  pinit : int ->
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
  unit = "lacaml_NPRECsub_mat_stub_bc" "lacaml_NPRECsub_mat_stub"

let sub ?patt ?m ?n
      ?(cr = 1) ?(cc = 1) ?c ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.sub" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let c = get_mat loc c_str create cr cc c m n in
  direct_mat_sub ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c;
  c

external direct_mat_mul :
  pkind : Mat_patt.kind ->
  pinit : int ->
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
  unit = "lacaml_NPRECmul_mat_stub_bc" "lacaml_NPRECmul_mat_stub"

let mul ?patt ?m ?n
      ?(cr = 1) ?(cc = 1) ?c ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.mul" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let c = get_mat loc c_str create cr cc c m n in
  direct_mat_mul ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c;
  c

external direct_mat_div :
  pkind : Mat_patt.kind ->
  pinit : int ->
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
  unit = "lacaml_NPRECdiv_mat_stub_bc" "lacaml_NPRECdiv_mat_stub"

let div ?patt ?m ?n
      ?(cr = 1) ?(cc = 1) ?c ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.div" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  let c = get_mat loc c_str create cr cc c m n in
  direct_mat_div ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c;
  c

external direct_axpy_mat :
  alpha : num_type ->
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  xr : int ->
  xc : int ->
  x : mat ->
  yr : int ->
  yc : int ->
  y : mat ->
  unit = "lacaml_NPRECaxpy_mat_stub_bc" "lacaml_NPRECaxpy_mat_stub"

let axpy ?(alpha = one) ?patt ?m ?n
      ?(xr = 1) ?(xc = 1) x ?(yr = 1) ?(yc = 1) y =
  let loc = "Lacaml.NPREC.Mat.axpy" in
  let m = get_dim1_mat loc x_str x xr m_str m in
  let n = get_dim2_mat loc x_str x xc n_str n in
  check_dim_mat loc y_str yr yc y m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_axpy_mat ~alpha ~pkind ~pinit ~m ~n ~xr ~xc ~x ~yr ~yc ~y

external direct_gemm_diag :
  transa : char ->
  transb : char ->
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  ofsy : int ->
  y : vec ->
  alpha : num_type ->
  beta : num_type ->
  unit = "lacaml_NPRECgemm_diag_stub_bc" "lacaml_NPRECgemm_diag_stub"

let gemm_diag ?n ?k ?(beta = zero) ?(ofsy = 1) ?y
  ?(transa = `N) ?(alpha = one) ?(ar = 1) ?(ac = 1) a
  ?(transb = `N) ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.gemm_diag" in
  let n = get_rows_mat_tr loc a_str a ar ac transa n_str n in
  let n = get_cols_mat_tr loc b_str b br bc transb n_str (Some n) in
  let k = get_inner_dim loc a_str a ar ac transa b_str b br bc transb k_str k in
  let transa = get_trans_char transa in
  let transb = get_trans_char transb in
  let y = get_vec loc y_str y ofsy 1 n vec_create in
  direct_gemm_diag
    ~transa ~transb ~n ~k ~ar ~ac ~a ~br ~bc ~b ~ofsy ~y ~alpha ~beta;
  y

external direct_gemm_trace :
  transa : char ->
  transb : char ->
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  num_type = "lacaml_NPRECgemm_trace_stub_bc" "lacaml_NPRECgemm_trace_stub"

let gemm_trace ?n ?k ?(transa = `N) ?(ar = 1) ?(ac = 1) a
  ?(transb = `N) ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.gemm_trace" in
  let n = get_rows_mat_tr loc a_str a ar ac transa n_str n in
  let n = get_cols_mat_tr loc b_str b br bc transb n_str (Some n) in
  let k = get_inner_dim loc a_str a ar ac transa b_str b br bc transb k_str k in
  let transa = get_trans_char transa in
  let transb = get_trans_char transb in
  direct_gemm_trace ~transa ~transb ~n ~k ~ar ~ac ~a ~br ~bc ~b

external direct_symm2_trace :
  n : int ->
  uploa : char ->
  ar : int ->
  ac : int ->
  a : mat ->
  uplob : char ->
  br : int ->
  bc : int ->
  b : mat ->
  num_type = "lacaml_NPRECsymm2_trace_stub_bc" "lacaml_NPRECsymm2_trace_stub"

let symm2_trace
  ?n
  ?(upa = true) ?(ar = 1) ?(ac = 1) a
  ?(upb = true) ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.symm2_trace" in
  let n = get_n_of_square loc a_str ar ac a n in
  let n = get_n_of_square loc b_str br bc b (Some n) in
  let uploa = get_uplo_char upa in
  let uplob = get_uplo_char upb in
  direct_symm2_trace ~n ~uploa ~ar ~ac ~a ~uplob ~br ~bc ~b

external direct_ssqr_diff :
  pkind : Mat_patt.kind ->
  pinit : int ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  num_type
  = "lacaml_NPRECssqr_diff_mat_stub_bc" "lacaml_NPRECssqr_diff_mat_stub"

let ssqr_diff ?patt ?m ?n ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.Mat.ssqr_diff" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  check_dim_mat loc b_str br bc b m n;
  let pkind, pinit = Mat_patt.normalize_args ~loc ~m ~n patt in
  direct_ssqr_diff ~pkind ~pinit ~m ~n ~ar ~ac ~a ~br ~bc ~b


(* Iterators over matrices *)

let map f ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) (a : mat) =
  let loc = "Lacaml.NPREC.Mat.map" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b = get_mat loc b_str create br bc b m n in
  let max_row = m - 1 in
  for i = 0 to n - 1 do
    for j = 0 to max_row do b.{br + j, bc + i} <- f a.{ar + j, ac + i} done;
  done;
  b

let fold_cols coll ?n ?(ac = 1) acc a =
  let loc = "Lacaml.NPREC.Mat.fold_cols" in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let rec loop i acc =
    if i = n then acc
    else loop (i + 1) (coll acc (col a (ac + i)))
  in
  loop 0 acc
