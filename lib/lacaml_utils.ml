(* File: lacaml_utils.ml

   Copyright (C) 2001-2005

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

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

(* $Id: lacaml_utils.ml,v 1.24 2006/01/18 15:03:40 mottl Exp $ *)

(** General auxiliary functions *)

open Printf
open Bigarray
open Lacaml_common

(* Zero-sized dummy vector (int) *)
let empty_int_vec = create_int_vec 0

(* Char indicating type of norm to retrieve for XlanYY routines *)
let get_norm_char = function `M -> 'M' | `O -> 'O' | `I -> 'I' | `F -> 'F'

(* Char indicating whether the "U"pper or "L"ower triangle of a matrix
   is stored *)
let get_uplo_char up = if up then 'U' else 'L'

(* Char indicating whether some operation operates on a "N"ormal,
   "T"ransposed or "C"onjugated transposed matrix. *)
let get_trans_char = function `N -> 'N' | `T -> 'T' | `C -> 'C'

(* Char indicating which side of the matrix B matrix A should be on *)
let get_side_char left = if left then 'L' else 'R'

(* Char indicating whether/how the left/right singular vectors
   should be computed *)
let get_s_d_job_char = function `A -> 'A' | `S -> 'S' | `O -> 'O' | `N -> 'N'

(* Char indicating whether the eigen"V"ectors are computed or "N"ot *)
let get_job_char = function true -> 'V' | _ -> 'N'
let job_char_true = get_job_char true
let job_char_false = get_job_char false

(* Get a work array *)
let get_work loc vec_create work min_lwork opt_lwork lwork_str =
  match work with
  | Some work ->
      let lwork = Array1.dim work in
      if lwork < min_lwork then
        invalid_arg (
          sprintf "%s: %s: valid=[%d..[ got=%d" loc lwork_str min_lwork lwork)
      else work, lwork
  | None -> vec_create opt_lwork, opt_lwork

(* Fetches problem-dependent parameters for LAPACK-functions *)
external ilaenv : int -> string -> string -> int -> int -> int -> int -> int
  = "lacaml_ilaenv_stub_bc" "lacaml_ilaenv_stub" "noalloc"

let check_var_ltz loc var_name var =
  if var < 0 then invalid_arg (sprintf "%s: %s < 0" loc var_name)

let check_vec loc vec_name vec min_dim =
  let dim = Array1.dim vec in
  if dim < min_dim then
    invalid_arg (sprintf "%s: dim(%s): valid=[%d..[ got=%d"
                   loc vec_name min_dim dim)

let get_vec loc vec_name vec ofs inc min_elem vec_create =
  let min_dim = ofs + (min_elem - 1) * abs inc in
  match vec with
  | Some vec -> check_vec loc vec_name vec min_dim; vec
  | _ -> vec_create min_dim

let check_dim1_mat loc mat_name mat mat_r m_name m =
  let dim1 = Array2.dim1 mat in
  let dim1_rest = dim1 - mat_r + 1 in
  if mat_r < 1 || dim1_rest < 1 then
    invalid_arg (
      sprintf "%s: mat_r(%s): valid=[1..%d] got=%d" loc mat_name dim1 mat_r);
  if m < 0 || dim1_rest < m then
    invalid_arg (
      sprintf "%s: %s(%s): valid=[0..%d] got=%d"
        loc m_name mat_name dim1_rest m)

let check_dim2_mat loc mat_name mat mat_c n_name n =
  let dim2 = Array2.dim2 mat in
  let dim2_rest = dim2 - mat_c + 1 in
  if mat_c < 1 || dim2_rest < 1 then
    invalid_arg (
      sprintf "%s: mat_c(%s): valid=[1..%d] got=%d" loc mat_name dim2 mat_c);
  if n < 0 || dim2_rest < n then
    invalid_arg (
      sprintf "%s: %s(%s): valid=[0..%d] got=%d"
        loc n_name mat_name dim2_rest n)

let get_mat loc mat_name mat_create r c mat m n =
  match mat with
  | Some mat ->
      check_dim1_mat loc mat_name mat r "m" m;
      check_dim2_mat loc mat_name mat c "n" n;
      mat
  | None -> mat_create m n

(* ??MV auxiliary functions *)

(* [get_dim_vec loc ofsvec incvec vec n_name n] if the dimension [n]
   is given, check that the vector [vec] is big enough, otherwise
   return the maximal [n] for the given vector [vec]. *)
let get_dim_vec loc vec_name ofsvec incvec vec n_name = function
  | Some n ->
      if n < 0 then invalid_arg (sprintf "%s: %s < 0" loc n_name);
      check_vec loc vec_name vec (ofsvec + (n - 1) * abs incvec);
      n
  | None -> (Array1.dim vec - ofsvec) / abs incvec + 1

let get_dim1_mat loc mat_name mat mat_r m_name m =
  let dim1 = Array2.dim1 mat in
  let dim1_rest = dim1 - mat_r + 1 in
  if mat_r < 1 || dim1_rest < 1 then
    invalid_arg (
      sprintf "%s: mat_r(%s): valid=[1..%d] got=%d" loc mat_name dim1 mat_r);
  match m with
  | Some m ->
      if m < 0 || dim1_rest < m then
        invalid_arg (
          sprintf "%s: %s(%s): valid=[0..%d] got=%d"
            loc m_name mat_name dim1_rest m)
      else m
  | None -> dim1_rest

let get_dim2_mat loc mat_name mat mat_c n_name n =
  let dim2 = Array2.dim2 mat in
  let dim2_rest = dim2 - mat_c + 1 in
  if mat_c < 1 || dim2_rest < 1 then
    invalid_arg (
      sprintf "%s: mat_c(%s): valid=[1..%d] got=%d" loc mat_name dim2 mat_c);
  match n with
  | Some n ->
      if n < 0 || dim2_rest < n then
        invalid_arg (
          sprintf "%s: %s(%s): valid=[0..%d] got=%d"
            loc n_name mat_name dim2_rest n)
      else n
  | None -> dim2_rest

(* A symmetric band (SB) matrix has physical size [k+1]*[n] for a
   logical matrix of size [n]*[n].  Check and return the [k] (possibly
   also given by the optional argument [k]). *)
let get_k_mat_sb loc mat_name mat mat_r k_name k =
  let dim1 = Array2.dim1 mat in
  let max_k = dim1 - mat_r in
  if mat_r < 1 || max_k < 0 then
    invalid_arg (
      sprintf "%s: mat_r(%s): valid=[1..%d] got=%d" loc mat_name dim1 mat_r);
  match k with
  | None -> max_k
  | Some k ->
      if k < 0 || max_k < k then
        invalid_arg (
          sprintf "%s: %s(%s): valid=[0..%d] got=%d"
            loc k_name mat_name max_k k)
      else k

let get_dim_mat_packed loc mat_name ofsmat mat n_name n =
  let dim = Array1.dim mat in
  match n with
  | Some n ->
      let n1 = ofsmat + (n - 1)*(n + 2)/2 (* ?overflow? *) in
      if n < 0 || dim < n1 then
        invalid_arg (sprintf "%s: %s(%s): valid=[0..%d] got=%d"
                       loc n_name mat_name dim n1)
      else n
  | None -> (* the greater n s.t. ofsmat - 1 + n(n+1)/2 <= dim mat *)
      max 0 (truncate((sqrt(9. +. 8. *. float(dim - ofsmat)) -. 1.) /. 2.))

let get_inc loc var = function
  | Some inc ->
      if inc = 0 then invalid_arg (sprintf "%s: inc%s = 0" loc var);
      inc
  | None -> 1

let get_ofs loc var = function
  | Some ofs ->
      if ofs < 1 then invalid_arg (sprintf "%s: ofs%s < 1" loc var);
      ofs
  | None -> 1

let get_vec_geom loc var ofs inc = get_ofs loc var ofs, get_inc loc var inc

(* Makes sure that [mat] is a square matrix and [n] is within range *)
let get_n_of_square mat_name loc r c mat n =
  let n_str = "n" in
  let n = get_dim2_mat loc mat_name mat c n_str n in
  check_dim1_mat loc mat_name mat r n_str n;
  n

let get_n_of_a loc ar ac a n = get_n_of_square "a" loc ar ac a n

let get_nrhs_of_b loc n br bc b nrhs =
  let b_str = "b" in
  let nrhs = get_dim2_mat loc b_str b bc "nrhs" nrhs in
  check_dim1_mat loc b_str b br "n" n;
  nrhs


(* GELS? - Auxiliary Functions *)

let gelsX_err loc gelsX_min_work ar a m n lwork nrhs br b err =
  if err > 0 then
    failwith
      (sprintf "%s: failed to converge on off-diagonal element %d" loc err)
  else
    let msg =
      match err with
      | -1 -> sprintf "m: valid=[0..[ got=%d" m
      | -2 -> sprintf "n: valid=[0..[ got=%d" n
      | -3 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
      | -5 ->
          sprintf "dim1(a): valid=[%d..[ got=%d"
            (max 1 m + ar - 1) (Array2.dim1 a)
      | -7 ->
          let min_dim = max 1 (max m n) + br - 1 in
          sprintf "dim1(b): valid=[%d..[ got=%d" min_dim (Array2.dim1 b)
      | -12 ->
          let min_lwork = gelsX_min_work ~m ~n ~nrhs in
          sprintf "lwork: valid=[%d..[ got=%d" min_lwork lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let gelsX_get_s vec_create loc min_dim ofss = function
  | Some s ->
      let dim_s = Array1.dim s in
      let min_dim_ofs = ofss - 1 + min_dim in
      if dim_s < min_dim_ofs then
        invalid_arg (sprintf "%s: s: valid=[%d..[ got=%d" loc min_dim_ofs dim_s)
      else s
  | None -> vec_create min_dim

let gelsX_get_params loc ar ac a m n nrhs br bc b =
  let m = get_dim1_mat loc "a" a ar "m" m in
  let n = get_dim2_mat loc "a" a ac "n" n in
  let nrhs = get_dim2_mat loc "b" b bc "nrhs" nrhs in
  check_dim1_mat loc "b" b br "m" (max m n);
  m, n, nrhs


(* ??ev -- auxiliary functions *)

let xxev_get_wx vec_create loc wname ofsw w n =
  match w with
  | None -> 1, vec_create n
  | Some w -> check_vec loc wname w (ofsw - 1 + n); ofsw, w


(* geev -- auxiliary functions *)

let geev_get_job_side loc mat_empty mat_create mat_name n r c = function
  | None -> 1, 1, mat_create n n, job_char_true, true
  | Some None -> 1, 1, mat_empty, job_char_false, false
  | Some (Some mat) ->
      let n_str = "n" in
      check_dim1_mat loc mat_name mat r n_str n;
      check_dim2_mat loc mat_name mat c n_str n;
      r, c, mat, job_char_true, true

let geev_gen_get_params loc mat_empty mat_create ar ac a n
    leftr leftc left rightr rightc right =
  let n = get_n_of_a loc ar ac a n in
  let leftr, leftc, vl, jobvl, lvs =
    geev_get_job_side loc mat_empty mat_create "lv" n leftr leftc left in
  let rightr, rightc, vr, jobvr, rvs =
    geev_get_job_side loc mat_empty mat_create "rv" n rightr rightc right in
  n, leftr, leftc, vl, jobvl, rightr, rightc, vr, jobvr, lvs || rvs


(* g?mv -- auxiliary functions *)

let gXmv_get_params loc vec_create ar ac a m n ofsx incx x ofsy incy y trans =
  let a_str = "a" in
  let x_str = "x" in
  let m = get_dim1_mat loc a_str a ar "m" m in
  let n = get_dim2_mat loc a_str a ac "n" n in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc "y" ofsy incy in
  let lx, ly, trans_char =
    let trans_char = get_trans_char trans in
    if trans = `N then n, m, trans_char else m, n, trans_char in
  check_vec loc x_str x (ofsx + (lx - 1) * abs incx);
  let y = get_vec loc "y" y ofsy incy ly vec_create in
  m, n, ofsx, incx, ofsy, incy, y, trans_char

(* gemm -- auxiliary functions *)

let get_c loc mat_create cr cc c m n = get_mat loc "c" mat_create cr cc c m n

let get_rows_mat_tr loc mat_str mat mat_r mat_c transp dim_str dim =
  match transp with
  | `N -> get_dim1_mat loc mat_str mat mat_r dim_str dim
  | _  -> get_dim2_mat loc mat_str mat mat_c dim_str dim

let get_cols_mat_tr loc mat_str mat mat_r mat_c transp dim_str dim =
  match transp with
  | `N -> get_dim2_mat loc mat_str mat mat_c dim_str dim
  | _  -> get_dim1_mat loc mat_str mat mat_r dim_str dim

let get_inner_dim loc mat1_str mat1 mat1_r mat1_c tr1
      mat2_str mat2 mat2_r mat2_c tr2 dim_str k =
  let k1 = get_cols_mat_tr loc mat1_str mat1 mat1_r mat1_c tr1 dim_str k in
  let k2 = get_rows_mat_tr loc mat2_str mat2 mat2_r mat2_c tr2 dim_str k in
  if k = None && k1 <> k2 then
    failwith (
      sprintf "%s: inner dimensions of matrices do not match (%d,%d)"
        loc k1 k2)
  else k1

let gemm_get_params loc mat_create ar ac a tra br bc b cr trb cc c m n k =
  let a_str = "a" in
  let b_str = "b" in
  let m = get_rows_mat_tr loc a_str a ar ac tra "m" m in
  let n = get_cols_mat_tr loc b_str b br bc trb "n" n in
  let k = get_inner_dim loc a_str a ar ac tra b_str b br bc trb "k" k in
  let tra_char, trb_char = get_trans_char tra, get_trans_char trb in
  let c = get_c loc mat_create cr cc c m n in
  m, n, k, tra_char, trb_char, c

(* symm -- auxiliary functions *)

let check_mat_square loc mat_str mat mat_r mat_c n =
  let n_str = "n" in
  check_dim1_mat loc mat_str mat mat_r n_str n;
  check_dim2_mat loc mat_str mat mat_c n_str n

let symm_get_params loc mat_create ar ac a br bc b cr cc c m n left up =
  let a_str = "a" in
  let b_str = "b" in
  let m = get_dim1_mat loc b_str b br "m" m in
  let n = get_dim2_mat loc b_str b bc "n" n in
  if left then check_mat_square loc a_str a ar ac m
  else check_mat_square loc a_str a ar ac n;
  let side_char = get_side_char left in
  let uplo_char = get_uplo_char up in
  let c = get_c loc mat_create cr cc c m n in
  m, n, side_char, uplo_char, c

(* syrk -- auxiliary functions *)

let syrk_get_params loc mat_create ar ac a cr cc c n k up trans =
  let a_str = "a" in
  let n = get_rows_mat_tr loc a_str a ar ac trans "n" n in
  let k = get_cols_mat_tr loc a_str a ar ac trans "k" k in
  let trans_char = get_trans_char trans in
  let uplo_char = get_uplo_char up in
  let c = get_c loc mat_create cr cc c n n in
  n, k, uplo_char, trans_char, c

(* ?lange -- auxiliary functions *)

let xlange_get_params loc m n ar ac a =
  let m = get_dim1_mat loc "a" a ar "m" m in
  let n = get_dim2_mat loc "a" a ac "n" n in
  m, n

(* ?lansy -- auxiliary functions *)

let xlansy_get_params loc n ar ac a =
  get_n_of_a loc ar ac a n

(* ??trf -- auxiliary functions *)

let xxtrf_get_params loc ar ac a n =
  get_n_of_a loc ar ac a n

(* ??trs -- auxiliary functions *)

let xxtrs_get_params loc ar ac a n br bc b nrhs =
  let n = get_n_of_a loc ar ac a n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  n, nrhs

let xxtrs_err loc n nrhs a b err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -3 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
    | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | -8 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* ??tri -- auxiliary functions *)

let xxtri_lu_err loc err =
  failwith (sprintf "%s: U(%i,%i)=0 in the LU factorization" loc err err)

let xxtri_get_params loc n ar ac a =
    get_dim2_mat loc "a" a ac "n" n

let xxtri_err loc n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* ??con -- auxiliary functions *)

let xxcon_get_params loc n ar ac a = get_dim2_mat loc "a" a ac "n" n

let xxcon_err loc n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* getrf -- auxiliary functions *)

let getrf_err loc m n a err =
  let msg =
    match err with
    | -1 -> sprintf "n: valid=[0..[ got=%d" n
    | -2 -> sprintf "m: valid=[0..[ got=%d" m
    | -4 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 m) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

let getrf_lu_err loc err =
  failwith (sprintf "%s: U(%i,%i)=0 in the LU factorization" loc err err)

let getrf_get_ipiv loc ipiv m n =
  match ipiv with
  | None -> create_int_vec (min m n)
  | Some ipiv ->
      check_vec loc "ipiv" ipiv (min m n);
      ipiv

let getrf_get_params loc m n ar ac a =
  let m = get_dim1_mat loc "a" a ar "m" m in
  let n = get_dim2_mat loc "a" a ac "n" n in
  m, n

(* sytrf -- auxiliary functions *)

let sytrf_get_ipiv loc ipiv n =
  match ipiv with
  | None -> create_int_vec n
  | Some ipiv ->
      check_vec loc "ipiv" ipiv n;
      ipiv

let sytrf_err loc n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

let sytrf_fact_err loc err =
  failwith (sprintf "%s: D(%i,%i)=0 in the factorization" loc err err)

(* potrf -- auxiliary functions *)

let potrf_chol_err loc err =
  failwith (
    sprintf "%s: leading minor of order %d is not positive definite" loc err)

let potrf_err loc uplo n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | _ -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* potrs -- auxiliary functions *)

let potrs_err loc n nrhs a b err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -3 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
    | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | -7 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* getri -- auxiliary functions *)

let getri_err loc getri_min_lwork n a lwork err =
  let msg =
    match err with
    | -1 -> sprintf "n: valid=[0..[ got=%d" n
    | -3 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | -6 ->
        let min_lwork = getri_min_lwork n in
        sprintf "lwork: valid=[%d..[ got=%d" min_lwork lwork
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* sytri -- auxiliary functions *)

let sytri_fact_err loc err =
  failwith (sprintf "%s: D(%i,%i)=0 in the factorization" loc err err)

(* potri -- auxiliary functions *)

let potri_err loc n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

(* gecon -- auxiliary functions *)

let gecon_err loc norm_char n a err =
  let msg =
    match err with
    | -1 -> sprintf "norm: valid=['O', I'] got='%c'" norm_char
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)


(* gesvd -- auxiliary functions *)

let gesvd_err loc jobu jobvt m n a u vt lwork err =
  if err > 0 then
    failwith
      (sprintf "%s: %d off-diagonal elements did not converge" loc err)
  else
    let msg =
      match err with
      | -3 -> sprintf "m: valid=[0..[ got=%d" m
      | -4 -> sprintf "n: valid=[0..[ got=%d" n
      | -6 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 m) (Array2.dim1 a)
      | -9 ->
          sprintf "dim1(u): valid=[%d..[ got=%d"
            (if jobu = 'A' || jobu = 'A' then max 1 m else 1) (Array2.dim1 u)
      | -11 ->
          sprintf "dim1(vt): valid=[%d..[ got=%d"
            (
              if jobvt = 'A' then max 1 n
              else if jobvt = 'S' then max 1 (min m n)
              else 1)
            (Array2.dim1 vt)
      | -13 -> sprintf "lwork: valid=[%d..[ got=%d" 1 lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let gesvd_get_params
    loc vec_create mat_create jobu jobvt m n ar ac a s ur uc u vtr vtc vt =
  let jobu = get_s_d_job_char jobu in
  let jobvt = get_s_d_job_char jobvt in
  let m = get_dim1_mat loc "a" a ar "m" m in
  let n = get_dim2_mat loc "a" a ac "n" n in
  let s = get_vec loc "s" s 1 1 (min m n) vec_create in
  let um, un =
    match jobu with
    | 'A' -> m, m
    | 'S' -> m, min m n
    | _  -> 1, 1 in  (* LDU >= 1 even when U not referenced *)
  let u =
    match u with
    | Some u ->
        check_dim1_mat loc "u" u ur "um" um;
        check_dim2_mat loc "u" u uc "un" un;
        u
    | None -> mat_create um un in
  let vm, vn =
    match jobvt with
    | 'A' -> n, n
    | 'S' -> min m n, n
    | _ ->  1, 1 in  (* LDVT >= 1 even when VT not referenced *)
  let vt =
    match vt with
    | Some vt ->
        check_dim1_mat loc "vt" vt vtr "vm" vm;
        check_dim2_mat loc "vt" vt vtc "vn" vn;
        vt
    | None -> mat_create vm vn in
  jobu, jobvt, m, n, s, u, vt


(* gesdd -- auxiliary functions *)

let gesdd_err loc jobz m n a u vt lwork err =
  if err > 0 then
    failwith (
      sprintf "%s: %d DBDSDC did not converge, updating process failed" loc err)
  else
    let msg =
      match err with
      | -2 -> sprintf "m: valid=[0..[ got=%d" m
      | -3 -> sprintf "n: valid=[0..[ got=%d" n
      | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 m) (Array2.dim1 a)
      | -8 ->
          sprintf "dim1(u): valid=[%d..[ got=%d"
            (if jobz = 'A' || jobz = 'O' && m < n then max 1 m else 1)
            (Array2.dim1 u)
      | -10 ->
          sprintf "dim1(vt): valid=[%d..[ got=%d"
            (
              if jobz = 'A' || jobz = 'O' && m >= n then max 1 n
              else if jobz = 'S' then max 1 (min m n)
              else 1
            )
            (Array2.dim1 vt)
      | -12 -> sprintf "lwork: valid=[%d..[ got=%d" 1 lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let gesdd_get_params
      loc vec_create mat_create jobz m n ar ac a s ur uc u vtr vtc vt =
  let jobz = get_s_d_job_char jobz in
  let m = get_dim1_mat loc "a" a ar "m" m in
  let n = get_dim2_mat loc "a" a ac "n" n in
  let min_m_n = min m n in
  let s = get_vec loc "s" s 1 1 min_m_n vec_create in
  let um, un, vm, vn =
    match jobz with
    | 'A' -> m, m, n, n
    | 'S' -> m, min_m_n, min_m_n, n
    | 'O' -> if m >= n then 1, 1, n, n else m, m, m, n
    | _  -> 1, 1, 1, 1 in  (* LDU >= 1 even when U not referenced *)
  let u =
    match u with
    | Some u ->
        check_dim1_mat loc "u" u ur "um" um;
        check_dim2_mat loc "u" u uc "un" un;
        u
    | None -> mat_create um un in
  let vt =
    match vt with
    | Some vt ->
        check_dim1_mat loc "vt" vt vtr "vm" vm;
        check_dim2_mat loc "vt" vt vtc "vn" vn;
        vt
    | None -> mat_create vm vn in
  jobz, m, n, s, u, vt


(* ??sv -- auxiliary functions *)

let xxsv_err loc n nrhs b err =
  let msg =
    match err with
    | -1 -> sprintf "n: valid=[0..[ got=%d" n
    | -2 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
    | -7 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

let xxsv_lu_err loc n nrhs b err =
  failwith (sprintf "%s: U(%i,%i)=0 in the LU factorization" loc err err)

let xxsv_pos_err loc n nrhs b err =
  let msg =
    sprintf
      "%s: the leading minor of order %i is not positive definite" loc err in
  failwith msg

let xxsv_ind_err loc n nrhs b err =
  let msg =
    sprintf
      "%s: D(%i,%i)=0 in the diagonal pivoting factorization" loc err err in
  failwith msg

let xxsv_a_err loc a n =
  let msg =
    sprintf "%s: dim1(a): valid=[%d..[ got=%d" loc (max 1 n) (Array2.dim1 a) in
  invalid_arg msg

let xxsv_work_err loc lwork =
  invalid_arg (sprintf "%s: dim(work): valid=[1..[ got=%d" loc lwork)

let xxsv_get_ipiv loc ipiv n =
  match ipiv with
  | None -> create_int_vec n
  | Some ipiv ->
      check_vec loc "ipiv" ipiv n;
      ipiv

let xxsv_get_params loc ar ac a n br bc b nrhs =
  let n = get_n_of_a loc ar ac a n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  n, nrhs