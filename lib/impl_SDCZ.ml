(* File: impl_SDCZ.ml

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

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

open Printf
open Bigarray
open Lacaml_numberxx
open Lacaml_common
open Lacaml_utils

module Vec = Lacaml_vec4_NPREC
module Mat = Lacaml_mat4_NPREC

module RVec = Lacaml_vec4_NBPREC


(* BLAS-1 *)

(* SWAP *)

external direct_swap :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECswap_stub_bc" "lacaml_NPRECswap_stub"

let swap ?n ?ofsx ?incx ~x ?ofsy ?incy y =
  let loc = "Lacaml.NPREC.swap" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  check_vec loc y_str y (ofsy + (n - 1) * abs incy);
  direct_swap ~n ~ofsx ~incx ~x ~ofsy ~incy ~y


(* SCAL *)

external direct_scal :
  n : int ->
  alpha : num_type ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECscal_stub"

let scal ?n alpha ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.scal" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_scal ~n ~alpha ~ofsx ~incx ~x


(* COPY *)

external direct_copy :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

let copy ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.copy" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  let y =
    let min_dim_y = ofsy + (n - 1) * abs incy in
    match y with
    | Some y -> check_vec loc y_str y min_dim_y; y
    | None -> Vec.create min_dim_y in
  direct_copy ~n ~ofsy ~incy ~y ~ofsx ~incx ~x;
  y


(* NRM2 *)

external direct_nrm2 :
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec
  -> float = "lacaml_NPRECnrm2_stub"

let nrm2 ?n ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.nrm2" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_nrm2 ~n ~ofsx ~incx ~x


(* AXPY *)

external direct_axpy :
  alpha : num_type ->
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  unit = "lacaml_NPRECaxpy_stub_bc" "lacaml_NPRECaxpy_stub"

let axpy ?(alpha = one) ?n ?ofsx ?incx x ?ofsy ?incy y =
  let loc = "Lacaml.NPREC.axpy" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  check_vec loc y_str y (ofsy + (n - 1) * abs incy);
  direct_axpy ~alpha ~n ~ofsx ~incx ~x ~ofsy ~incy ~y


(* AMAX *)

external direct_iamax :
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  int = "lacaml_NPRECiamax_stub"

let iamax ?n ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.iamax" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_iamax ~n ~ofsx ~incx ~x

let amax ?n ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.amax" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  x.{direct_iamax ~n ~ofsx ~incx ~x}


(* BLAS-2 *)

(* GEMV *)

external direct_gemv :
  ofsy : int ->
  incy : int ->
  y : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  m : int ->
  n : int ->
  trans : char ->
  alpha : num_type ->
  beta : num_type ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECgemv_stub_bc" "lacaml_NPRECgemv_stub"

let gemv ?m ?n ?(beta = zero) ?ofsy ?incy ?y ?(trans = `N) ?(alpha = one)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.gemv" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let ofsx, incx, ofsy, incy, y, trans =
    gXmv_get_params loc Vec.create m n ofsx incx x ofsy incy y trans
  in
  direct_gemv
    ~ofsy ~incy ~y ~ar ~ac ~a ~m ~n ~trans ~alpha ~beta ~ofsx ~incx ~x;
  y


(* GBMV *)

external direct_gbmv :
  ofsy : int ->
  incy : int ->
  y : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  m : int ->
  n : int ->
  kl : int ->
  ku : int ->
  trans : char ->
  alpha : num_type ->
  beta : num_type ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECgbmv_stub_bc" "lacaml_NPRECgbmv_stub"

let gbmv ?m ?n ?(beta = zero) ?ofsy ?incy ?y ?(trans = `N) ?(alpha = one)
      ?(ar = 1) ?(ac = 1) a kl ku ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.gbmv" in
  check_var_ltz loc kl_str kl;
  check_var_ltz loc ku_str ku;
  check_dim1_mat loc a_str a ar "kl + ku + 1 for " (kl + ku + 1);
  let n = get_dim2_mat loc a_str a ac n_str n in
  let m = match m with
    | None -> n
    | Some m -> check_var_ltz loc m_str m; m in
  let ofsx, incx, ofsy, incy, y, trans =
    gXmv_get_params loc Vec.create m n ofsx incx x ofsy incy y trans
  in
  direct_gbmv
    ~ofsy ~incy ~y ~ar ~ac ~a ~m ~n ~kl ~ku ~trans ~alpha ~beta ~ofsx ~incx ~x;
  y


(* SYMV *)

external direct_symv :
  ofsy : int ->
  incy : int ->
  y : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  uplo : char ->
  alpha : num_type ->
  beta : num_type ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECsymv_stub_bc" "lacaml_NPRECsymv_stub"

let symv ?n ?(beta = zero) ?ofsy ?incy ?y ?(up = true) ?(alpha = one)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.symv" in
  let n, ofsx, incx, ofsy, incy, y, uplo =
    symv_get_params loc Vec.create ar ac a n ofsx incx x ofsy incy y up in
  direct_symv ~ofsy ~incy ~y ~ar ~ac ~a ~n ~uplo ~alpha ~beta ~ofsx ~incx ~x;
  y


(* TRMV *)

external direct_trmv :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  uplo : char ->
  trans : char ->
  diag : char ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECtrmv_stub_bc" "lacaml_NPRECtrmv_stub"

let trmv
      ?n ?(trans = `N) ?(diag = `N) ?(up = true)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.trmv" in
  let n, ofsx, incx, uplo, trans, diag =
    trXv_get_params loc ar ac a n ofsx incx x up trans diag
  in
  direct_trmv ~ar ~ac ~a ~n ~uplo ~trans ~diag ~ofsx ~incx ~x


(* TRSV *)

external direct_trsv :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  uplo : char ->
  trans : char ->
  diag : char ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECtrsv_stub_bc" "lacaml_NPRECtrsv_stub"

let trsv
      ?n ?(trans = `N) ?(diag = `N) ?(up = true)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.trsv" in
  let n, ofsx, incx, uplo, trans, diag =
    trXv_get_params loc ar ac a n ofsx incx x up trans diag
  in
  direct_trsv ~ar ~ac ~a ~n ~uplo ~trans ~diag ~ofsx ~incx ~x


(* TPMV *)

external direct_tpmv :
  ofsap : int ->
  ap : vec ->
  n : int ->
  uplo : char ->
  trans : char ->
  diag : char ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECtpmv_stub_bc" "lacaml_NPRECtpmv_stub"

let tpmv ?n ?(trans = `N) ?(diag = `N) ?(up = true) ?ofsap ap ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.tpmv" in
  let n, ofsap, ofsx, incx, uplo, trans, diag =
    tpXv_get_params loc ofsap ap ?n ofsx incx x up trans diag
  in
  direct_tpmv ~ofsap ~ap ~n ~uplo ~trans ~diag ~ofsx ~incx ~x


(* TPSV *)

external direct_tpsv :
  ofsap : int ->
  ap : vec ->
  n : int ->
  uplo : char ->
  trans : char ->
  diag : char ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  unit = "lacaml_NPRECtpsv_stub_bc" "lacaml_NPRECtpsv_stub"

let tpsv ?n ?(trans = `N) ?(diag = `N) ?(up = true) ?ofsap ap ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.tpsv" in
  let n, ofsap, ofsx, incx, uplo, trans, diag =
    tpXv_get_params loc ofsap ap ?n ofsx incx x up trans diag
  in
  direct_tpsv ~ofsap ~ap ~n ~uplo ~trans ~diag ~ofsx ~incx ~x


(* BLAS-3 *)

(* GEMM *)

external direct_gemm :
  transa : char ->
  transb : char ->
  m : int ->
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  cr : int ->
  cc : int ->
  c : mat ->
  alpha : num_type ->
  beta : num_type ->
  unit = "lacaml_NPRECgemm_stub_bc" "lacaml_NPRECgemm_stub"

let gemm ?m ?n ?k ?(beta = zero) ?(cr = 1) ?(cc = 1) ?c
      ?(transa = `N) ?(alpha = one) ?(ar = 1) ?(ac = 1) a
      ?(transb = `N) ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.gemm" in
  let m, n, k, transa, transb, c =
    gemm_get_params loc Mat.create ar ac a transa br bc b cr transb cc c m n k in
  direct_gemm
    ~transa ~transb ~m ~n ~k ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c ~alpha ~beta;
  c


(* SYMM *)

external direct_symm :
  side : char ->
  uplo : char ->
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
  alpha : num_type ->
  beta : num_type ->
  unit = "lacaml_NPRECsymm_stub_bc" "lacaml_NPRECsymm_stub"

let symm ?m ?n ?(side = `L) ?(up = true)
      ?(beta = zero) ?(cr = 1) ?(cc = 1) ?c
      ?(alpha = one) ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.symm" in
  let m, n, side, uplo, c =
    symm_get_params loc Mat.create ar ac a br bc b cr cc c m n side up in
  direct_symm ~side ~uplo ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c ~alpha ~beta;
  c


(* TRMM *)

external direct_trmm :
  side : char ->
  uplo : char ->
  transa : char ->
  diag : char ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  alpha : num_type ->
  unit = "lacaml_NPRECtrmm_stub_bc" "lacaml_NPRECtrmm_stub"

let trmm ?m ?n ?(side = `L) ?(up = true) ?(transa = `N) ?(diag = `N)
      ?(alpha = one) ?(ar = 1) ?(ac = 1) ~a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.trmm" in
  let m, n, side, uplo, transa, diag =
    trXm_get_params loc ar ac a br bc b m n side up transa diag
  in
  direct_trmm ~side ~uplo ~transa ~diag ~m ~n ~ar ~ac ~a ~br ~bc ~b ~alpha


(* TRSM *)

external direct_trsm :
  side : char ->
  uplo : char ->
  transa : char ->
  diag : char ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  alpha : num_type ->
  unit = "lacaml_NPRECtrsm_stub_bc" "lacaml_NPRECtrsm_stub"

let trsm ?m ?n ?(side = `L) ?(up = true) ?(transa = `N) ?(diag = `N)
      ?(alpha = one) ?(ar = 1) ?(ac = 1) ~a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.trsm" in
  let m, n, side, uplo, transa, diag =
    trXm_get_params loc ar ac a br bc b m n side up transa diag
  in
  direct_trsm ~side ~uplo ~transa ~diag ~m ~n ~ar ~ac ~a ~br ~bc ~b ~alpha


(* SYRK *)

external direct_syrk :
  uplo : char ->
  trans : char ->
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  cr : int ->
  cc : int ->
  c : mat ->
  alpha : num_type ->
  beta : num_type ->
  unit = "lacaml_NPRECsyrk_stub_bc" "lacaml_NPRECsyrk_stub"

let syrk ?n ?k ?(up = true) ?(beta = zero) ?(cr = 1) ?(cc = 1) ?c
      ?(trans = `N) ?(alpha = one) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.syrk" in
  let n, k, uplo, trans, c =
    syrk_get_params loc Mat.create ar ac a cr cc c n k up trans in
  direct_syrk ~uplo ~trans ~n ~k ~ar ~ac ~a ~cr ~cc ~c ~alpha ~beta;
  c


(* SYR2K *)

external direct_syr2k :
  uplo : char ->
  trans : char ->
  n : int ->
  k : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  cr : int ->
  cc : int ->
  c : mat ->
  alpha : num_type ->
  beta : num_type ->
  unit = "lacaml_NPRECsyr2k_stub_bc" "lacaml_NPRECsyr2k_stub"

let syr2k ?n ?k ?(up = true) ?(beta = zero) ?(cr = 1) ?(cc = 1) ?c
      ?(trans = `N) ?(alpha = one) ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.syr2k" in
  let n, k, uplo, trans, c =
    syr2k_get_params loc Mat.create ar ac a br bc b cr cc c n k up trans
  in
  direct_syr2k ~uplo ~trans ~n ~k ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c ~alpha ~beta;
  c


(* LAPACK *)

(* Auxiliary routines *)

(* LACPY *)

external direct_lacpy :
  uplo : char ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  unit = "lacaml_NPREClacpy_stub_bc" "lacaml_NPREClacpy_stub"

let lacpy ?uplo ?m ?n ?(br = 1) ?(bc = 1) ?b ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.lacpy" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let b =
    match b with
    | Some b -> check_dim_mat loc b_str br bc b m n; b
    | None ->
        check_var_ltz loc br_str br;
        check_var_ltz loc bc_str bc;
        let min_bm = m + br - 1 in
        let min_bn = n + bc - 1 in
        Mat.create min_bm min_bn
  in
  let uplo =
    match uplo with
    | None -> 'A'
    | Some `U -> 'U'
    | Some `L -> 'L'
  in
  direct_lacpy ~uplo ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  b

(* LASWP  *)

external direct_laswp :
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  k1 : int ->
  k2 : int ->
  ipiv : int32_vec ->
  incx : int ->
  unit = "lacaml_NPREClaswp_stub_bc" "lacaml_NPREClaswp_stub"

let laswp ?n ?(ar = 1) ?(ac = 1) a ?(k1 = 1) ?k2 ?(incx = 1) ipiv =
  let loc = "Lacaml.NPREC.laswp" in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let ipiv_n = Array1.dim ipiv in
  check_var_within loc k1_str k1 1 ipiv_n string_of_int;
  let k2 =
    match k2 with
    | None -> ipiv_n
    | Some k2v ->
        check_var_within loc k2_str k2v 1 ipiv_n string_of_int;
        k2v
  in
  check_vec loc ipiv_str ipiv (k2 * abs incx);
  let ub = Int32.of_int (Array2.dim1 a) in
  for i = 1 to ipiv_n do
    let r = ipiv.{i} in
    check_var_within loc (sprintf "%s(%d)" ipiv_str i) r 1l ub Int32.to_string
  done;
  direct_laswp ~n ~ar ~ac ~a ~k1 ~k2 ~ipiv ~incx

(* LAPMT  *)

external direct_lapmt :
  forward : bool ->
  m : int ->
  n : int ->
  k : int32_vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  unit = "lacaml_NPREClapmt_stub_bc" "lacaml_NPREClapmt_stub"

let lapmt ?(forward = true) ?m ?n ?(ar = 1) ?(ac = 1) a k =
  let loc = "Lacaml.NPREC.lapmt" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let k_n = Array1.dim k in
  check_vec_is_perm loc k_str k n;
  direct_lapmt ~forward ~m ~n ~k ~ar ~ac ~a

(* LASSQ *)

external direct_lassq :
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  scale : float ->
  sumsq : float ->
  float * float = "lacaml_NPREClassq_stub_bc" "lacaml_NPREClassq_stub"

let lassq ?n ?(scale = 0.) ?(sumsq = 1.) ?ofsx ?incx x =
  let loc = "Lacaml.NPREC.lassq" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_lassq ~n ~ofsx ~incx ~x ~scale ~sumsq


(* LARNV *)

external direct_larnv :
  idist : int ->
  iseed : int32_vec ->
  n : int ->
  ofsx : int ->
  x : vec ->
  unit = "lacaml_NPREClarnv_stub"

let larnv ?idist ?iseed ?n ?ofsx ?x () =
  let loc = "Lacaml.NPREC.larnv" in
  let idist =
    match idist with
    | Some `Uniform0 -> 1
    | Some `Uniform1 -> 2
    | None | Some `Normal -> 3
  in
  let ofsiseed = 1 in
  let iseed =
    match iseed with
    | None ->
        let iseed = create_int32_vec (ofsiseed + 3) in
        for i = ofsiseed to ofsiseed + 3 do iseed.{i} <- Int32.one done;
        iseed
    | Some iseed ->
        if Array1.dim iseed - ofsiseed < 3 then
          invalid_arg (
            sprintf "%s: iseed needs at least four available elements" loc);
        for i = ofsiseed to ofsiseed + 3 do
          if iseed.{i} < Int32.zero || iseed.{i} > 4095l then
            invalid_arg (
              sprintf "%s: iseed entries must be between 0 and 4095" loc)
        done;
        if (Int32.to_int iseed.{ofsiseed + 3}) land 1 = 1 then iseed
        else invalid_arg (sprintf "%s: last iseed entry must be odd" loc)
  in
  let ofsx = get_ofs loc x_str ofsx in
  let n, x =
    match n, x with
    | None, None -> 1, Vec.create ofsx
    | Some n, None ->
        check_var_ltz loc n_str n;
        n, Vec.create (ofsx - 1 + n)
    | None, Some x -> Vec.dim x - ofsx + 1, x
    | Some n, Some x ->
        check_var_ltz loc n_str n;
        let min_dim = ofsx - 1 + n in
        check_vec loc x_str x min_dim;
        n, x
  in
  direct_larnv ~idist ~iseed ~n ~ofsx ~x;
  x


(* LANGE *)

external direct_lange :
  norm : char ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  work : rvec ->
  float = "lacaml_NPREClange_stub_bc" "lacaml_NPREClange_stub"

let lange_min_lwork m = function `I -> m | _ -> 0

let lange ?m ?n ?(norm = `O) ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.lange" in
  let m, n = xlange_get_params loc m n ar ac a in
  let work =
    match work with
    | Some work ->
        let lwork = Array1.dim work in
        let min_lwork = lange_min_lwork m norm in
        if lwork < min_lwork then
          invalid_arg
            (sprintf "%s: lwork: valid=[%d..[ got=%d" loc min_lwork lwork)
        else work
    | None ->
        let lwork = lange_min_lwork m norm in
        RVec.create lwork
  in
  let norm = get_norm_char norm in
  direct_lange ~norm ~m ~n ~ar ~ac ~a ~work

(* DLAUUM *)

external direct_lauum :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  unit = "lacaml_NPREClauum_stub"

let lauum ?(up = true) ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.lauum" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  direct_lauum ~uplo ~n ~ar ~ac ~a


(* Linear equations (computational routines) *)

(* GETRF *)

external direct_getrf :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ipiv : int32_vec ->
  int = "lacaml_NPRECgetrf_stub_bc" "lacaml_NPRECgetrf_stub"

let getrf ?m ?n ?ipiv ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.getrf" in
  let m, n = geXrf_get_params loc m n ar ac a in
  let ipiv = getrf_get_ipiv loc ipiv m n in
  let info = direct_getrf ~m ~n ~ar ~ac ~a ~ipiv in
  if info = 0 then ipiv
  else
    if info > 0 then getrf_lu_err loc info
    else getrf_err loc m n a info

(* GETRS *)

external direct_getrs :
  trans : char ->
  n : int ->
  nrhs : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  ipiv : int32_vec ->
  int = "lacaml_NPRECgetrs_stub_bc" "lacaml_NPRECgetrs_stub"

let getrs
    ?n ?ipiv ?(trans = `N) ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.getrs" in
  let trans = get_trans_char trans in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  let ipiv =
    if ipiv = None then getrf ~m:n ~n ~ar ~ac a
    else getrf_get_ipiv loc ipiv n n in
  let info = direct_getrs ~trans ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b ~ipiv in
  if info <> 0 then xxtrs_err loc n nrhs a b info

(* GETRI *)

external direct_getri :
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ipiv : int32_vec ->
  work : vec ->
  lwork : int ->
  int = "lacaml_NPRECgetri_stub_bc" "lacaml_NPRECgetri_stub"

let getri_min_lwork n = max 1 n

let getri_get_opt_lwork loc n ar ac a =
  let work = Vec.create 1 in
  let info = direct_getri ~n ~ar ~ac ~a ~ipiv:empty_int32_vec ~work ~lwork:~-1 in
  if info = 0 then int_of_numberxx work.{1}
  else getri_err loc getri_min_lwork n a 1 info

let getri_opt_lwork ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.getri_opt_lwork" in
  let n = get_n_of_a loc ar ac a n in
  getri_get_opt_lwork loc n ar ac a

let getri ?n ?ipiv ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.getri" in
  let n = get_n_of_a loc ar ac a n in
  let work, lwork =
    get_work
      loc Vec.create work
      (getri_min_lwork n) (getri_get_opt_lwork loc n ar ac a) "lwork" in
  let ipiv =
    if ipiv = None then getrf ~m:n ~n ~ar ~ac a
    else getrf_get_ipiv loc ipiv n n in
  let info = direct_getri ~n ~ar ~ac ~a ~ipiv ~work ~lwork in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else getri_err loc getri_min_lwork n a lwork info

(* SYTRF *)

external direct_sytrf :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ipiv : int32_vec ->
  work : vec ->
  lwork : int ->
  int = "lacaml_NPRECsytrf_stub_bc" "lacaml_NPRECsytrf_stub"

let sytrf_get_opt_lwork loc uplo n ar ac a =
  let work = Vec.create 1 in
  let info =
    direct_sytrf ~uplo ~n ~ar ~ac ~a ~ipiv:empty_int32_vec ~work ~lwork:~-1
  in
  if info = 0 then int_of_numberxx work.{1}
  else sytrf_err loc n a info

let sytrf_opt_lwork ?n ?(up = true) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.sytrf_opt_lwork" in
  let uplo = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  sytrf_get_opt_lwork loc uplo n ar ac a

let sytrf_min_lwork () = 1

let sytrf ?n ?(up = true) ?ipiv ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.sytrf" in
  let uplo = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  let ipiv = sytrf_get_ipiv loc ipiv n in
  let work, lwork =
    get_work
      loc Vec.create work
      (sytrf_min_lwork ())
      (sytrf_get_opt_lwork loc uplo n ar ac a) "lwork" in
  let info = direct_sytrf ~uplo ~n ~ar ~ac ~a ~ipiv ~work ~lwork in
  if info = 0 then ipiv
  else
    if info > 0 then sytrf_fact_err loc info
    else sytrf_err loc n a info

(* SYTRS *)

external direct_sytrs :
  uplo : char ->
  n : int ->
  nrhs : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  ipiv : int32_vec ->
  int = "lacaml_NPRECsytrs_stub_bc" "lacaml_NPRECsytrs_stub"

let sytrs
      ?n ?(up = true) ?ipiv ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.sytrs" in
  let uplo = get_uplo_char up in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n in
  let info = direct_sytrs ~uplo ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b ~ipiv in
  if info <> 0 then xxtrs_err loc n nrhs a b info

(* SYTRI *)

external direct_sytri :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ipiv : int32_vec ->
  work : vec ->
  int = "lacaml_NPRECsytri_stub_bc" "lacaml_NPRECsytri_stub"

let sytri_min_lwork n = n

let sytri ?n ?(up = true) ?ipiv ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.sytri" in
  let uplo = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  let work, _lwork =
    get_work
      loc Vec.create work
      (sytri_min_lwork n) (sytri_min_lwork n) "lwork"
  in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n
  in
  let info = direct_sytri ~uplo ~n ~ar ~ac ~a ~ipiv ~work in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else xxtri_err loc n a info

(* POTRF *)

external direct_potrf :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  int = "lacaml_NPRECpotrf_stub"

let maybe_add_jitter ~loc ?jitter ~ar ~ac ~n a =
  match jitter with
  | None -> ()
  | Some jitter when jitter < zero ->
      invalid_arg (sprintf "%s: jitter < zero" loc)
  | Some jitter when jitter = zero -> ()
  | Some jitter ->
      for i = 0 to n - 1 do
        let ari = ar + i in
        let aci = ac + i in
        a.{ari, aci} <- add a.{ari, aci} jitter
      done

let potrf ?n ?(up = true) ?(ar = 1) ?(ac = 1) ?jitter a =
  let loc = "Lacaml.NPREC.potrf" in
  let uplo = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  maybe_add_jitter ~loc ?jitter ~ar ~ac ~n a;
  let info = direct_potrf ~uplo ~n ~ar ~ac ~a in
  if info <> 0 then
    if info > 0 then potrf_chol_err loc info
    else potrf_err loc n a info

(* POTRS *)

external direct_potrs :
  uplo : char ->
  n : int ->
  nrhs : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECpotrs_stub_bc" "lacaml_NPRECpotrs_stub"

let potrs
      ?n ?(up = true) ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1)
      ?(factorize = true) ?jitter b =
  let loc = "Lacaml.NPREC.potrs" in
  let uplo = get_uplo_char up in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  if factorize then potrf ~n ~up ~ar ~ac ?jitter a;
  let info = direct_potrs ~uplo ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b in
  if info <> 0 then potrs_err loc n nrhs a b info

(* POTRI *)

external direct_potri :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  int = "lacaml_NPRECpotri_stub"

let potri ?n ?(up = true) ?(ar = 1) ?(ac = 1) ?(factorize = true) ?jitter a =
  let loc = "Lacaml.NPREC.potri" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  if factorize then potrf ~n ~up ~ar ~ac ?jitter a;
  let info = direct_potri ~uplo ~n ~ar ~ac ~a in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else xxtri_err loc n a info

(* TRTRS *)

external direct_trtrs :
  uplo : char ->
  trans : char ->
  diag : char ->
  n : int ->
  nrhs : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECtrtrs_stub_bc" "lacaml_NPRECtrtrs_stub"

let trtrs
      ?n ?(up = true) ?(trans = `N) ?(diag = `N)
      ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.trtrs" in
  let uplo = get_uplo_char up in
  let trans = get_trans_char trans in
  let diag = get_diag_char diag in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  let info =
    direct_trtrs ~uplo ~trans ~diag ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b
  in
  if info <> 0 then trtrs_err loc n nrhs a b info

(* TRTRI *)

external direct_trtri :
  uplo : char ->
  diag : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  int = "lacaml_NPRECtrtri_stub_bc" "lacaml_NPRECtrtri_stub"

let trtri ?n ?(up = true) ?(diag = `N) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.trtri" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let diag = get_diag_char diag in
  let info = direct_trtri ~uplo ~diag ~n ~ar ~ac ~a in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else trtri_err loc n a info

(* TBTRS *)

external direct_tbtrs :
  uplo : char ->
  trans : char ->
  diag : char ->
  n : int ->
  kd : int ->
  nrhs : int ->
  abr : int ->
  abc : int ->
  ab : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECtbtrs_stub_bc" "lacaml_NPRECtbtrs_stub"

let tbtrs
    ?n ?kd ?(up = true) ?(trans = `N) ?(diag = `N)
    ?(abr = 1) ?(abc = 1) ab ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.tbtrs" in
  let uplo = get_uplo_char up in
  let trans = get_trans_char trans in
  let diag = get_diag_char diag in
  let n = get_dim2_mat loc ab_str ab abc n_str n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let kd = get_k_mat_sb loc ab_str ab abr kd_str kd in
  let info =
    direct_tbtrs ~uplo ~trans ~diag ~n ~kd ~nrhs ~abr ~abc ~ab ~br ~bc ~b
  in
  if info <> 0 then tbtrs_err loc n nrhs kd ab b info

(* GEQRF *)

external direct_geqrf :
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  tau : vec ->
  work : vec ->
  lwork : int ->
  unit = "lacaml_NPRECgeqrf_stub_bc" "lacaml_NPRECgeqrf_stub"

let geqrf_get_opt_lwork m n ar ac a =
  let work = Vec.create 1 in
  direct_geqrf ~m ~n ~ar ~ac ~a ~tau:work ~work ~lwork:~-1;
  int_of_numberxx work.{1}

let geqrf_opt_lwork ?m ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.geqrf_opt_lwork" in
  let m, n = geXrf_get_params loc m n ar ac a in
  geqrf_get_opt_lwork m n ar ac a

let geqrf_min_lwork ~n = max 1 n

let geqrf ?m ?n ?work ?tau ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.NPREC.geqrf" in
  let m, n = geXrf_get_params loc m n ar ac a in
  let tau =
    let min_m_n = min m n in
    match tau with
    | None -> Vec.create min_m_n
    | Some tau ->
        if Vec.dim tau < min_m_n then
          invalid_arg (sprintf "%s: dim(tau): valid=[1..[ got=%d" loc min_m_n)
        else tau
  in
  let work, lwork =
    get_work
      loc Vec.create work
      (geqrf_min_lwork ~n)
      (geqrf_get_opt_lwork m n ar ac a) "lwork"
  in
  direct_geqrf ~m ~n ~ar ~ac ~a ~tau ~work ~lwork;
  tau


(* Linear equations (simple drivers) *)

(* GESV *)

external direct_gesv :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  ipiv : int32_vec ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECgesv_stub_bc" "lacaml_NPRECgesv_stub"

let gesv ?n ?ipiv ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.gesv" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let info = direct_gesv ~ar ~ac ~a ~n ~ipiv ~nrhs ~br ~bc ~b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_lu_err loc info
  | -4 -> xxsv_a_err loc a n
  | _ -> xxsv_err loc n nrhs b info

(* GBSV *)

external direct_gbsv :
  abr : int ->
  abc : int ->
  ab : mat ->
  n : int ->
  kl : int ->
  ku : int ->
  ipiv : int32_vec ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECgbsv_stub_bc" "lacaml_NPRECgbsv_stub"

let gbsv ?n ?ipiv ?(abr = 1) ?(abc = 1) ab kl ku ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.gbsv" in
  let n = get_dim2_mat loc ab_str ab abc n_str n in
  let min_dim1 = 2*kl + ku + 1 in
  (* kl >= 0, ku >= 0: tested by the FORTRAN routine. *)
  check_dim1_mat loc ab_str ab abr "2*kl + ku + 1 for " min_dim1;
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let info = direct_gbsv ~abr ~abc ~ab ~n ~kl ~ku ~ipiv ~nrhs ~br ~bc ~b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_lu_err loc info
  | -1 -> xxsv_err loc n nrhs b info
  | -2 -> invalid_arg (sprintf "%s: kl: valid=[0..[ got=%d" loc kl)
  | -3 -> invalid_arg (sprintf "%s: ku: valid=[0..[ got=%d" loc ku)
  | -6 ->
      let msg =
        sprintf "%s: dim1(ab): valid=[%d..[ got=%d"
          loc min_dim1 (Mat.dim1 ab) in
      invalid_arg msg
  | _ -> xxsv_err loc n nrhs b (info + 2)

(* GTSV *)

external direct_gtsv :
  ofsdl : int ->
  dl : vec ->
  ofsd : int ->
  d : vec ->
  ofsdu : int ->
  du : vec ->
  n : int ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECgtsv_stub_bc" "lacaml_NPRECgtsv_stub"

let gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.gtsv" in
  let ofsdl = get_ofs loc dl_str ofsdl in
  let ofsd = get_ofs loc d_str ofsd in
  let ofsdu = get_ofs loc du_str ofsdu in
  let n = get_dim_vec loc d_str ofsd 1 d n_str n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  check_vec loc dl_str dl (ofsdl + n - 2);
  check_vec loc du_str du (ofsdu + n - 2);
  let info = direct_gtsv ~ofsdl ~dl ~ofsd ~d ~ofsdu ~du ~n ~nrhs ~br ~bc ~b in
  if info <> 0 then (
    if info > 0 then xxsv_lu_err loc info
    else xxsv_err loc n nrhs b info)

(* POSV *)

external direct_posv :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  uplo : char ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECposv_stub_bc" "lacaml_NPRECposv_stub"

let posv ?n ?(up = true) ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.posv" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let info =
    direct_posv ~ar ~ac ~a ~n ~uplo:(get_uplo_char up) ~nrhs ~br ~bc ~b
  in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_pos_err loc info
  | -5 -> xxsv_a_err loc a n
  | _ -> xxsv_err loc n nrhs b (info + 1)

(* PPSV *)

external direct_ppsv :
  ofsap : int ->
  ap : vec ->
  n : int ->
  uplo : char ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECppsv_stub_bc" "lacaml_NPRECppsv_stub"

let ppsv ?n ?(up = true) ?ofsap ap ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.ppsv" in
  let ofsap = get_ofs loc ap_str ofsap in
  let n = get_dim_mat_packed loc ap_str ofsap ap n_str n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let info =
    direct_ppsv ~ofsap ~ap ~n ~uplo:(get_uplo_char up) ~nrhs ~br ~bc ~b
  in
  if info <> 0 then
    if info > 0 then xxsv_pos_err loc info
    else xxsv_err loc n nrhs b (info - 1) (* only: LDB *)

(* PBSV *)

external direct_pbsv :
  abr : int ->
  abc : int ->
  ab : mat ->
  n : int ->
  kd : int ->
  uplo : char ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECpbsv_stub_bc" "lacaml_NPRECpbsv_stub"

let pbsv ?n ?(up = true) ?kd ?(abr = 1) ?(abc = 1) ab
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.pbsv" in
  (* [a] is a band matrix of size [k+1]*[n]. *)
  let n = get_dim2_mat loc ab_str ab abc n_str n in
  let kd = get_k_mat_sb loc ab_str ab abr kd_str kd in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let info =
    direct_pbsv ~abr ~abc ~ab ~n ~kd ~uplo:(get_uplo_char up) ~nrhs ~br ~bc ~b
  in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_pos_err loc info
  | -2 -> xxsv_err loc n nrhs b info
  | -3 -> invalid_arg (sprintf "%s: kd: valid=[0..[ got=%d" loc kd)
  | -6 ->
      let msg =
        sprintf
          "%s: dim1(ab): valid=[%d..[ got=%d" loc (kd + 1) (Mat.dim1 ab) in
      invalid_arg msg
  | _ -> xxsv_err loc n nrhs b (info + 2)

(* PTSV *)

external direct_ptsv :
  ofsd : int ->
  d : vec ->
  ofse : int ->
  e : vec ->
  n : int ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECptsv_stub_bc" "lacaml_NPRECptsv_stub"

let ptsv ?n ?ofsd d ?ofse e ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.ptsv" in
  let ofsd = get_ofs loc d_str ofsd in
  let ofse = get_ofs loc e_str ofse in
  let n = get_dim_vec loc d_str ofsd 1 d n_str n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  check_vec loc e_str e (ofse + n - 2);
  let info = direct_ptsv ~ofsd ~d ~ofse ~e ~n ~nrhs ~br ~bc ~b in
  if info <> 0 then
    if info > 0 then xxsv_pos_err loc info
    else xxsv_err loc n nrhs b (info - 1)

(* SYSV *)

external direct_sysv :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  uplo : char ->
  ipiv : int32_vec ->
  work : vec ->
  lwork : int ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECsysv_stub_bc" "lacaml_NPRECsysv_stub"

let sysv_get_opt_lwork loc ar ac a n uplo nrhs br bc b =
  let work = Vec.create 1 in
  let info =
    direct_sysv
      ~ar ~ac ~a ~n ~uplo
      ~ipiv:empty_int32_vec ~work ~lwork:~-1 ~nrhs ~br ~bc ~b
  in
  if info = 0 then int_of_numberxx work.{1}
  else xxsv_err loc n nrhs b (info + 1)

let sysv_opt_lwork ?n ?(up = true) ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.sysv_opt_lwork" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let uplo = get_uplo_char up in
  sysv_get_opt_lwork loc ar ac a n uplo nrhs br bc b

let sysv ?n ?(up = true) ?ipiv ?work ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.sysv" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let uplo = get_uplo_char up in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork = sysv_get_opt_lwork loc ar ac a n uplo nrhs br bc b in
        Vec.create lwork, lwork in
  let info = direct_sysv ~ar ~ac ~a ~n ~uplo ~ipiv ~work ~lwork ~nrhs ~br ~bc ~b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_ind_err loc info
  | -10 -> xxsv_work_err loc lwork
  | _ -> xxsv_err loc n nrhs b (info + 1)

(* SPSV *)

external direct_spsv :
  ofsap : int ->
  ap : vec ->
  n : int ->
  uplo : char ->
  ipiv : int32_vec ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECspsv_stub_bc" "lacaml_NPRECspsv_stub"

let spsv ?n ?(up = true) ?ipiv ?ofsap ap ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.spsv" in
  let ofsap = get_ofs loc ap_str ofsap in
  let n = get_dim_mat_packed loc ap_str ofsap ap n_str n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let info =
    direct_spsv ~ofsap ~ap ~n ~uplo:(get_uplo_char up) ~ipiv ~nrhs ~br ~bc ~b
  in
  if info <> 0 then
    if info > 0 then xxsv_ind_err loc info
    else xxsv_err loc n nrhs b (info - 1) (* only possibility: LDB *)


(* Linear Equations (expert drivers) *)

(* TODO: GESVX *)

(* TODO: GBSVX *)

(* TODO: GTSVX *)

(* TODO: POSVX *)

(* TODO: PPSVX *)

(* TODO: PBSVX *)

(* TODO: PTSVX *)

(* TODO: SYSVX *)

(* TODO: SPSVX *)


(* Least squares (simple drivers) *)

(* GELS *)

external direct_gels :
  ar : int ->
  ac : int ->
  a : mat ->
  m : int ->
  n : int ->
  trans : char ->
  work : vec ->
  lwork : int ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat ->
  int = "lacaml_NPRECgels_stub_bc" "lacaml_NPRECgels_stub"

let gels_min_lwork ~m ~n ~nrhs =
  let min_dim = min m n in
  max 1 (min_dim + max min_dim nrhs)

let gels_err loc ar a m n lwork nrhs br b err =
  let gelsX_err_code = if err = -10 then -12 else err + 1 in
  gelsX_err loc gels_min_lwork ar a m n lwork nrhs br b gelsX_err_code

let gels_get_opt_lwork loc ar ac a m n trans nrhs br bc b =
  let work = Vec.create 1 in
  let info =
    direct_gels ~ar ~ac ~a ~m ~n ~trans ~work ~lwork:~-1 ~nrhs ~br ~bc ~b
  in
  if info = 0 then int_of_numberxx work.{1}
  else gels_err loc ar a m n 1 nrhs br b info

let gels_opt_lwork ?m ?n ?(trans = `N) ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.gels_opt_lwork" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  gels_get_opt_lwork loc ar ac a m n
    (get_trans_char trans) nrhs br bc b

let gels ?m ?n ?work ?(trans = `N) ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.NPREC.gels" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  let trans = get_trans_char trans in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork = gels_get_opt_lwork loc ar ac a m n trans nrhs br bc b in
        Vec.create lwork, lwork in
  let info =
    direct_gels ~ar ~ac ~a ~m ~n ~trans ~work ~lwork ~nrhs ~br ~bc ~b
  in
  if info <> 0 then gels_err loc ar a m n lwork nrhs br b info
