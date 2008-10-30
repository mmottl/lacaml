(* File: impl_SDCZ.ml

   Copyright (C) 2001-

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

open Printf
open Bigarray
open Numberxx
open Common
open Utils

module Vec = Vec4_NPREC
module Mat = Mat4_NPREC

module RVec = Vec4_NBPREC


(* BLAS-1 *)

(* SWAP *)

external direct_swap :
  int -> (* N *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECswap_stub_bc" "lacaml_NPRECswap_stub"

let swap ?n ?ofsx ?incx ~x ?ofsy ?incy y =
  let loc = "Lacaml.Impl.NPREC.swap" in
  let ofsx, incx = get_vec_geom loc "x" ofsx incx in
  let ofsy, incy = get_vec_geom loc "y" ofsy incy in
  let n = get_dim_vec loc "x" ofsx incx x "n" n in
  check_vec loc "y" y (ofsy + (n - 1) * abs incy);
  direct_swap n ofsx incx x ofsy incy y


(* SCAL *)

external direct_scal :
  int -> (* N *)
  num_type -> (* ALPHA *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECscal_stub"

let scal ?n alpha ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.scal" in
  let ofsx, incx = get_vec_geom loc "x" ofsx incx in
  let n = get_dim_vec loc "x" ofsx incx x "n" n in
  direct_scal n alpha ofsx incx x


(* COPY *)

external direct_copy :
  int -> (* N *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECcopy_stub_bc" "lacaml_NPRECcopy_stub"

let copy ?n ?ofsy ?incy ?y ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.copy" in
  let ofsx, incx = get_vec_geom loc "x" ofsx incx in
  let ofsy, incy = get_vec_geom loc "y" ofsy incy in
  let n = get_dim_vec loc "x" ofsx incx x "n" n in
  let y, ofsy, incy =
    let min_dim_y = ofsy + (n - 1) * abs incy in
    match y with
    | Some y -> check_vec loc "y" y min_dim_y; y, ofsy, incy
    | None -> Vec.create min_dim_y, 1, 1 in
  direct_copy n ofsy incy y ofsx incx x;
  y


(* AXPY *)

external direct_axpy :
  int -> (* N *)
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  num_type -> (* ALPHA *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECaxpy_stub_bc" "lacaml_NPRECaxpy_stub"

let axpy ?n ?(alpha = one) ?ofsx ?incx ~x ?ofsy ?incy y =
  let loc = "Lacaml.Impl.NPREC.axpy" in
  let ofsx, incx = get_vec_geom loc "x" ofsx incx in
  let ofsy, incy = get_vec_geom loc "y" ofsy incy in
  let n = get_dim_vec loc "x" ofsx incx x "n" n in
  check_vec loc "y" y (ofsy + (n - 1) * abs incy);
  direct_axpy n ofsy incy y alpha ofsx incx x


(* AMAX *)

external direct_iamax :
  int -> (* N *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> int = "lacaml_NPRECiamax_stub"

let iamax ?n ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.iamax" in
  let ofsx, incx = get_vec_geom loc "x" ofsx incx in
  let n = get_dim_vec loc "x" ofsx incx x "n" n in
  direct_iamax n ofsx incx x

let amax ?n ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.amax" in
  let ofsx, incx = get_vec_geom loc "x" ofsx incx in
  let n = get_dim_vec loc "x" ofsx incx x "n" n in
  x.{direct_iamax n ofsx incx x}


(* BLAS-2 *)

(* GEMV *)

external direct_gemv :
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* M *)
  int -> (* N *)
  char -> (* TRANS *)
  float -> (* ALPHA *)
  float -> (* BETA *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECgemv_stub_bc" "lacaml_NPRECgemv_stub"

let gemv ?m ?n ?(beta = 0.0) ?ofsy ?incy ?y ?(trans = `N) ?(alpha = 1.0)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.gemv" in
  let m, n, ofsx, incx, ofsy, incy, y, trans_char =
    gXmv_get_params loc Vec.make0 ar ac a m n ofsx incx x ofsy incy y trans in
  direct_gemv ofsy incy y ar ac a m n trans_char alpha beta ofsx incx x;
  y


(* GBMV *)

external direct_gbmv :
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* M *)
  int -> (* N *)
  int -> (* KL *)
  int -> (* KU *)
  char -> (* TRANS *)
  float -> (* ALPHA *)
  float -> (* BETA *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECgbmv_stub_bc" "lacaml_NPRECgbmv_stub"

let gbmv ?m ?n ?(beta = 0.0) ?ofsy ?incy ?y ?(trans = `N) ?(alpha = 1.0)
      ?(ar = 1) ?(ac = 1) a kl ku ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.gbmv" in
  check_var_ltz loc "kl" kl;
  check_var_ltz loc "ku" ku;
  let m, n, ofsx, incx, ofsy, incy, y, trans_char =
    gXmv_get_params loc Vec.make0 ar ac a m n ofsx incx x ofsy incy y trans in
  let min_dim1 = kl + ku + 1 in
  if min_dim1 > m then
    invalid_arg (sprintf "%s: m: valid=[%d..[ got=%d" loc min_dim1 m);
  direct_gbmv ofsy incy y ar ac a m n kl ku trans_char alpha beta ofsx incx x;
  y


(* SYMV *)

external direct_symv :
  int -> (* OFSY *)
  int -> (* INCY *)
  vec -> (* Y *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* N *)
  char -> (* TRANS *)
  float -> (* ALPHA *)
  float -> (* BETA *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECsymv_stub_bc" "lacaml_NPRECsymv_stub"

let symv ?n ?(beta = 0.0) ?ofsy ?incy ?y ?(up = true) ?(alpha = 1.0)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.symv" in
  let n, ofsx, incx, ofsy, incy, y, uplo_char =
    symv_get_params loc Vec.make0 ar ac a n ofsx incx x ofsy incy y up in
  direct_symv ofsy incy y ar ac a n uplo_char alpha beta ofsx incx x;
  y


(* TRMV *)

external direct_trmv :
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* N *)
  char -> (* UPLO *)
  char -> (* TRANS *)
  char -> (* DIAG *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECtrmv_stub_bc" "lacaml_NPRECtrmv_stub"

let trmv
      ?n ?(trans = `N) ?(diag = `N) ?(up = true)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.trmv" in
  let n, ofsx, incx, uplo_char, trans_char, diag_char =
    trXv_get_params loc ar ac a n ofsx incx x up trans diag
  in
  direct_trmv ar ac a n uplo_char trans_char diag_char ofsx incx x


(* TRSV *)

external direct_trsv :
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* N *)
  char -> (* UPLO *)
  char -> (* TRANS *)
  char -> (* DIAG *)
  int -> (* OFSX *)
  int -> (* INCX *)
  vec (* X *)
  -> unit = "lacaml_NPRECtrsv_stub_bc" "lacaml_NPRECtrsv_stub"

let trsv
      ?n ?(trans = `N) ?(diag = `N) ?(up = true)
      ?(ar = 1) ?(ac = 1) a ?ofsx ?incx x =
  let loc = "Lacaml.Impl.NPREC.trsv" in
  let n, ofsx, incx, uplo_char, trans_char, diag_char =
    trXv_get_params loc ar ac a n ofsx incx x up trans diag
  in
  direct_trsv ar ac a n uplo_char trans_char diag_char ofsx incx x


(* BLAS-3 *)

(* GEMM *)

external direct_gemm :
  char -> (* TRANSA *)
  char -> (* TRANSB *)
  int -> (* M *)
  int -> (* N *)
  int -> (* K *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat -> (* B *)
  int -> (* CR *)
  int -> (* CC *)
  mat -> (* C *)
  float -> (* ALPHA *)
  float (* BETA *)
  -> unit = "lacaml_NPRECgemm_stub_bc" "lacaml_NPRECgemm_stub"

let gemm ?m ?n ?k ?(beta = 0.0) ?(cr = 1) ?(cc = 1) ?c
      ?(transa = `N) ?(alpha = 1.0) ?(ar = 1) ?(ac = 1) a
      ?(transb = `N) ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.gemm" in
  let m, n, k, transa_char, transb_char, c =
    gemm_get_params loc Mat.make0 ar ac a transa br bc b cr transb cc c m n k in
  direct_gemm transa_char transb_char m n k ar ac a br bc b cr cc c alpha beta;
  c


(* SYMM *)

external direct_symm :
  char -> (* SIDE *)
  char -> (* UPLO *)
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat -> (* B *)
  int -> (* CR *)
  int -> (* CC *)
  mat -> (* C *)
  float -> (* ALPHA *)
  float (* BETA *)
  -> unit = "lacaml_NPRECsymm_stub_bc" "lacaml_NPRECsymm_stub"

let symm ?m ?n ?(side = `L) ?(up = true)
      ?(beta = 0.0) ?(cr = 1) ?(cc = 1) ?c
      ?(alpha = 1.0) ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.symm" in
  let m, n, side_char, uplo_char, c =
    symm_get_params loc Mat.make0 ar ac a br bc b cr cc c m n side up in
  direct_symm side_char uplo_char m n ar ac a br bc b cr cc c alpha beta;
  c


(* TRMM *)

external direct_trmm :
  char -> (* SIDE *)
  char -> (* UPLO *)
  char -> (* TRANS *)
  char -> (* DIAG *)
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat -> (* B *)
  float (* ALPHA *)
  -> unit = "lacaml_NPRECtrmm_stub_bc" "lacaml_NPRECtrmm_stub"

let trmm ?m ?n ?(side = `L) ?(up = true) ?(trans = `N) ?(diag = `N)
      ?(br = 1) ?(bc = 1) ~b ?(alpha = 1.0) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.trmm" in
  let m, n, side_char, uplo_char, trans_char, diag_char =
    trmm_get_params loc ar ac a br bc b m n side up trans diag
  in
  direct_trmm side_char uplo_char trans_char diag_char m n ar ac a br bc b alpha


(* SYRK *)

external direct_syrk :
  char -> (* UPLO *)
  char -> (* TRANS *)
  int -> (* N *)
  int -> (* K *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* CR *)
  int -> (* CC *)
  mat -> (* C *)
  float -> (* ALPHA *)
  float (* BETA *)
  -> unit = "lacaml_NPRECsyrk_stub_bc" "lacaml_NPRECsyrk_stub"

let syrk ?n ?k ?(up = true) ?(beta = 0.0) ?(cr = 1) ?(cc = 1) ?c
      ?(trans = `N) ?(alpha = 1.0) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.syrk" in
  let n, k, uplo_char, trans_char, c =
    syrk_get_params loc Mat.make0 ar ac a cr cc c n k up trans in
  direct_syrk uplo_char trans_char n k ar ac a cr cc c alpha beta;
  c


(* LAPACK *)

(* Auxiliary routines *)

(* LANGE *)

external direct_lange :
  char -> (* NORM *)
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  rvec (* WORK *)
  -> float = "lacaml_NPREClange_stub_bc" "lacaml_NPREClange_stub"

let lange_min_lwork m = function `I -> m | _ -> 0

let lange ?m ?n ?(norm = `O) ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.lange" in
  let m, n = xlange_get_params loc m n ar ac a in
  let norm_char = get_norm_char norm in
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
  direct_lange norm_char m n ar ac a work


(* Linear equations (computational routines) *)

(* GETRF *)

external direct_getrf :
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int_vec (* IPIV *)
  -> int = "lacaml_NPRECgetrf_stub_bc" "lacaml_NPRECgetrf_stub"

let getrf ?m ?n ?ipiv ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.getrf" in
  let m, n = geXrf_get_params loc m n ar ac a in
  let ipiv = getrf_get_ipiv loc ipiv m n in
  let info = direct_getrf m n ar ac a ipiv in
  if info = 0 then ipiv
  else
    if info > 0 then getrf_lu_err loc info
    else getrf_err loc m n a info

(* GETRS *)

external direct_getrs :
  char -> (* TRANS *)
  int -> (* N *)
  int -> (* NRHS *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat -> (* B *)
  int_vec (* IPIV *)
  -> int = "lacaml_NPRECgetrs_stub_bc" "lacaml_NPRECgetrs_stub"

let getrs
    ?n ?ipiv ?(trans = `N) ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.getrs" in
  let trans_char = get_trans_char trans in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  let ipiv =
    if ipiv = None then getrf ~m:n ~n ~ar ~ac a
    else getrf_get_ipiv loc ipiv n n in
  let info = direct_getrs trans_char n nrhs ar ac a br bc b ipiv in
  if info <> 0 then xxtrs_err loc n nrhs a b info

(* GETRI *)

external direct_getri :
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int_vec -> (* IPIV *)
  vec -> (* WORK *)
  int (* LWORK *)
  -> int = "lacaml_NPRECgetri_stub_bc" "lacaml_NPRECgetri_stub"

let getri_min_lwork n = max 1 n

let getri_get_opt_lwork loc n ar ac a =
  let dummy_work = Vec.create 1 in
  let info = direct_getri n ar ac a empty_int_vec dummy_work (-1) in
  if info = 0 then int_of_numberxx dummy_work.{1}
  else getri_err loc getri_min_lwork n a 1 info

let getri_opt_lwork ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.getri_opt_lwork" in
  let n = get_n_of_a loc ar ac a n in
  getri_get_opt_lwork loc n ar ac a

let getri ?n ?ipiv ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.getri" in
  let n = get_n_of_a loc ar ac a n in
  let work, lwork =
    get_work
      loc Vec.create work
      (getri_min_lwork n) (getri_get_opt_lwork loc n ar ac a) "lwork" in
  let ipiv =
    if ipiv = None then getrf ~m:n ~n ~ar ~ac a
    else getrf_get_ipiv loc ipiv n n in
  let info = direct_getri n ar ac a ipiv work lwork in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else getri_err loc getri_min_lwork n a lwork info

(* SYTRF *)

external direct_sytrf :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int_vec -> (* IPIV *)
  vec (* WORK *) ->
  int (* LWORK *)
  -> int = "lacaml_NPRECsytrf_stub_bc" "lacaml_NPRECsytrf_stub"

let sytrf_get_opt_lwork loc uplo n ar ac a =
  let dummy_work = Vec.create 1 in
  let info = direct_sytrf uplo n ar ac a empty_int_vec dummy_work (-1) in
  if info = 0 then int_of_numberxx dummy_work.{1}
  else sytrf_err loc n a info

let sytrf_opt_lwork ?n ?(up = true) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.sytrf_opt_lwork" in
  let uplo_char = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  sytrf_get_opt_lwork loc uplo_char n ar ac a

let sytrf_min_lwork () = 1

let sytrf ?n ?(up = true) ?ipiv ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.sytrf" in
  let uplo_char = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  let ipiv = sytrf_get_ipiv loc ipiv n in
  let work, lwork =
    get_work
      loc Vec.create work
      (sytrf_min_lwork ())
      (sytrf_get_opt_lwork loc uplo_char n ar ac a) "lwork" in
  let info = direct_sytrf uplo_char n ar ac a ipiv work lwork in
  if info = 0 then ipiv
  else
    if info > 0 then sytrf_fact_err loc info
    else sytrf_err loc n a info

(* SYTRS *)

external direct_sytrs :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* NRHS *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat -> (* B *)
  int_vec (* IPIV *)
  -> int = "lacaml_NPRECsytrs_stub_bc" "lacaml_NPRECsytrs_stub"

let sytrs
      ?n ?(up = true) ?ipiv ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.sytrs" in
  let uplo_char = get_uplo_char up in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n in
  let info = direct_sytrs uplo_char n nrhs ar ac a br bc b ipiv in
  if info <> 0 then xxtrs_err loc n nrhs a b info

(* SYTRI *)

external direct_sytri :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int_vec -> (* IPIV *)
  vec (* WORK *)
  -> int = "lacaml_NPRECsytri_stub_bc" "lacaml_NPRECsytri_stub"

let sytri_min_lwork n = n

let sytri ?n ?(up = true) ?ipiv ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.sytri" in
  let uplo_char = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  let work, _lwork =
    get_work
      loc Vec.create work
      (sytri_min_lwork n) (sytri_min_lwork n) "lwork" in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n in
  let info = direct_sytri uplo_char n ar ac a ipiv work in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else xxtri_err loc n a info

(* POTRF *)

external direct_potrf :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat (* A *)
  -> int = "lacaml_NPRECpotrf_stub"

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
  let loc = "Lacaml.Impl.NPREC.potrf" in
  let uplo_char = get_uplo_char up in
  let n = get_n_of_a loc ar ac a n in
  maybe_add_jitter ~loc ?jitter ~ar ~ac ~n a;
  let info = direct_potrf uplo_char n ar ac a in
  if info <> 0 then
    if info > 0 then potrf_chol_err loc info
    else potrf_err loc n a info

(* POTRS *)

external direct_potrs :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* NRHS *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECpotrs_stub_bc" "lacaml_NPRECpotrs_stub"

let potrs
      ?n ?(up = true) ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1)
      ?(factorize = true) ?jitter b =
  let loc = "Lacaml.Impl.NPREC.potrs" in
  let uplo_char = get_uplo_char up in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  if factorize then potrf ~n ~up ~ar ~ac ?jitter a;
  let info = direct_potrs uplo_char n nrhs ar ac a br bc b in
  if info <> 0 then potrs_err loc n nrhs a b info

(* POTRI *)

external direct_potri :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat (* A *)
  -> int = "lacaml_NPRECpotri_stub"

let potri ?n ?(up = true) ?(ar = 1) ?(ac = 1) ?(factorize = true) ?jitter a =
  let loc = "Lacaml.Impl.NPREC.potri" in
  let n = get_n_of_a loc ar ac a n in
  let uplo_char = get_uplo_char up in
  if factorize then potrf ~n ~up ~ar ~ac ?jitter a;
  let info = direct_potri uplo_char n ar ac a in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else xxtri_err loc n a info

(* TRTRS *)

external direct_trtrs :
  char -> (* UPLO *)
  char -> (* TRANS *)
  char -> (* DIAG *)
  int -> (* N *)
  int -> (* NRHS *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECtrtrs_stub_bc" "lacaml_NPRECtrtrs_stub"

let trtrs
      ?n ?(up = true) ?(trans = `N) ?(diag = `N)
      ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.trtrs" in
  let uplo_char = get_uplo_char up in
  let trans_char = get_trans_char trans in
  let diag_char = get_diag_char diag in
  let n, nrhs = xxtrs_get_params loc ar ac a n br bc b nrhs in
  let info =
    direct_trtrs uplo_char trans_char diag_char n nrhs ar ac a br bc b
  in
  if info <> 0 then trtrs_err loc n nrhs a b info

(* TRTRI *)

external direct_trtri :
  char -> (* UPLO *)
  char -> (* DIAG *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat (* A *)
  -> int = "lacaml_NPRECtrtri_stub_bc" "lacaml_NPRECtrtri_stub"

let trtri ?n ?(up = true) ?(diag = `N) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.trtri" in
  let n = get_n_of_a loc ar ac a n in
  let uplo_char = get_uplo_char up in
  let diag_char = get_diag_char diag in
  let info = direct_trtri uplo_char diag_char n ar ac a in
  if info <> 0 then
    if info > 0 then xxtri_singular_err loc info
    else trtri_err loc n a info

(* GEQRF *)

external direct_geqrf :
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  vec -> (* TAU *)
  vec -> (* WORK *)
  int (* LWORK *)
  -> unit = "lacaml_NPRECgeqrf_stub_bc" "lacaml_NPRECgeqrf_stub"

let geqrf_get_opt_lwork m n ar ac a =
  let dummy_work = Vec.create 1 in
  direct_geqrf m n ar ac a dummy_work dummy_work (-1);
  int_of_numberxx dummy_work.{1}

let geqrf_opt_lwork ?m ?n ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.geqrf_opt_lwork" in
  let m, n = geXrf_get_params loc m n ar ac a in
  geqrf_get_opt_lwork m n ar ac a

let geqrf_min_lwork ~n = max 1 n

let geqrf ?m ?n ?work ?tau ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.NPREC.geqrf" in
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
  direct_geqrf m n ar ac a tau work lwork;
  tau


(* Linear equations (simple drivers) *)

(* GESV *)

external direct_gesv :
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* N *)
  int_vec -> (* IPIV *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECgesv_stub_bc" "lacaml_NPRECgesv_stub"

let gesv ?n ?ipiv ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.gesv" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let info = direct_gesv ar ac a n ipiv nrhs br bc b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_lu_err loc info
  | -4 -> xxsv_a_err loc a n
  | _ -> xxsv_err loc n nrhs b info

(* GBSV *)

external direct_gbsv :
  int -> (* ABR *)
  int -> (* ABC *)
  mat -> (* AB *)
  int -> (* N *)
  int -> (* KL *)
  int -> (* KU *)
  int_vec -> (* IPIV *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECgbsv_stub_bc" "lacaml_NPRECgbsv_stub"

let gbsv ?n ?ipiv ?(abr = 1) ?(abc = 1) ab kl ku ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.gbsv" in
  let n = get_n_of_square "ab" loc abr abc ab n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let info = direct_gbsv abr abc ab n kl ku ipiv nrhs br bc b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_lu_err loc info
  | -1 -> xxsv_err loc n nrhs b info
  | -2 -> invalid_arg (sprintf "%s: kl: valid=[0..[ got=%d" loc kl)
  | -3 -> invalid_arg (sprintf "%s: ku: valid=[0..[ got=%d" loc ku)
  | -6 ->
      let msg =
        sprintf "%s: dim1(ab): valid=[%d..[ got=%d"
          loc (2*kl + ku + 1) (Mat.dim1 ab) in
      invalid_arg msg
  | _ -> xxsv_err loc n nrhs b (info + 2)

(* GTSV *)

external direct_gtsv :
  int -> (* OFSDL *)
  vec -> (* DL *)
  int -> (* OFSD *)
  vec -> (* D *)
  int -> (* OFSDU *)
  vec -> (* DU *)
  int -> (* N *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECgtsv_stub_bc" "lacaml_NPRECgtsv_stub"

let gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.gtsv" in
  let ofsdl = get_ofs loc "dl" ofsdl in
  let ofsd = get_ofs loc "d" ofsd in
  let ofsdu = get_ofs loc "du" ofsdu in
  let n = get_dim_vec loc "d" ofsd 1 d "n" n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  check_vec loc "dl" dl (ofsdl + n - 2);
  check_vec loc "du" du (ofsdu + n - 2);
  let info = direct_gtsv ofsdl dl ofsd d ofsdu du n nrhs br bc b in
  if info <> 0 then (
    if info > 0 then xxsv_lu_err loc info
    else xxsv_err loc n nrhs b info)

(* POSV *)

external direct_posv :
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* N *)
  char -> (* UPLO *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECposv_stub_bc" "lacaml_NPRECposv_stub"

let posv ?n ?(up = true) ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.posv" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let info = direct_posv ar ac a n (get_uplo_char up) nrhs br bc b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_pos_err loc info
  | -5 -> xxsv_a_err loc a n
  | _ -> xxsv_err loc n nrhs b (info + 1)

(* PPSV *)

external direct_ppsv :
  int -> (* OFSAP *)
  vec -> (* AP *)
  int -> (* N *)
  char -> (* UPLO *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECppsv_stub_bc" "lacaml_NPRECppsv_stub"

let ppsv ?n ?(up = true) ?ofsap ap ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.ppsv" in
  let ofsap = get_ofs loc "ap" ofsap in
  let n = get_dim_mat_packed loc "ap" ofsap ap "n" n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let info = direct_ppsv ofsap ap n (get_uplo_char up) nrhs br bc b in
  if info <> 0 then
    if info > 0 then xxsv_pos_err loc info
    else xxsv_err loc n nrhs b (info - 1) (* only: LDB *)

(* PBSV *)

external direct_pbsv :
  int -> (* ABR *)
  int -> (* ABC *)
  mat -> (* AB *)
  int -> (* N *)
  int -> (* KD *)
  char -> (* UPLO *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECpbsv_stub_bc" "lacaml_NPRECpbsv_stub"

let pbsv ?n ?(up = true) ?kd ?(abr = 1) ?(abc = 1) ab
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.pbsv" in
  (* [a] is a band matrix of size [k+1]*[n]. *)
  let n = get_dim2_mat loc "ab" ab abc "n" n in
  let kd = get_k_mat_sb loc "ab" ab abr "kd" kd in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let info = direct_pbsv abr abc ab n kd (get_uplo_char up) nrhs br bc b in
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
  int -> (* OFSD *)
  vec -> (* D *)
  int -> (* OFSE *)
  vec -> (* E *)
  int -> (* N *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECptsv_stub_bc" "lacaml_NPRECptsv_stub"

let ptsv ?n ?ofsd d ?ofse e ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.ptsv" in
  let ofsd = get_ofs loc "d" ofsd in
  let ofse = get_ofs loc "e" ofse in
  let n = get_dim_vec loc "d" ofsd 1 d "n" n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  check_vec loc "e" e (ofse + n - 2);
  let info = direct_ptsv ofsd d ofse e n nrhs br bc b in
  if info <> 0 then
    if info > 0 then xxsv_pos_err loc info
    else xxsv_err loc n nrhs b (info - 1)

(* SYSV *)

external direct_sysv :
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* N *)
  char -> (* UPLO *)
  int_vec -> (* IPIV *)
  vec -> (* WORK *)
  int -> (* LWORK *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECsysv_stub_bc" "lacaml_NPRECsysv_stub"

let sysv_get_opt_lwork loc ar ac a n uplo nrhs br bc b =
  let dummy_work = Vec.create 1 in
  let info =
    direct_sysv ar ac a n uplo empty_int_vec dummy_work (-1) nrhs br bc b in
  if info = 0 then int_of_numberxx dummy_work.{1}
  else xxsv_err loc n nrhs b (info + 1)

let sysv_opt_lwork ?n ?(up = true) ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.sysv_opt_lwork" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let uplo = get_uplo_char up in
  sysv_get_opt_lwork loc ar ac a n uplo nrhs br bc b

let sysv ?n ?(up = true) ?ipiv ?work ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.sysv" in
  let n, nrhs = xxsv_get_params loc ar ac a n br bc b nrhs in
  let uplo = get_uplo_char up in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork = sysv_get_opt_lwork loc ar ac a n uplo nrhs br bc b in
        Vec.create lwork, lwork in
  let info = direct_sysv ar ac a n uplo ipiv work lwork nrhs br bc b in
  match info with
  | 0 -> ()
  | i when i > 0 -> xxsv_ind_err loc info
  | -10 -> xxsv_work_err loc lwork
  | _ -> xxsv_err loc n nrhs b (info + 1)

(* SPSV *)

external direct_spsv :
  int -> (* OFSAP *)
  vec -> (* AP *)
  int -> (* N *)
  char -> (* UPLO *)
  int_vec -> (* IPIV *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECspsv_stub_bc" "lacaml_NPRECspsv_stub"

let spsv ?n ?(up = true) ?ipiv ?ofsap ap ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.spsv" in
  let ofsap = get_ofs loc "ap" ofsap in
  let n = get_dim_mat_packed loc "ap" ofsap ap "n" n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  let ipiv = xxsv_get_ipiv loc ipiv n in
  let info = direct_spsv ofsap ap n (get_uplo_char up) ipiv nrhs br bc b in
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
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int -> (* M *)
  int -> (* N *)
  char -> (* TRANS *)
  vec -> (* WORK *)
  int -> (* LWORK *)
  int -> (* NRHS *)
  int -> (* BR *)
  int -> (* BC *)
  mat (* B *)
  -> int = "lacaml_NPRECgels_stub_bc" "lacaml_NPRECgels_stub"

let gels_min_lwork ~m ~n ~nrhs =
  let min_dim = min m n in
  max 1 (min_dim + max min_dim nrhs)

let gels_err loc ar a m n lwork nrhs br b err =
  let gelsX_err_code = if err = -10 then -12 else err + 1 in
  gelsX_err loc gels_min_lwork ar a m n lwork nrhs br b gelsX_err_code

let gels_get_opt_lwork loc ar ac a m n trans_char nrhs br bc b =
  let dummy_work = Vec.create 1 in
  let info = direct_gels ar ac a m n trans_char dummy_work (-1) nrhs br bc b in
  if info = 0 then int_of_numberxx dummy_work.{1}
  else gels_err loc ar a m n 1 nrhs br b info

let gels_opt_lwork ?m ?n ?(trans = `N) ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.gels_opt_lwork" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  gels_get_opt_lwork loc ar ac a m n
    (get_trans_char trans) nrhs br bc b

let gels ?m ?n ?work ?(trans = `N) ?(ar = 1) ?(ac = 1) a
      ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.NPREC.gels" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  let trans_char = get_trans_char trans in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork =
          gels_get_opt_lwork loc ar ac a m n trans_char nrhs br bc b in
        Vec.create lwork, lwork in
  let info = direct_gels ar ac a m n trans_char work lwork nrhs br bc b in
  if info <> 0 then gels_err loc ar a m n lwork nrhs br b info
