(* File: lacaml_CZ.ml

   Copyright (C) 2005

     Egbert Ammicht
     email: eammicht@lucent.com

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

     Oleg Trott
     email: ot14@columbia.edu
     WWW: http://www.columbia.edu/~ot14

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
open Complex
open Lacaml_floatxx
open Lacaml_complexxx
open Lacaml_common
open Lacaml_utils
open Lacaml4_CPREC

module Vec = Vec4_CPREC
module Mat = Mat4_CPREC

module RVec = Vec4_CBPREC

(* Auxiliary routines *)

external direct_lansy :
  char -> (* NORM *)
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  rvec (* WORK *)
  -> float = "lacaml_CPREClansy_stub_bc" "lacaml_CPREClansy_stub"

let lansy_min_lwork n = function `I -> n | _ -> 0

let lansy ?n ?(up = true) ?(norm = `O) ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.lansy" in
  let n = get_n_of_a loc ar ac a n in
  let norm_char = get_norm_char norm in
  let uplo_char = get_uplo_char up in
  let min_work = lansy_min_lwork n norm in
  let work, _lwork = get_work loc RVec.create work min_work min_work "lwork" in
  direct_lansy norm_char uplo_char n ar ac a work


(* Linear equations (computational routines) *)

(* GECON *)

external direct_gecon :
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  vec -> (* WORK *)
  rvec -> (* RWORK *)
  char -> (* NORM *)
  float -> (* ANORM *)
  int * float = "lacaml_CPRECgecon_stub_bc" "lacaml_CPRECgecon_stub"

let gecon_min_lwork n = 2 * n

let gecon_min_lrwork n = 2 * n

let gecon ?n ?(norm = `O) ?anorm ?work ?rwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gecon" in
  let n = get_n_of_a loc ar ac a n in
  let norm_char = get_norm_char norm in
  let work, _lwork =
    get_work
      loc Vec.create work (gecon_min_lwork n) (gecon_min_lwork n) "lwork" in
  let rwork, _lrwork =
    get_work
      loc RVec.create rwork
      (gecon_min_lrwork n) (gecon_min_lrwork n) "lrwork" in
  let anorm =
    match anorm with
    | None -> lange ~norm:(norm :> norm4) ~m:n ~n ~work:rwork a
    | Some anorm -> anorm in
  let info, rcond = direct_gecon n ar ac a work rwork norm_char anorm in
  if info = 0 then rcond
  else gecon_err loc norm_char n a info

(* SYCON *)

external direct_sycon :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  int_vec -> (* IPIV *)
  vec -> (* WORK *)
  float -> (* ANORM *)
  int * float = "lacaml_CPRECsycon_stub_bc" "lacaml_CPRECsycon_stub"

let sycon_min_lwork n = 2 * n

let sycon ?n ?(up = true) ?ipiv ?anorm ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.sycon" in
  let n = get_n_of_a loc ar ac a n in
  let uplo_char = get_uplo_char up in
  let work, _lwork =
    get_work
      loc Vec.create work (sycon_min_lwork n) (sycon_min_lwork n) "lwork" in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~work ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n in
  let anorm =
    match anorm with
    | None -> lange ~m:n ~n ~ar ~ac a
    | Some anorm -> anorm in
  let info, rcond = direct_sycon uplo_char n ar ac a ipiv work anorm in
  if info = 0 then rcond
  else xxcon_err loc n a info

(* POCON *)

external direct_pocon :
  char -> (* UPLO *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  vec -> (* WORK *)
  rvec -> (* RWORK *)
  float -> (* ANORM *)
  int * float = "lacaml_CPRECpocon_stub_bc" "lacaml_CPRECpocon_stub"

let pocon_min_lwork n = 3 * n

let pocon_min_lrwork n = n

let pocon ?n ?(up = true) ?anorm ?work ?rwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.pocon" in
  let n = get_n_of_a loc ar ac a n in
  let uplo_char = get_uplo_char up in
  let min_lwork, min_lrwork = pocon_min_lwork n, pocon_min_lrwork n in
  let work, _lwork =
    get_work loc Vec.create work min_lwork min_lwork "lwork" in
  let rwork, _lrwork =
    get_work loc RVec.create rwork min_lrwork min_lrwork "lrwork" in
  let anorm =
    match anorm with
    | None -> lange ~m:n ~n ~ar ~ac a
    | Some anorm -> anorm in
  let info, rcond = direct_pocon uplo_char n ar ac a work rwork anorm in
  if info = 0 then rcond
  else xxcon_err loc n a info


(* General SVD routines *)

(* GESVD *)

external direct_gesvd :
  char -> (* JOBU *)
  char -> (* JOBVT *)
  int -> (* M *)
  int -> (* N *)
  int -> (* AR *)
  int -> (* AC *)
  mat -> (* A *)
  rvec -> (* S *)
  int -> (* UR *)
  int -> (* UC *)
  mat -> (* U *)
  int -> (* VTC *)
  int -> (* VTR *)
  mat -> (* VT *)
  vec -> (* WORK *)
  int -> (* LWORK *)
  rvec (* RWORK *)
  -> int = "lacaml_CPRECgesvd_stub_bc" "lacaml_CPRECgesvd_stub"

let gesvd_min_lwork ~m ~n =
  let min_m_n = min m n in
  max 1 (min_m_n + min_m_n + max m n)

let gesvd_lrwork ~m ~n = 5 * min m n

let gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt =
  let lwork = -1 in
  let dummy_work = Vec.create 1 in
  let info =
    direct_gesvd
      jobu jobvt m n ar ac a s ur uc u vtr vtc vt dummy_work lwork RVec.empty in
  if info = 0 then int_of_floatxx dummy_work.{1}.re
  else gesvd_err loc jobu jobvt m n a u vt lwork info

let gesvd_opt_lwork
    ?m ?n
    ?(jobu = `A) ?(jobvt = `A) ?s
    ?(ur = 1) ?(uc = 1) ?u
    ?(vtr = 1) ?(vtc = 1) ?vt ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gesvd_opt_lwork" in
  let jobu, jobvt, m, n, s, u, vt =
    gesvd_get_params
      loc RVec.create Mat.create jobu jobvt m n ar ac a s ur uc u vtr vtc vt in
  gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt

let gesvd
    ?m ?n
    ?(jobu = `A) ?(jobvt = `A) ?s
    ?(ur = 1) ?(uc = 1) ?u
    ?(vtr = 1) ?(vtc = 1) ?vt ?work ?rwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gesvd" in
  let jobu, jobvt, m, n, s, u, vt =
    gesvd_get_params
      loc RVec.create Mat.create jobu jobvt m n ar ac a s ur uc u vtr vtc vt in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork =
          gesvd_get_opt_lwork
            loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt in
        Vec.create lwork, lwork in
  let rwork =
    match rwork with
    | None -> RVec.create (gesvd_lrwork ~m ~n)
    | Some rwork ->
        let lrwork = Array1.dim rwork in
        let min_lrwork = gesvd_lrwork ~m ~n in
        if lrwork < min_lrwork then
          invalid_arg
            (sprintf "%s: lrwork: valid=[%d..[ got=%d" loc min_lrwork lrwork)
        else rwork in
  let info =
    direct_gesvd jobu jobvt m n ar ac a s ur uc u vtc vtr vt work lwork rwork in
  if info = 0 then s, u, vt
  else gesvd_err loc jobu jobvt m n a u vt lwork info


(* General eigenvalue problem (simple drivers) *)

(* GEEV error handler *)

let geev_err loc min_work a n vl vr lwork err =
  if err > 0 then
    let msg =
      sprintf "\
        %s: The QR algorithm failed to compute all the eigenvalues, and\n\
        no eigenvectors have been computed; elements %d:%d of WR and WI\n\
        contain eigenvalues which have converged" loc (err + 1) n in
    failwith msg
  else
    let msg =
      match err with
      | -3 -> sprintf "n: valid=[0..[ got=%d" n
      | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
      | -8 -> sprintf "dim1(vl): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 vl)
      | -10-> sprintf "dim1(vr): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 vr)
      | -12 -> sprintf "dim(work): valid=[%d..[ got=%d" (min_work n) lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

(* GEEV *)

external direct_geev :
  int -> int -> mat -> (* AR, AC, A *)
  int -> (* N *)
  int -> vec -> (* OFSW, W *)
  int -> int -> mat -> char -> (* VLR, VLC, VL, JOBVL *)
  int -> int -> mat -> char -> (* VRR, VRC, VR, JOBVR *)
  vec -> int -> (* WORK, LWORK *)
  vec (* RWORK *)
  -> int = "lacaml_CPRECgeev_stub_bc" "lacaml_CPRECgeev_stub"

let geev_min_lwork n = max 1 (n + n)
let geev_min_lrwork n = n + n

let geev_get_opt_lwork loc n leftr leftc vl jobvl rightr rightc vr jobvr
    ofsw w ar ac a =
  let dummy_work = Vec.create 1 in
  let info =
    direct_geev ar ac a n ofsw w leftr leftc vl jobvl rightr rightc vr jobvr
      dummy_work (-1) Vec.empty in
  if info = 0 then int_of_float dummy_work.{1}.re
  else geev_err loc geev_min_lwork a n vl vr (-1) info

let geev_get_params loc ar ac a n leftr leftc left rightr rightc right ofsw w =
  let n, _, _, _, _, _, _, _, _, _ as params =
    geev_gen_get_params
      loc Mat.empty Mat.create ar ac a n leftr leftc left rightr rightc right in
  params, xxev_get_wx Vec.create loc "w" ofsw w n

let geev_opt_lwork
    ?n
    ?(leftr = 1) ?(leftc = 1) ?left
    ?(rightr = 1) ?(rightc = 1) ?right
    ?(ofsw = 1) ?w
    ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.geev_opt_lwork" in
  let (n, leftr, leftc, vl, jobvl, rightr, rightc, vr, jobvr, _), (ofsw, w) =
    geev_get_params loc ar ac a n leftr leftc left rightr rightc right ofsw w in
  geev_get_opt_lwork
    loc n leftr leftc vl jobvl rightr rightc vr jobvr ofsw w ar ac a

let geev
    ?n ?work ?rwork
    ?(leftr = 1) ?(leftc = 1) ?left
    ?(rightr = 1) ?(rightc = 1) ?right
    ?(ofsw = 1) ?w
    ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.geev" in
  let (n, leftr, leftc, vl, jobvl, rightr, rightc, vr, jobvr, _), (ofsw, w) =
    geev_get_params loc ar ac a n leftr leftc left rightr rightc right ofsw w in

  let work, lwork =
    match work with
    | Some work ->
        let lwork = Array1.dim work in
        let min_lwork = geev_min_lwork n in
        if lwork < min_lwork then
          invalid_arg
            (sprintf "%s: lwork: valid=[%d..[ got=%d" loc min_lwork lwork)
        else work, lwork
    | None ->
        let lwork =
          geev_get_opt_lwork loc n leftr leftc vl jobvl rightr rightc vr jobvr
            ofsw w ar ac a in
        Vec.create lwork, lwork in

  let rwork =
    match rwork with
    | None -> Vec.create (geev_min_lrwork n)
    | Some rwork ->
        let lrwork = Array1.dim rwork in
        let min_lrwork = geev_min_lrwork n in
        if lrwork < min_lrwork then
          invalid_arg
            (sprintf "%s: lrwork: valid=[%d..[ got=%d" loc min_lrwork lrwork)
        else rwork in

  let info =
    direct_geev ar ac a n ofsw w leftr leftc vl jobvl rightr rightc vr jobvr
      work lwork rwork in

  if info = 0 then vl, w, vr
  else geev_err loc geev_min_lwork a n vl vr lwork info
