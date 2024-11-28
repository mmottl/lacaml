(* File: impl_CZ.ml

   Copyright © 2005-

   Egbert Ammicht <eammicht@lucent.com>

   Markus Mottl <markus.mottl@gmail.com>

   Liam Stewart <liam@cs.toronto.edu>

   Christophe Troestler <Christophe.Troestler@umons.ac.be>

   Oleg Trott <ot14@columbia.edu>

   Florent Hoareau <h.florent@gmail.com>

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

open Printf
open Bigarray
open Complex
open Complexxx
open Common
open Utils
open Impl4_CPREC
module Vec = Vec4_CPREC
module Mat = Mat4_CPREC
module RVec = Vec4_CBPREC

(* BLAS-1 interface *)

external direct_dotu :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  Complex.t = "lacaml_CPRECdotu_stub_bc" "lacaml_CPRECdotu_stub"

external direct_dotc :
  n:(int[@untagged]) ->
  ofsx:(int[@untagged]) ->
  incx:(int[@untagged]) ->
  x:vec ->
  ofsy:(int[@untagged]) ->
  incy:(int[@untagged]) ->
  y:vec ->
  Complex.t = "lacaml_CPRECdotc_stub_bc" "lacaml_CPRECdotc_stub"

let gen_dot loc dot_fun =
  ();
  fun ?n ?ofsx ?incx x ?ofsy ?incy y ->
    let ofsx, incx = get_vec_geom loc x_str ofsx incx in
    let ofsy, incy = get_vec_geom loc y_str ofsy incy in
    let n = get_dim_vec loc x_str ofsx incx x n_str n in
    check_vec loc y_str y (ofsy + ((n - 1) * abs incy));
    dot_fun ~n ~ofsx ~incx ~x ~ofsy ~incy ~y

let dotu = gen_dot "Lacaml.CPREC.dotu" direct_dotu
let dotc = gen_dot "Lacaml.CPREC.dotc" direct_dotc

(* Auxiliary routines *)

external direct_lansy :
  norm:char ->
  uplo:char ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  work:rvec ->
  (float[@unboxed]) = "lacaml_CPREClansy_stub_bc" "lacaml_CPREClansy_stub"

let lansy_min_lwork n = function `I -> n | _ -> 0

let lansy ?n ?(up = true) ?(norm = `O) ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.lansy" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let min_work = lansy_min_lwork n norm in
  let work, _lwork = get_work loc RVec.create work min_work min_work "lwork" in
  let norm = get_norm_char norm in
  direct_lansy ~norm ~uplo ~n ~ar ~ac ~a ~work

(* Linear equations (computational routines) *)

(* GECON *)

external direct_gecon :
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  work:vec ->
  rwork:rvec ->
  norm:char ->
  anorm:(float[@unboxed]) ->
  int * float = "lacaml_CPRECgecon_stub_bc" "lacaml_CPRECgecon_stub"

let gecon_min_lwork n = 2 * n
let gecon_min_lrwork n = 2 * n

let gecon ?n ?(norm = `O) ?anorm ?work ?rwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gecon" in
  let n = get_n_of_a loc ar ac a n in
  let work, _lwork =
    get_work loc Vec.create work (gecon_min_lwork n) (gecon_min_lwork n) "lwork"
  in
  let rwork, _lrwork =
    get_work loc RVec.create rwork (gecon_min_lrwork n) (gecon_min_lrwork n)
      "lrwork"
  in
  let anorm =
    match anorm with
    | None -> lange ~norm:(norm :> norm4) ~m:n ~n ~work:rwork a
    | Some anorm -> anorm
  in
  let norm = get_norm_char norm in
  let info, rcond = direct_gecon ~n ~ar ~ac ~a ~work ~rwork ~norm ~anorm in
  if info = 0 then rcond else gecon_err loc norm n a info

(* SYCON *)

external direct_sycon :
  uplo:char ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  ipiv:int32_vec ->
  work:vec ->
  anorm:(float[@unboxed]) ->
  int * float = "lacaml_CPRECsycon_stub_bc" "lacaml_CPRECsycon_stub"

let sycon_min_lwork n = 2 * n

let sycon ?n ?(up = true) ?ipiv ?anorm ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.sycon" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let work, _lwork =
    get_work loc Vec.create work (sycon_min_lwork n) (sycon_min_lwork n) "lwork"
  in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~work ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n
  in
  let anorm =
    match anorm with None -> lange ~m:n ~n ~ar ~ac a | Some anorm -> anorm
  in
  let info, rcond = direct_sycon ~uplo ~n ~ar ~ac ~a ~ipiv ~work ~anorm in
  if info = 0 then rcond else xxcon_err loc n a info

(* POCON *)

external direct_pocon :
  uplo:char ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  work:vec ->
  rwork:rvec ->
  anorm:(float[@unboxed]) ->
  int * float = "lacaml_CPRECpocon_stub_bc" "lacaml_CPRECpocon_stub"

let pocon_min_lwork n = 3 * n
let pocon_min_lrwork n = n

let pocon ?n ?(up = true) ?anorm ?work ?rwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.pocon" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let min_lwork, min_lrwork = (pocon_min_lwork n, pocon_min_lrwork n) in
  let work, _lwork = get_work loc Vec.create work min_lwork min_lwork "lwork" in
  let rwork, _lrwork =
    get_work loc RVec.create rwork min_lrwork min_lrwork "lrwork"
  in
  let anorm =
    match anorm with None -> lange ~m:n ~n ~ar ~ac a | Some anorm -> anorm
  in
  let info, rcond = direct_pocon ~uplo ~n ~ar ~ac ~a ~work ~rwork ~anorm in
  if info = 0 then rcond else xxcon_err loc n a info

(* General Schur factorization *)

(* GEES *)

external direct_gees :
  jobvs:char ->
  sort:char ->
  select:(int[@untagged]) ->
  select_fun:(Complex.t -> bool) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  w:vec ->
  vsr:(int[@untagged]) ->
  vsc:(int[@untagged]) ->
  vs:mat ->
  work:vec ->
  lwork:(int[@untagged]) ->
  rwork:rvec ->
  bwork:int32_vec ->
  int * int = "lacaml_CPRECgees_stub_bc" "lacaml_CPRECgees_stub"
(* result : (SDIM, INFO) *)

external init_gees : unit -> unit = "lacaml_CPRECinit_gees"

let () = init_gees ()

let gees_get_opt_lwork ~loc ~jobvs ~sort ~select ~select_fun ~n ~ar ~ac ~a ~w
    ~vsr ~vsc ~vs ~rwork ~bwork =
  let lwork = -1 in
  let work = Vec.create 1 in
  let _, info =
    direct_gees ~jobvs ~sort ~select ~select_fun ~n ~ar ~ac ~a ~w ~vsr ~vsc ~vs
      ~work ~lwork ~rwork ~bwork
  in
  if info = 0 then int_of_float work.{1}.re else gees_err loc n info jobvs sort

let gees ?n ?(jobvs = `Compute_Schur_vectors) ?(sort = `No_sort) ?w ?(vsr = 1)
    ?(vsc = 1) ?vs ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gees" in
  let jobvs, sort_char, select, select_fun, n, vs, w =
    gees_get_params_complex loc Vec.create Mat.create Mat.empty jobvs sort n ar
      ac a w vsr vsc vs
  in
  let bwork =
    match sort with `No_sort -> empty_int32_vec | _ -> create_int32_vec n
  in
  let rwork = RVec.create n in
  let work, lwork =
    match work with
    | Some work -> (work, Array1.dim work)
    | None ->
        let lwork =
          gees_get_opt_lwork ~loc ~jobvs ~sort:sort_char ~select ~select_fun ~n
            ~ar ~ac ~a ~w ~vsr ~vsc ~vs ~rwork ~bwork
        in
        (Vec.create lwork, lwork)
  in
  let sdim, info =
    direct_gees ~jobvs ~sort:sort_char ~select ~select_fun ~n ~ar ~ac ~a ~w ~vsr
      ~vsc ~vs ~work ~lwork ~rwork ~bwork
  in
  if info = 0 then (sdim, w, vs) else gees_err loc n info jobvs sort_char

(* General SVD routines *)

(* GESVD *)

external direct_gesvd :
  jobu:char ->
  jobvt:char ->
  m:(int[@untagged]) ->
  n:(int[@untagged]) ->
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  s:rvec ->
  ur:(int[@untagged]) ->
  uc:(int[@untagged]) ->
  u:mat ->
  vtc:(int[@untagged]) ->
  vtr:(int[@untagged]) ->
  vt:mat ->
  work:vec ->
  lwork:(int[@untagged]) ->
  rwork:rvec ->
  (int[@untagged]) = "lacaml_CPRECgesvd_stub_bc" "lacaml_CPRECgesvd_stub"

let gesvd_min_lwork ~m ~n =
  let min_m_n = min m n in
  max 1 (min_m_n + min_m_n + max m n)

let gesvd_lrwork ~m ~n = 5 * min m n

let gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt =
  let lwork = -1 in
  let work = Vec.create 1 in
  let info =
    direct_gesvd ~jobu ~jobvt ~m ~n ~ar ~ac ~a ~s ~ur ~uc ~u ~vtr ~vtc ~vt ~work
      ~lwork ~rwork:RVec.empty
  in
  if info = 0 then Floatxx.int_of_floatxx work.{1}.re
  else gesvd_err loc jobu jobvt m n a u vt lwork info

let gesvd_opt_lwork ?m ?n ?(jobu = `A) ?(jobvt = `A) ?s ?(ur = 1) ?(uc = 1) ?u
    ?(vtr = 1) ?(vtc = 1) ?vt ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gesvd_opt_lwork" in
  let jobu, jobvt, m, n, s, u, vt =
    gesvd_get_params loc RVec.create Mat.create jobu jobvt m n ar ac a s ur uc u
      vtr vtc vt
  in
  gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt

let gesvd ?m ?n ?(jobu = `A) ?(jobvt = `A) ?s ?(ur = 1) ?(uc = 1) ?u ?(vtr = 1)
    ?(vtc = 1) ?vt ?work ?rwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.gesvd" in
  let jobu, jobvt, m, n, s, u, vt =
    gesvd_get_params loc RVec.create Mat.create jobu jobvt m n ar ac a s ur uc u
      vtr vtc vt
  in
  let work, lwork =
    match work with
    | Some work -> (work, Array1.dim work)
    | None ->
        let lwork =
          gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt
        in
        (Vec.create lwork, lwork)
  in
  let rwork =
    match rwork with
    | None -> RVec.create (gesvd_lrwork ~m ~n)
    | Some rwork ->
        let lrwork = Array1.dim rwork in
        let min_lrwork = gesvd_lrwork ~m ~n in
        if lrwork < min_lrwork then
          invalid_arg
            (sprintf "%s: lrwork: valid=[%d..[ got=%d" loc min_lrwork lrwork)
        else rwork
  in
  let info =
    direct_gesvd ~jobu ~jobvt ~m ~n ~ar ~ac ~a ~s ~ur ~uc ~u ~vtc ~vtr ~vt ~work
      ~lwork ~rwork
  in
  if info = 0 then (s, u, vt)
  else gesvd_err loc jobu jobvt m n a u vt lwork info

(* General eigenvalue problem (simple drivers) *)

(* GEEV error handler *)

let geev_err loc min_work a n vl vr lwork err =
  if err > 0 then
    let msg =
      sprintf
        "%s: The QR algorithm failed to compute all the eigenvalues, and\n\
         no eigenvectors have been computed; elements %d:%d of WR and WI\n\
         contain eigenvalues which have converged" loc (err + 1) n
    in
    failwith msg
  else
    let msg =
      match err with
      | -3 -> sprintf "n: valid=[0..[ got=%d" n
      | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
      | -8 -> sprintf "dim1(vl): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 vl)
      | -10 ->
          sprintf "dim1(vr): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 vr)
      | -12 -> sprintf "dim(work): valid=[%d..[ got=%d" (min_work n) lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n))
    in
    invalid_arg (sprintf "%s: %s" loc msg)

(* GEEV *)

external direct_geev :
  ar:(int[@untagged]) ->
  ac:(int[@untagged]) ->
  a:mat ->
  n:(int[@untagged]) ->
  ofsw:(int[@untagged]) ->
  w:vec ->
  vlr:(int[@untagged]) ->
  vlc:(int[@untagged]) ->
  vl:mat ->
  jobvl:char ->
  vrr:(int[@untagged]) ->
  vrc:(int[@untagged]) ->
  vr:mat ->
  jobvr:char ->
  work:vec ->
  lwork:(int[@untagged]) ->
  rwork:vec ->
  (int[@untagged]) = "lacaml_CPRECgeev_stub_bc" "lacaml_CPRECgeev_stub"

let geev_min_lwork n = max 1 (n + n)
let geev_min_lrwork n = n + n

let geev_get_opt_lwork loc n vlr vlc vl jobvl vrr vrc vr jobvr ofsw w ar ac a =
  let work = Vec.create 1 in
  let info =
    direct_geev ~ar ~ac ~a ~n ~ofsw ~w ~vlr ~vlc ~vl ~jobvl ~vrr ~vrc ~vr ~jobvr
      ~work ~lwork:~-1 ~rwork:Vec.empty
  in
  if info = 0 then int_of_float work.{1}.re
  else geev_err loc geev_min_lwork a n vl vr ~-1 info

let geev_get_params loc ar ac a n vlr vlc vl vrr vrc vr ofsw w =
  let ((n, _, _, _, _, _, _, _, _, _) as params) =
    geev_gen_get_params loc Mat.empty Mat.create ar ac a n vlr vlc vl vrr vrc vr
  in
  (params, xxev_get_wx Vec.create loc w_str ofsw w n)

let geev_opt_lwork ?n ?(vlr = 1) ?(vlc = 1) ?vl ?(vrr = 1) ?(vrc = 1) ?vr
    ?(ofsw = 1) ?w ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.geev_opt_lwork" in
  let (n, vlr, vlc, vl, jobvl, vrr, vrc, vr, jobvr, _), w =
    geev_get_params loc ar ac a n vlr vlc vl vrr vrc vr ofsw w
  in
  geev_get_opt_lwork loc n vlr vlc vl jobvl vrr vrc vr jobvr ofsw w ar ac a

let geev ?n ?work ?rwork ?(vlr = 1) ?(vlc = 1) ?vl ?(vrr = 1) ?(vrc = 1) ?vr
    ?(ofsw = 1) ?w ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.CPREC.geev" in
  let (n, vlr, vlc, vl, jobvl, vrr, vrc, vr, jobvr, _), w =
    geev_get_params loc ar ac a n vlr vlc vl vrr vrc vr ofsw w
  in

  let work, lwork =
    match work with
    | Some work ->
        let lwork = Array1.dim work in
        let min_lwork = geev_min_lwork n in
        if lwork < min_lwork then
          invalid_arg
            (sprintf "%s: lwork: valid=[%d..[ got=%d" loc min_lwork lwork)
        else (work, lwork)
    | None ->
        let lwork =
          geev_get_opt_lwork loc n vlr vlc vl jobvl vrr vrc vr jobvr ofsw w ar
            ac a
        in
        (Vec.create lwork, lwork)
  in

  let rwork =
    match rwork with
    | None -> Vec.create (geev_min_lrwork n)
    | Some rwork ->
        let lrwork = Array1.dim rwork in
        let min_lrwork = geev_min_lrwork n in
        if lrwork < min_lrwork then
          invalid_arg
            (sprintf "%s: lrwork: valid=[%d..[ got=%d" loc min_lrwork lrwork)
        else rwork
  in

  let info =
    direct_geev ~ar ~ac ~a ~n ~ofsw ~w ~vlr ~vlc ~vl ~jobvl ~vrr ~vrc ~vr ~jobvr
      ~work ~lwork ~rwork
  in

  if info = 0 then (vl, w, vr)
  else geev_err loc geev_min_lwork a n vl vr lwork info
