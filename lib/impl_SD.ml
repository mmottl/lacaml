(* File: impl_SD.ml

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
open Floatxx
open Common
open Utils
open Impl4_FPREC

module Vec = Vec4_FPREC
module Mat = Mat4_FPREC

(* BLAS-1 *)

(* DOT *)

external direct_dot :
  n : int ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ofsx : int ->
  incx : int ->
  x : vec
  -> float = "lacaml_FPRECdot_stub_bc" "lacaml_FPRECdot_stub"

let dot ?n ?ofsx ?incx ~x ?ofsy ?incy y =
  let loc = "Lacaml.Impl.FPREC.dot" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  check_vec loc y_str y (ofsy + (n - 1) * abs incy);
  direct_dot ~n ~ofsy ~incy ~y ~ofsx ~incx ~x


(* ASUM *)

external direct_asum :
  n : int ->
  ofsx : int ->
  incx : int ->
  x : vec
  -> float = "lacaml_FPRECasum_stub"

let asum ?n ?ofsx ?incx x =
  let loc = "Lacaml.Impl.FPREC.asum" in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let n = get_dim_vec loc x_str ofsx incx x n_str n in
  direct_asum ~n ~ofsx ~incx ~x



(* BLAS-2 *)

(* SBMV *)

external direct_sbmv :
  ofsy : int ->
  incy : int ->
  y : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  k : int ->
  uplo : char ->
  alpha : float ->
  beta : float ->
  ofsx : int ->
  incx : int ->
  x : vec
  -> unit = "lacaml_FPRECsbmv_stub_bc" "lacaml_FPRECsbmv_stub"

let sbmv ?n ?k ?ofsy ?incy ?y ?(ar = 1) ?(ac = 1) a ?(up = true) ?(alpha = 1.0)
    ?(beta = 0.0) ?ofsx ?incx x =
  let loc = "Lacaml.Impl.FPREC.sbmv" in
  (* [a] is a band matrix of size [k+1]*[n]. *)
  let n = get_dim2_mat loc a_str a ac n_str n in
  let k = get_k_mat_sb loc a_str a ar k_str k in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  let y = get_vec loc y_str y ofsy incy n Vec.create in
  check_vec loc x_str x (ofsx + (n - 1) * abs incx);
  direct_sbmv
    ~ofsy ~incy ~y ~ar ~ac ~a ~n ~k ~uplo:(get_uplo_char up)
    ~alpha ~beta ~ofsx ~incx ~x;
  y

(* GER *)

external direct_ger :
  m : int ->
  n : int ->
  alpha : float ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ofsy : int ->
  incy : int ->
  y : vec ->
  ar : int ->
  ac : int ->
  a : mat
  -> unit = "lacaml_FPRECger_stub_bc" "lacaml_FPRECger_stub"

let ger ?m ?n ?(alpha = 1.0) ?ofsx ?incx x ?ofsy ?incy y ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.ger" in
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  check_vec loc x_str x (ofsx + (m - 1) * abs incx);
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  check_vec loc y_str y (ofsy + (n - 1) * abs incy);
  direct_ger ~m ~n ~alpha ~ofsx ~incx ~x ~ofsy ~incy ~y ~ar ~ac ~a;
  a

(* SYR *)

external direct_syr :
  uplo : char ->
  n : int ->
  alpha : float ->
  ofsx : int ->
  incx : int ->
  x : vec ->
  ar : int ->
  ac : int ->
  a : mat
  -> unit = "lacaml_FPRECsyr_stub_bc" "lacaml_FPRECsyr_stub"

let syr ?n ?(alpha = 1.0) ?(up = true) ?ofsx ?incx x ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syr" in
  let n = get_n_of_a loc ar ac a n in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let uplo = get_uplo_char up in
  check_vec loc x_str x (ofsx + (n - 1) * abs incx);
  direct_syr ~uplo ~n ~alpha ~ofsx ~incx ~x ~ar ~ac ~a;
  a


(* LAPACK *)

(* Auxiliary routines *)

external direct_lansy :
  norm : char ->
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  work : vec
  -> float = "lacaml_FPREClansy_stub_bc" "lacaml_FPREClansy_stub"

let lansy_min_lwork n = function
  | `I | `O  -> n
  | _ -> 0

let lansy ?n ?(up = true) ?(norm = `O) ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.lansy" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let min_work = lansy_min_lwork n norm in
  let work, _lwork = get_work loc Vec.create work min_work min_work lwork_str in
  let norm = get_norm_char norm in
  direct_lansy ~norm ~uplo ~n ~ar ~ac ~a ~work

external direct_lamch : char -> float = "lacaml_FPREClamch_stub"

let lamch cmach =
  direct_lamch (
    match cmach with
    | `E -> 'E'
    | `S -> 'S'
    | `B -> 'B'
    | `P -> 'P'
    | `N -> 'N'
    | `R -> 'R'
    | `M -> 'M'
    | `U -> 'U'
    | `L -> 'L'
    | `O -> 'O')


(* Linear equations (computational routines) *)

(* ORGQR *)

external direct_orgqr :
  m : int ->
  n : int ->
  k : int ->
  work : vec ->
  lwork : int ->
  tau : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  int = "lacaml_FPRECorgqr_stub_bc" "lacaml_FPRECorgqr_stub"

let orgqr_min_lwork ~n = max 1 n

let orgqr_get_opt_lwork loc ~m ~n ~k ~tau ~ar ~ac ~a =
  let work = Vec.create 1 in
  let info = direct_orgqr ~m ~n ~k ~work ~lwork:~-1 ~tau ~ar ~ac ~a in
  if info = 0 then int_of_float work.{1}
  else orgqr_err ~loc ~m ~n ~k ~work ~a ~err:info

let orgqr_opt_lwork ?m ?n ?k ~tau ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.orgqr_opt_lwork" in
  let m, n, k = orgqr_get_params loc ?m ?n ?k ~tau ?ar ?ac a in
  orgqr_get_opt_lwork loc ~m ~n ~k ~tau ~ar ~ac ~a

let orgqr ?m ?n ?k ?work ~tau ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.orgqr" in
  let m, n, k = orgqr_get_params loc ?m ?n ?k ~tau ?ar ?ac a in
  let work, lwork =
    let min_lwork = orgqr_min_lwork ~n in
    let opt_lwork = orgqr_get_opt_lwork loc ~m ~n ~k ~tau ~ar ~ac ~a in
    get_work loc Vec.create work min_lwork opt_lwork lwork_str
  in
  let info = direct_orgqr ~m ~n ~k ~work ~lwork ~tau ~ar ~ac ~a in
  if info = 0 then ()
  else orgqr_err ~loc ~m ~n ~k ~work ~a ~err:info


(* ORMQR *)

external direct_ormqr :
  side : char ->
  trans : char ->
  m : int ->
  n : int ->
  k : int ->
  work : vec ->
  lwork : int ->
  tau : vec ->
  ar : int ->
  ac : int ->
  a : mat ->
  cr : int ->
  cc : int ->
  c : mat ->
  int = "lacaml_FPRECormqr_stub_bc" "lacaml_FPRECormqr_stub"

let ormqr_min_lwork ~side ~m ~n =
  match side with
  | `L -> max 1 n
  | `R -> max 1 m

let ormqr_get_opt_lwork loc ~side ~trans ~m ~n ~k ~tau ~ar ~ac ~a ~cr ~cc ~c =
  let work = Vec.create 1 in
  let info =
    let side = get_side_char side in
    let trans = get_trans_char trans in
    direct_ormqr
      ~side ~trans ~m ~n ~k ~work ~lwork:~-1 ~tau ~ar ~ac ~a ~cr ~cc ~c
  in
  if info = 0 then int_of_float work.{1}
  else ormqr_err ~loc ~side ~m ~n ~k ~lwork:1 ~a ~c ~err:info

let ormqr_opt_lwork
      ?(side = `L) ?m ?n ?k ~tau ?(ar = 1) ?(ac = 1) a ?(cr = 1) ?(cc = 1) c =
  let loc = "Lacaml.Impl.FPREC.ormqr_opt_lwork" in
  let m, n, k = ormqr_get_params loc ~side ?m ?n ?k ~tau ~ar ~ac a ~cr ~cc c in
  ormqr_get_opt_lwork loc ~side ~m ~n ~k ~tau ~ar ~ac ~a

let ormqr
      ?(side = `L) ?(trans = `N) ?m ?n ?k ?work ~tau ?(ar = 1) ?(ac = 1) a
      ?(cr = 1) ?(cc = 1) c =
  let loc = "Lacaml.Impl.FPREC.ormqr" in
  let m, n, k = ormqr_get_params loc ~side ?m ?n ?k ~tau ~ar ~ac a ~cr ~cc c in
  let work, lwork =
    let min_lwork = ormqr_min_lwork ~side ~m ~n in
    let opt_lwork =
      ormqr_get_opt_lwork loc ~side ~trans ~m ~n ~k ~tau ~ar ~ac ~a ~cr ~cc ~c
    in
    get_work loc Vec.create work min_lwork opt_lwork lwork_str
  in
  let info =
    let side = get_side_char side in
    let trans = get_trans_char trans in
    direct_ormqr ~side ~trans ~m ~n ~k ~work ~lwork ~tau ~ar ~ac ~a ~cr ~cc ~c
  in
  if info = 0 then ()
  else ormqr_err ~loc ~side ~m ~n ~k ~lwork ~a ~c ~err:info


(* GECON *)

external direct_gecon :
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  work : vec ->
  iwork : int_vec ->
  norm : char ->
  anorm : float ->
  int * float = "lacaml_FPRECgecon_stub_bc" "lacaml_FPRECgecon_stub"

let gecon_min_lwork n = 4 * n

let gecon_min_liwork n = n

let gecon ?n ?(norm = `O) ?anorm ?work ?iwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.gecon" in
  let n = get_n_of_a loc ar ac a n in
  let work, _lwork =
    let min_lwork = gecon_min_lwork n in
    get_work loc Vec.create work min_lwork min_lwork lwork_str
  in
  let iwork, _liwork =
    get_work
      loc Common.create_int_vec iwork
      (gecon_min_liwork n) (gecon_min_liwork n) liwork_str in
  let anorm =
    match anorm with
    | None -> lange ~norm:(norm :> norm4) ~m:n ~n a
    | Some anorm -> anorm in
  let norm = get_norm_char norm in
  let info, rcond = direct_gecon ~n ~ar ~ac ~a ~work ~iwork ~norm ~anorm in
  if info = 0 then rcond
  else gecon_err loc norm n a info

(* SYCON *)

external direct_sycon :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  ipiv : int_vec ->
  work : vec ->
  iwork : int_vec ->
  anorm : float ->
  int * float = "lacaml_FPRECsycon_stub_bc" "lacaml_FPRECsycon_stub"

let sycon_min_lwork n = 2 * n

let sycon_min_liwork n = n

let sycon ?n ?(up = true) ?ipiv ?anorm ?work ?iwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.sycon" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let work, _lwork =
    get_work
      loc Vec.create work (sycon_min_lwork n) (sycon_min_lwork n) lwork_str in
  let iwork, _liwork =
    get_work
      loc Common.create_int_vec iwork
      (sycon_min_liwork n) (sycon_min_liwork n) liwork_str in
  let ipiv =
    if ipiv = None then sytrf ~n ~up ~work ~ar ~ac a
    else sytrf_get_ipiv loc ipiv n in
  let anorm =
    match anorm with
    | None -> lange ~m:n ~n ~work ~ar ~ac a
    | Some anorm -> anorm in
  let info, rcond =
    direct_sycon ~uplo ~n ~ar ~ac ~a ~ipiv ~work ~iwork ~anorm
  in
  if info = 0 then rcond
  else xxcon_err loc n a info

(* POCON *)

external direct_pocon :
  uplo : char ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  work : vec ->
  iwork : int_vec ->
  anorm : float ->
  int * float = "lacaml_FPRECpocon_stub_bc" "lacaml_FPRECpocon_stub"

let pocon_min_lwork n = 3 * n

let pocon_min_liwork n = n

let pocon ?n ?(up = true) ?anorm ?work ?iwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.pocon" in
  let n = get_n_of_a loc ar ac a n in
  let uplo = get_uplo_char up in
  let work, _lwork =
    get_work
      loc Vec.create work (pocon_min_lwork n) (pocon_min_lwork n) lwork_str in
  let iwork, _liwork =
    get_work
      loc Common.create_int_vec iwork
      (pocon_min_liwork n) (pocon_min_liwork n) liwork_str in
  let anorm =
    match anorm with
    | None -> lange ~m:n ~n ~work ~ar ~ac a
    | Some anorm -> anorm in
  let info, rcond = direct_pocon ~uplo ~n ~ar ~ac ~a ~work ~iwork ~anorm in
  if info = 0 then rcond
  else xxcon_err loc n a info


(* Least squares (expert drivers) *)

(* GELSY *)

external direct_gelsy :
  ar : int ->
  ac : int ->
  a : mat ->
  m : int ->
  n : int ->
  jpvt : int_vec ->
  rcond : float ->
  work : vec ->
  lwork : int ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat
  -> int * int = "lacaml_FPRECgelsy_stub_bc" "lacaml_FPRECgelsy_stub"

let gelsy_min_lwork ~m ~n ~nrhs =
  let mn = min m n in
  max (mn + 3*n + 1) (2*mn + nrhs)

let gelsy_get_opt_lwork loc ar ac a m n nrhs br bc b =
  let work = Vec.create 1 in
  let info, _ =
    direct_gelsy
      ~ar ~ac ~a ~m ~n ~jpvt:empty_int_vec
      ~rcond:(-1.0) ~work ~lwork:~-1 ~nrhs ~br ~bc ~b
  in
  if info = 0 then int_of_float work.{1}
  else gelsX_err loc gelsy_min_lwork ar a m n 1 nrhs br b info

let gelsy_opt_lwork ?m ?n ?(ar = 1) ?(ac = 1) a ?nrhs
    ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.gelsy_opt_lwork" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  gelsy_get_opt_lwork loc ar ac a m n nrhs br bc b

let gelsy ?m ?n ?(ar = 1) ?(ac = 1) a ?(rcond = -1.0)
    ?jpvt ?work ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.gelsy" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in

  let jpvt =
    match jpvt with
    | Some jpvt ->
        let dim_jpvt = Array1.dim jpvt in
        if dim_jpvt < n then
          invalid_arg (sprintf "%s: jpvt: valid=[%d..[ got=%d" loc n dim_jpvt)
        else jpvt
    | None ->
        let jpvt = create_int_vec n in
        Array1.fill jpvt Int32.zero;
        jpvt in

  let work, lwork =
    match work with
    | Some work ->
        let lwork = Array1.dim work in
        let min_lwork = gelsy_min_lwork ~m ~n ~nrhs in
        if lwork < min_lwork then
          invalid_arg
            (sprintf "%s: lwork: valid=[%d..[ got=%d" loc min_lwork lwork)
        else work, lwork
    | None ->
        let lwork = gelsy_get_opt_lwork loc ar ac a m n nrhs br bc b in
        Vec.create lwork, lwork in

  let info, rank =
    direct_gelsy ~ar ~ac ~a ~m ~n ~jpvt ~rcond ~work ~lwork ~nrhs ~br ~bc ~b
  in
  if info = 0 then rank
  else gelsX_err loc gelsy_min_lwork ar a m n lwork nrhs br b info


(* GELSD *)

external direct_gelsd :
  ar : int ->
  ac : int ->
  a : mat ->
  m : int ->
  n : int ->
  ofss : int ->
  s : vec ->
  rcond : float ->
  work : vec ->
  lwork : int ->
  iwork : vec ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat
  -> int * int = "lacaml_FPRECgelsd_stub_bc" "lacaml_FPRECgelsd_stub"

let lg2_10 = log10 2.0
let log2 n = log10 n /. lg2_10

let gelsd_smlsiz = ilaenv 9 "FPRECGELSD" " " 0 0 0 0
let gelsd_smlsiz1 = gelsd_smlsiz + 1
let fgelsd_smlsiz1 = float gelsd_smlsiz1
let gelsd_smlsiz1_2 = gelsd_smlsiz1 * gelsd_smlsiz1

let gelsd_nlvl mn =
  max 0 (int_of_float (log2 (float mn /. fgelsd_smlsiz1)) + 1)

let gelsd_min_lwork ~m ~n ~nrhs =
  let mn = min m n in
  let nlvl = gelsd_nlvl mn in
  12*mn + 2*mn*gelsd_smlsiz + 8*mn*nlvl + mn*nrhs + gelsd_smlsiz1_2

let gelsd_get_min_iwork mn nlvl = 3*mn*nlvl + 11*mn

let gelsd_min_iwork m n =
  let mn = min m n in
  gelsd_get_min_iwork mn (gelsd_nlvl mn)

let gelsd_get_opt_lwork loc ar ac a m n nrhs br bc b =
  let work = Vec.create 1 in
  let info, _ =
    direct_gelsd
      ~ar ~ac ~a ~m ~n ~ofss:1 ~s:Vec.empty ~rcond:(-1.0) ~work
      ~lwork:~-1 ~iwork:Vec.empty
      ~nrhs ~br ~bc ~b
  in
  if info = 0 then
    (* FIXME: LAPACK bug??? *)
    (* int_of_floatxx work.{1} *)
    max (int_of_floatxx work.{1}) (gelsd_min_lwork ~m ~n ~nrhs)
  else gelsX_err loc gelsd_min_lwork ar a m n 1 nrhs br b info

let gelsd_opt_lwork ?m ?n ?(ar = 1) ?(ac = 1) a ?nrhs
    ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.gelsd_opt_lwork" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  gelsd_get_opt_lwork loc ar ac a m n nrhs br bc b

let gelsd ?m ?n ?(rcond = -1.0) ?ofss ?s ?work ?iwork
      ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.gelsd" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  let mn = min m n in
  let ofss = get_ofs loc s_str ofss in
  let s = gelsX_get_s Vec.create loc mn ofss s in

  let iwork =
    let min_iwork = gelsd_get_min_iwork mn (gelsd_nlvl mn) in
    match iwork with
    | Some iwork ->
        let dim_iwork = Array1.dim iwork in
        if dim_iwork < min_iwork then
          invalid_arg
            (sprintf "%s: iwork: valid=[%d..[ got=%d" loc min_iwork dim_iwork)
        else iwork
    | None -> Vec.create min_iwork in

  let work, lwork =
    match work with
    | Some work ->
        let lwork = Array1.dim work in
        let min_lwork = gelsd_min_lwork ~m ~n ~nrhs in
        if lwork < min_lwork then
          invalid_arg
            (sprintf "%s: lwork: valid=[%d..[ got=%d" loc min_lwork lwork)
        else work, lwork
    | None ->
        let lwork = gelsd_get_opt_lwork loc ar ac a m n nrhs br bc b in
        Vec.create lwork, lwork in

  let info, rank =
    direct_gelsd
      ~ar ~ac ~a ~m ~n ~ofss ~s ~rcond ~work ~lwork ~iwork ~nrhs ~br ~bc ~b
  in

  if info = 0 then rank
  else gelsX_err loc gelsd_min_lwork ar a m n lwork nrhs br b info


(* GELSS *)

external direct_gelss :
  ar : int ->
  ac : int ->
  a : mat ->
  m : int ->
  n : int ->
  ofss : int ->
  s : vec ->
  rcond : float ->
  work : vec ->
  lwork : int ->
  nrhs : int ->
  br : int ->
  bc : int ->
  b : mat
  -> int * int = "lacaml_FPRECgelss_stub_bc" "lacaml_FPRECgelss_stub"

let gelss_min_lwork ~m ~n ~nrhs =
  let min_dim = min m n in
  max 1 (3*min_dim + max (max (2*min_dim) (max m n)) nrhs)

let gelss_get_opt_lwork loc ar ac a m n nrhs br bc b =
  let work = Vec.create 1 in
  let info, _ =
    direct_gelss
      ~ar ~ac ~a ~m ~n ~ofss:1 ~s:Vec.empty ~rcond:(-1.0)
      ~work ~lwork:~-1 ~nrhs ~br ~bc ~b
  in
  if info = 0 then int_of_floatxx work.{1}
  else gelsX_err loc gelss_min_lwork ar a m n 1 nrhs br b info

let gelss_opt_lwork ?(ar = 1) ?(ac = 1) a ?m ?n ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.gelss_opt_lwork" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  gelss_get_opt_lwork loc ar ac a m n nrhs br bc b

let gelss ?m ?n ?(rcond = -1.0) ?ofss ?s ?work
      ?(ar = 1) ?(ac = 1) a ?nrhs ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.gelss" in
  let m, n, nrhs = gelsX_get_params loc ar ac a m n nrhs br bc b in
  let ofss = get_ofs loc s_str ofss in
  let s = gelsX_get_s Vec.create loc (min m n) ofss s in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork = gelss_get_opt_lwork loc ar ac a m n nrhs br bc b in
        Vec.create lwork, lwork in
  let info, rank =
    direct_gelss ~ar ~ac ~a ~m ~n ~ofss ~s ~rcond ~work ~lwork ~nrhs ~br ~bc ~b
  in
  if info = 0 then rank
  else gelsX_err loc gelss_min_lwork ar a m n lwork nrhs br b info


(* General SVD routines *)

(* GESVD *)

external direct_gesvd :
  jobu : char ->
  jobvt : char ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  s : vec ->
  ur : int ->
  uc : int ->
  u : mat ->
  vtc : int ->
  vtr : int ->
  vt : mat ->
  work : vec ->
  lwork : int
  -> int = "lacaml_FPRECgesvd_stub_bc" "lacaml_FPRECgesvd_stub"

let gesvd_min_lwork ~m ~n =
  let min_m_n = min m n in
  max 1 (max (3 * min_m_n + max m n) (5 * min_m_n))

let gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt =
  let lwork = -1 in
  let work = Vec.create 1 in
  let info =
    direct_gesvd
      ~jobu ~jobvt ~m ~n ~ar ~ac ~a ~s ~ur ~uc ~u ~vtr ~vtc ~vt ~work ~lwork
  in
  if info = 0 then int_of_floatxx work.{1}
  else gesvd_err loc jobu jobvt m n a u vt lwork info

let gesvd_opt_lwork
    ?m ?n
    ?(jobu = `A) ?(jobvt = `A) ?s
    ?(ur = 1) ?(uc = 1) ?u
    ?(vtr = 1) ?(vtc = 1) ?vt ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.gesvd_opt_lwork" in
  let jobu, jobvt, m, n, s, u, vt =
    gesvd_get_params
      loc Vec.create Mat.create jobu jobvt m n ar ac a s ur uc u vtr vtc vt in
  gesvd_get_opt_lwork loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt

let get_job_svecs mat = function
  | `A | `S -> mat
  | `O | `N -> Mat.empty

let gesvd
    ?m ?n
    ?jobu:(jobu_t = `A) ?jobvt:(jobvt_t = `A) ?s
    ?(ur = 1) ?(uc = 1) ?u
    ?(vtr = 1) ?(vtc = 1) ?vt ?work ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.gesvd" in
  let jobu, jobvt, m, n, s, u, vt =
    gesvd_get_params
      loc Vec.create Mat.create jobu_t jobvt_t m n ar ac a s ur uc u vtr vtc vt
  in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork =
          gesvd_get_opt_lwork
            loc jobu jobvt m n ar ac a s ur uc u vtr vtc vt in
        Vec.create lwork, lwork in
  let info =
    direct_gesvd
      ~jobu ~jobvt ~m ~n ~ar ~ac ~a ~s ~ur ~uc ~u ~vtr ~vtc ~vt ~work ~lwork
  in
  if info = 0 then
    let u = get_job_svecs u jobu_t in
    let vt = get_job_svecs vt jobvt_t in
    s, u, vt
  else gesvd_err loc jobu jobvt m n a u vt lwork info


(* GESDD *)

external direct_gesdd :
  jobz : char ->
  m : int ->
  n : int ->
  ar : int ->
  ac : int ->
  a : mat ->
  s : vec ->
  ur : int ->
  uc : int ->
  u : mat ->
  vtr : int ->
  vtc : int ->
  vt : mat ->
  work : vec ->
  lwork : int ->
  iwork : int_vec
  -> int = "lacaml_FPRECgesdd_stub_bc" "lacaml_FPRECgesdd_stub"

let gesdd_min_lwork ?(jobz = `A) ~m ~n () =
  let min_lwork =
    let min_m_n = min m n in
    let max_m_n = max m n in
    let min_m_n_3 = 3 * min_m_n in
    match jobz with
    | `N -> min_m_n_3 + max max_m_n (min_m_n_3 + min_m_n_3)
    | `O ->
        min_m_n_3 * min_m_n +
          max max_m_n (5*min_m_n*min_m_n + (min_m_n_3 + min_m_n))
    | `S | `A ->
        min_m_n_3 * min_m_n +
          max max_m_n ((min_m_n_3 + min_m_n) * (min_m_n + 1)) in
  max 1 min_lwork

let gesdd_liwork ~m ~n = 8 * min m n

let gesdd_get_iwork loc ~m ~n iwork =
  let min_liwork = gesdd_liwork ~m ~n in
  match iwork with
  | Some iwork ->
      let liwork = Array1.dim iwork in
      if liwork < min_liwork then
        invalid_arg
          (sprintf "%s: liwork: valid=[%d..[ got=%d" loc min_liwork liwork);
      iwork
  | None -> create_int_vec min_liwork

let gesdd_min_lwork_char jobz ~m ~n =
  let jobz =
    match jobz with
    | 'N' -> `N
    | 'O' -> `O
    | 'S' -> `S
    | _ -> `A in
  gesdd_min_lwork ~jobz ~m ~n ()

let gesdd_get_opt_lwork loc jobz ?iwork m n ar ac a s ur uc u vtr vtc vt =
  let iwork = gesdd_get_iwork loc ~m ~n iwork in
  let lwork = -1 in
  let work = Vec.create 1 in
  let info =
    direct_gesdd
      ~jobz ~m ~n ~ar ~ac ~a ~s ~ur ~uc ~u ~vtr ~vtc ~vt ~work ~lwork ~iwork
  in
  if info = 0 then
    (* FIXME: LAPACK bug??? *)
    (* int_of_floatxx work.{1} *)
    max (int_of_floatxx work.{1}) (gesdd_min_lwork_char jobz ~m ~n)
  else gesdd_err loc jobz m n a u vt lwork info

let gesdd_opt_lwork
    ?m ?n
    ?(jobz = `A) ?s
    ?(ur = 1) ?(uc = 1) ?u ?(vtr = 1) ?(vtc = 1) ?vt
    ?iwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.gesdd_opt_lwork" in
  let jobz_c, m, n, s, u, vt =
    gesdd_get_params
      loc Vec.create Mat.create jobz m n ar ac a s ur uc u vtr vtc vt
  in
  gesdd_get_opt_lwork loc jobz_c ?iwork m n ar ac a s ur uc u vtr vtc vt

let gesdd
    ?m ?n
    ?jobz:(jobz_t = `A) ?s
    ?(ur = 1) ?(uc = 1) ?u ?(vtr = 1) ?(vtc = 1) ?vt
    ?work ?iwork ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.gesdd" in
  let jobz, m, n, s, u, vt =
    gesdd_get_params
      loc Vec.create Mat.create jobz_t m n ar ac a s ur uc u vtr vtc vt in
  let iwork = gesdd_get_iwork loc ~m ~n iwork in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork =
          gesdd_get_opt_lwork
            loc jobz ~iwork m n ar ac a s ur uc u vtr vtc vt in
        Vec.create lwork, lwork in
  let info =
    direct_gesdd
      ~jobz ~m ~n ~ar ~ac ~a ~s ~ur ~uc ~u ~vtr ~vtc ~vt ~work ~lwork ~iwork
  in
  if info = 0 then
    match jobz_t with
    | `A | `S -> s, u, vt
    | `O -> if m >= n then s, Mat.empty, vt else s, u, Mat.empty
    | `N -> s, Mat.empty, Mat.empty
  else gesdd_err loc jobz m n a u vt lwork info


(* General eigenvalue problem (simple drivers) *)

(* GEEV error handler *)

let geev_err loc min_work a n vl vr lwork err =
  if err > 0 then
    let msg =
      sprintf "\
        %s: The QR algorithm failed to compute all the eigenvalues,\n\
        and no eigenvectors have been computed; elements %d:%d of WR\n\
        and WI contain eigenvalues which have converged" loc (err + 1) n in
    failwith msg
  else
    let msg =
      match err with
      | -3 -> sprintf "n: valid=[0..[ got=%d" n
      | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
      | -9 -> sprintf "dim1(vl): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 vl)
      | -11-> sprintf "dim1(vr): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 vr)
      | -13 -> sprintf "dim(work): valid=[%d..[ got=%d" (min_work n) lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

(* GEEV *)

external direct_geev :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  ofswr : int ->
  wr : vec ->
  ofswi : int ->
  wi : vec ->
  vlr : int ->
  vlc : int ->
  vl : mat ->
  jobvl : char ->
  vrr : int ->
  vrc : int ->
  vr : mat ->
  jobvr : char ->
  work : vec ->
  lwork : int
  -> int = "lacaml_FPRECgeev_stub_bc" "lacaml_FPRECgeev_stub"

let geev_min_lwork ?(vectors = true) n =
  if vectors then max 1 (4 * n)
  else max 1 (3 * n)

let geev_get_opt_lwork
    ~loc ~n
    ~vlr ~vlc ~vl ~jobvl
    ~vrr ~vrc ~vr ~jobvr
    ~ofswr ~wr
    ~ofswi ~wi
    ~ar ~ac ~a ~vectors =
  let work = Vec.create 1 in
  let info =
    direct_geev
      ~ar ~ac ~a ~n ~ofswr ~wr ~ofswi ~wi ~vlr ~vlc ~vl ~jobvl
      ~vrr ~vrc ~vr ~jobvr ~work ~lwork:~-1
  in
  if info = 0 then int_of_float work.{1}
  else geev_err loc (geev_min_lwork ~vectors) a n vl vr ~-1 info

let geev_get_params
      ~loc ~ar ~ac ~a ~n ~vlr ~vlc ~vl
      ~vrr ~vrc ~vr ~ofswr ~wr ~ofswi ~wi =
  let n, _, _, _, _, _, _, _, _, _ as params =
    geev_gen_get_params
      loc Mat.empty Mat.create ar ac a n vlr vlc vl vrr vrc vr
  in
  (
    params,
    xxev_get_wx Vec.create loc wr_str ofswr wr n,
    xxev_get_wx Vec.create loc wi_str ofswi wi n
  )

let geev_opt_lwork
    ?n
    ?(vlr = 1) ?(vlc = 1) ?vl
    ?(vrr = 1) ?(vrc = 1) ?vr
    ?(ofswr = 1) ?wr
    ?(ofswi = 1) ?wi
    ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.geev_opt_lwork" in
  let (n, vlr, vlc, vl, jobvl, vrr, vrc, vr, jobvr, vectors),
      (ofswr, wr), (ofswi, wi) =
    geev_get_params
      ~loc ~ar ~ac ~a ~n ~vlr ~vlc ~vl ~vrr ~vrc ~vr ~ofswr ~wr ~ofswi ~wi
  in
  geev_get_opt_lwork
    ~loc ~n ~vlr ~vlc ~vl ~jobvl ~vrr ~vrc ~vr ~jobvr ~ofswr ~wr
    ~ofswi ~wi ~ar ~ac ~a ~vectors

let geev
    ?n ?work
    ?(vlr = 1) ?(vlc = 1) ?vl
    ?(vrr = 1) ?(vrc = 1) ?vr
    ?(ofswr = 1) ?wr
    ?(ofswi = 1) ?wi
    ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.geev" in
  let (n, vlr, vlc, vl, jobvl, vrr, vrc, vr, jobvr, vectors),
      (ofswr, wr), (ofswi, wi) =
    geev_get_params
      ~loc ~ar ~ac ~a ~n ~vlr ~vlc ~vl ~vrr ~vrc ~vr ~ofswr ~wr ~ofswi ~wi
  in

  let work, lwork =
    match work with
    | Some work ->
        let lwork = Array1.dim work in
        let min_lwork = geev_min_lwork ~vectors n in
        if lwork < min_lwork then
          invalid_arg
            (sprintf "%s: lwork: valid=[%d..[ got=%d" loc min_lwork lwork)
        else work, lwork
    | None ->
        let lwork =
          geev_get_opt_lwork
            ~loc ~n ~vlr ~vlc ~vl ~jobvl ~vrr ~vrc ~vr ~jobvr
            ~ofswr ~wr ~ofswi ~wi ~ar ~ac ~a ~vectors
        in
        Vec.create lwork, lwork in

  let info =
    direct_geev ~ar ~ac ~a ~n ~ofswr ~wr ~ofswi ~wi ~vlr ~vlc ~vl ~jobvl
      ~vrr ~vrc ~vr ~jobvr ~work ~lwork
  in

  if info = 0 then vl, wr, wi, vr
  else geev_err loc (geev_min_lwork ~vectors) a n vl vr lwork info


(* Symmetric-matrix eigenvalue and singular value problems (simple drivers) *)

(* SYEV? auxiliary functions *)

let syev_err loc min_work a n lwork err =
  if err > 0 then
    let msg =
      sprintf "%s: failed to converge on off-diagonal element %d" loc err in
    failwith msg
  else
    let msg =
      match err with
      | -3 -> sprintf "n: valid=[0..[ got=%d" n
      | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
      | -8 -> sprintf "dim(work): valid=[%d..[ got=%d" (min_work n) lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let syevd_err loc min_work min_iwork a n lwork liwork err =
  if err = -10 then
    let msg =
      sprintf "%s: dim(iwork): valid=[%d..[ got=%d" loc (min_iwork n) liwork in
    invalid_arg msg
  else syev_err loc min_work a n lwork err


(* SYEV *)

external direct_syev :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  jobz : char ->
  uplo : char ->
  ofsw : int ->
  w : vec ->
  work : vec ->
  lwork : int
  -> int = "lacaml_FPRECsyev_stub_bc" "lacaml_FPRECsyev_stub"

let syev_min_lwork n = max 1 (3 * n - 1)

let syev_get_opt_lwork loc ar ac a n jobz uplo =
  let work = Vec.create 1 in
  let info =
    direct_syev ~ar ~ac ~a ~n ~jobz ~uplo ~ofsw:1 ~w:Vec.empty ~work ~lwork:~-1
  in
  if info = 0 then int_of_float work.{1}
  else syev_err loc syev_min_lwork a n ~-1 info

let syev_opt_lwork ?n ?(vectors = false) ?(up = true) ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syev_opt_lwork" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  syev_get_opt_lwork loc ar ac a n jobz uplo

let syev ?n ?(vectors = false) ?(up = true) ?work ?ofsw ?w ?(ar = 1)
    ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syev" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  let ofsw = get_ofs loc w_str ofsw in
  let ofsw, w = xxev_get_wx Vec.create loc w_str ofsw w n in

  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork = syev_get_opt_lwork loc ar ac a n jobz uplo in
        Vec.create lwork, lwork in
  let info = direct_syev ~ar ~ac ~a ~n ~jobz ~uplo ~ofsw ~w ~work ~lwork in
  if info = 0 then w
  else syev_err loc syev_min_lwork a n lwork info


(* SYEVD *)

external direct_syevd :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  jobz : char ->
  uplo : char ->
  ofsw : int ->
  w : vec ->
  work : vec ->
  lwork : int ->
  iwork : int_vec ->
  liwork : int
  -> int = "lacaml_FPRECsyevd_stub_bc" "lacaml_FPRECsyevd_stub"

let syevd_min_lwork ~vectors n =
  if n <= 1 then 1
  else
    if vectors then
      let nn = n * n in
      1 + 6*n + nn + nn
    else n + n + 1

let syevd_min_liwork ~vectors n =
  if n <= 1 || not vectors then 1
  else 3 + 5*n

let syevd_get_opt_l_li_work loc ar ac a n vectors jobz uplo =
  let work = Vec.create 1 in
  let iwork = Common.create_int_vec 1 in
  let info =
    direct_syevd
      ~ar ~ac ~a ~n ~jobz ~uplo ~ofsw:1 ~w:Vec.empty
      ~work ~lwork:~-1 ~iwork ~liwork:~-1
  in
  if info = 0 then int_of_float work.{1}, Int32.to_int iwork.{1}
  else
    syevd_err
      loc (syevd_min_lwork ~vectors) (syevd_min_liwork ~vectors)
      a n ~-1 ~-1 info

let syevd_get_opt_lwork loc ar ac a n vectors jobz uplo =
  fst (syevd_get_opt_l_li_work loc ar ac a n vectors jobz uplo)

let syevd_get_opt_liwork loc ar ac a n vectors jobz uplo =
  snd (syevd_get_opt_l_li_work loc ar ac a n vectors jobz uplo)

let syevd_opt_l_li_work ?n ?(vectors = false) ?(up = true)
    ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syevd_opt_l_li_work" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  syevd_get_opt_l_li_work loc ar ac a n vectors jobz uplo

let syevd_opt_lwork ?n ?vectors ?up ?ar ?ac a =
  fst (syevd_opt_l_li_work ?n ?vectors ?up ?ar ?ac a)

let syevd_opt_liwork ?n ?vectors ?up ?ar ?ac a =
  snd (syevd_opt_l_li_work ?n ?vectors ?up ?ar ?ac a)

let syevd ?n ?(vectors = false) ?(up = true) ?work ?iwork ?ofsw ?w
      ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syevd" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  let ofsw = get_ofs loc w_str ofsw in
  let ofsw, w = xxev_get_wx Vec.create loc w_str ofsw w n in
  let work, iwork, lwork, liwork =
    match work, iwork with
    | Some work, Some iwork ->
        let lwork = Array1.dim work in
        let liwork = Array1.dim iwork in
        work, iwork, lwork, liwork
    | Some work, None ->
        let lwork = Array1.dim work in
        let liwork = syevd_get_opt_liwork loc ar ac a n vectors jobz uplo in
        let iwork = Common.create_int_vec liwork in
        work, iwork, lwork, liwork
    | None, Some iwork ->
        let lwork = syevd_get_opt_lwork loc ar ac a n vectors jobz uplo in
        let work = Vec.create lwork in
        let liwork = Array1.dim iwork in
        work, iwork, lwork, liwork
    | None, None ->
        let lwork, liwork =
          syevd_get_opt_l_li_work loc ar ac a n vectors jobz uplo in
        let work = Vec.create lwork in
        let iwork = Common.create_int_vec liwork in
        work, iwork, lwork, liwork in
  let info =
    direct_syevd ~ar ~ac ~a ~n ~jobz ~uplo ~ofsw ~w ~work ~lwork ~iwork ~liwork
  in
  if info = 0 then w
  else
    syevd_err
      loc (syevd_min_lwork ~vectors) (syevd_min_liwork ~vectors)
      a n lwork liwork info


(* Symmetric-matrix eigenvalue and singular value problems (expert &
   RRR drivers) *)

(* SYEVR *)

external direct_syevr :
  ar : int ->
  ac : int ->
  a : mat ->
  n : int ->
  jobz : char ->
  range : char ->
  uplo : char ->
  vl : float ->
  vu : float ->
  il : int ->
  iu : int ->
  abstol : float ->
  ofsw : int ->
  w : vec ->
  zr : int ->
  zc : int ->
  z : mat ->
  isuppz : int_vec ->
  work : vec ->
  lwork : int ->
  iwork : int_vec ->
  liwork : int
  -> int * int = "lacaml_FPRECsyevr_stub_bc" "lacaml_FPRECsyevr_stub"

let syevr_err loc a n err =
  if err > 0 then
    let msg = sprintf "%s: internal error: %d" loc err in
    failwith msg
  else
    let msg =
      match err with
      | -4 -> sprintf "n: valid=[0..[ got=%d" n
      | -6 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let syevr_min_lwork n = max 1 (26 * n)
let syevr_min_liwork n = max 1 (10 * n)

let syevr_get_params loc n = function
  | `A -> 'A', n, 0., 0., 0, 0
  | `V (vl, vu) ->
      if vl >= vu then
        invalid_arg (sprintf "%s: vl >= vu  (%f >= %f)" loc vl vu);
      'V', n, vl, vu, 0, 0
  | `I (il, iu) ->
      if n = 0 && iu <> 0 then
        invalid_arg (sprintf "%s: n = 0 && iu <> 0  (%d)" loc iu);
      if il < 1 then
        invalid_arg (sprintf "%s: il < 1  (%d)" loc il);
      if iu > n then
        invalid_arg (sprintf "%s: iu > n  (%d > %d)" loc iu n);
      if il > iu then
        invalid_arg (sprintf "%s: il > iu (%d > %d)" loc il iu);
      'I', iu - il + 1, 0., 0., il, iu

let syevr_get_abstol = function Some abstol -> abstol | None -> lamch `S

let syevr_get_opt_l_li_work
      loc ar ac a n jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz =
  let work = Vec.create 1 in
  let iwork = Common.create_int_vec 1 in
  let info, _ =
    direct_syevr
      ~ar ~ac ~a ~n
      ~jobz ~range ~uplo
      ~vl ~vu
      ~il ~iu
      ~abstol
      ~ofsw ~w
      ~zr ~zc ~z
      ~isuppz
      ~work ~lwork:~-1
      ~iwork ~liwork:~-1 in
  if info = 0 then int_of_float work.{1}, Int32.to_int iwork.{1}
  else syevr_err loc a n info

let syevr_get_opt_lwork
      loc ar ac a n jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz =
  fst (
    syevr_get_opt_l_li_work
      loc ar ac a n jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz)

let syevr_get_opt_liwork
      loc ar ac a n jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz =
  snd (
    syevr_get_opt_l_li_work
      loc ar ac a n jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz)

let syevr_opt_l_li_work
    ?n ?(vectors = false) ?(range = `A) ?(up = true) ?(abstol = 0.)
    ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syevr_opt_l_li_work" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  let range, _m, vl, vu, il, iu = syevr_get_params loc n range in
  let zr = 1 in
  let zc = 1 in
  let z = Mat.create n 0 in
  let ofsw = 1 in
  let w = Vec.empty in
  let isuppz = empty_int_vec in
  syevr_get_opt_l_li_work
    loc ar ac a n jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz

let syevr_opt_lwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a =
  fst (syevr_opt_l_li_work ?n ?vectors ?range ?up ?abstol ?ar ?ac a)

let syevr_opt_liwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a =
  snd (syevr_opt_l_li_work ?n ?vectors ?range ?up ?abstol ?ar ?ac a)

let syevr ?n ?(vectors = false) ?(range = `A) ?(up = true) ?abstol ?work ?iwork
      ?ofsw ?w ?(zr = 1) ?(zc = 1) ?z ?isuppz ?(ar = 1) ?(ac = 1) a =
  let loc = "Lacaml.Impl.FPREC.syevr" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  let range, m, vl, vu, il, iu = syevr_get_params loc n range in
  let abstol = syevr_get_abstol abstol in
  let ofsw = get_ofs loc w_str ofsw in
  let ofsw, w = xxev_get_wx Vec.create loc w_str ofsw w n in
  let z = get_mat loc z_str Mat.create zr zc z n m (* order of n, m is ok! *) in
  let isuppz =
    let min_lisuppz_1 = max 1 m in
    let min_lisuppz = min_lisuppz_1 + min_lisuppz_1 in
    match isuppz with
    | None -> create_int_vec min_lisuppz
    | Some isuppz ->
        let lisuppz = Array1.dim isuppz in
        if lisuppz < min_lisuppz then
          invalid_arg (
            sprintf "%s: dim(isuppz): valid=[%d..[ got=%d"
              loc min_lisuppz lisuppz);
        isuppz in
  let work, iwork, lwork, liwork =
    match work, iwork with
    | Some work, Some iwork ->
        let lwork = Array1.dim work in
        let liwork = Array1.dim iwork in
        work, iwork, lwork, liwork
    | Some work, None ->
        let lwork = Array1.dim work in
        let liwork =
          syevr_get_opt_liwork
            loc ar ac a n
            jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz in
        let iwork = Common.create_int_vec liwork in
        work, iwork, lwork, liwork
    | None, Some iwork ->
        let lwork =
          syevr_get_opt_lwork
            loc ar ac a n
            jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz in
        let work = Vec.create lwork in
        let liwork = Array1.dim iwork in
        work, iwork, lwork, liwork
    | None, None ->
        let lwork, liwork =
          syevr_get_opt_l_li_work
            loc ar ac a n
            jobz range uplo vl vu il iu abstol ofsw w zr zc z isuppz in
        let work = Vec.create lwork in
        let iwork = Common.create_int_vec liwork in
        work, iwork, lwork, liwork in
  let info, m =
    direct_syevr
      ~ar ~ac ~a ~n
      ~jobz ~range ~uplo
      ~vl ~vu
      ~il ~iu
      ~abstol
      ~ofsw ~w
      ~zr ~zc ~z
      ~isuppz
      ~work ~lwork
      ~iwork ~liwork
  in
  if info = 0 then m, w, z, isuppz
  else syevr_err loc a n info


(* SYGV *)

let sygv_err loc min_work a b n lwork err =
  if err > n then
    let msg =
      sprintf "%s: leading minor of order %d of b is not positive definite"
        loc (err - n)
    in
    failwith msg
  else if err > 0 then
    let msg =
      sprintf "%s: failed to converge on off-diagonal element %d" loc err
    in
    failwith msg
  else
    let msg =
      match err with
      | -4 -> sprintf "n: valid=[0..[ got=%d" n
      | -6 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
      | -8 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
      | -11 -> sprintf "dim(work): valid=[%d..[ got=%d" (min_work n) lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n))
    in
    invalid_arg (sprintf "%s: %s" loc msg)

let get_itype = function `A_B -> 1 | `AB -> 2 | `BA -> 3

external direct_sygv :
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  n : int ->
  itype : int ->
  jobz : char ->
  uplo : char ->
  ofsw : int ->
  w : vec ->
  work : vec ->
  lwork : int
  -> int = "lacaml_FPRECsygv_stub_bc" "lacaml_FPRECsygv_stub"

let sygv_min_lwork n = max 1 (3 * n - 1)

let sygv_get_opt_lwork loc ar ac a br bc b n itype jobz uplo =
  let work = Vec.create 1 in
  let info =
    direct_sygv ~ar ~ac ~a ~br ~bc ~b ~n ~itype ~jobz ~uplo
      ~ofsw:1 ~w:Vec.empty ~work ~lwork:(-1)
  in
  if info = 0 then int_of_float work.{1}
  else sygv_err loc sygv_min_lwork a b n (-1) info

let sygv_opt_lwork
      ?n ?(vectors = false) ?(up = true) ?(itype = `A_B)
      ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.sygv_opt_lwork" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  check_dim1_mat loc b_str b br n_str n;
  check_dim2_mat loc b_str b bc n_str n;
  let itype = get_itype itype in
  sygv_get_opt_lwork loc ar ac a br bc b n itype jobz uplo

let sygv
      ?n ?(vectors = false) ?(up = true) ?work ?ofsw ?w ?(itype = `A_B)
      ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.sygv" in
  let n, jobz, uplo = xxev_get_params loc ar ac a n vectors up in
  check_dim1_mat loc b_str b br n_str n;
  check_dim2_mat loc b_str b bc n_str n;
  let ofsw = get_ofs loc w_str ofsw in
  let ofsw, w = xxev_get_wx Vec.create loc w_str ofsw w n in
  let itype = get_itype itype in
  let work, lwork =
    match work with
    | Some work -> work, Array1.dim work
    | None ->
        let lwork = sygv_get_opt_lwork loc ar ac a br bc b n itype jobz uplo in
        Vec.create lwork, lwork
  in
  let info =
    direct_sygv
      ~ar ~ac ~a ~br ~bc ~b ~n ~itype ~jobz ~uplo ~ofsw ~w ~work ~lwork
  in
  if info = 0 then w
  else sygv_err loc sygv_min_lwork a b n lwork info

(* SBGV *)

external direct_sbgv :
  ar : int ->
  ac : int ->
  a : mat ->
  br : int ->
  bc : int ->
  b : mat ->
  n : int ->
  ka : int ->
  kb : int ->
  jobz : char ->
  uplo : char ->
  ofsw : int ->
  w : vec ->
  zr : int ->
  zc : int ->
  z : mat ->
  work : vec
  -> int = "lacaml_FPRECsbgv_stub_bc" "lacaml_FPRECsbgv_stub"

let sbgv_min_lwork n = 3 * n

let sbgv_err loc a b n ka kb z err =
  if err > n then
    let msg =
      sprintf "%s: leading minor of order %d of b is not positive definite"
        loc (err - n) in
    failwith msg
  else if err > 0 then
    let msg =
      sprintf "%s: failed to converge on off-diagonal element %d" loc err in
    failwith msg
  else
    let msg =
      match err with
      | -3 -> sprintf "n: valid=[0..[ got=%d" n
      | -4 -> sprintf "ka: valid=[0..[ got=%d" ka
      | -5 -> sprintf "kb: valid=[0..[ got=%d" kb
      | -7 -> sprintf "dim1(a): valid=[%d..[ got=%d" (ka + 1) (Array2.dim1 a)
      | -9 -> sprintf "dim1(b): valid=[%d..[ got=%d" (kb + 1) (Array2.dim1 b)
      | -12 -> sprintf "dim1(vectors): valid=[%d..[ got=%d" n (Array2.dim1 z)
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n))
    in
    invalid_arg (sprintf "%s: %s" loc msg)

let dummy_matrix = Mat.create 1 0
(* The fact that the number of rows is 1 is important, otherwise SBGV
   rejects LDZ as being illegal. *)

let sbgv
    ?n ?ka ?kb ?(zr = 1) ?(zc = 1) ?z ?(up = true) ?work ?ofsw ?w
    ?(ar = 1) ?(ac = 1) a ?(br = 1) ?(bc = 1) b =
  let loc = "Lacaml.Impl.FPREC.sbgv" in
  (* [a] is a band matrix of size [ka+1]*[n]. *)
  let n = get_dim2_mat loc a_str a ac n_str n in
  let ka = get_k_mat_sb loc a_str a ar ka_str ka in
  check_dim2_mat loc b_str b bc n_str n;
  let kb = get_k_mat_sb loc a_str a ar kb_str kb in
  let uplo = get_uplo_char up in
  let jobz, z = match z with
    | None -> job_char_false, dummy_matrix
    | Some z ->
      check_dim_mat loc z_str zr zc z n n;
      job_char_true, z in
  let ofsw = get_ofs loc w_str ofsw in
  let ofsw, w = xxev_get_wx Vec.create loc w_str ofsw w n in
  let work = match work with
    | Some work -> check_vec loc work_str work (3 * n); work
    | None -> Vec.create (3 * n)
  in
  let info =
    direct_sbgv
      ~ar ~ac ~a ~br ~bc ~b ~n ~ka ~kb ~jobz ~uplo ~ofsw ~w ~zr ~zc ~z ~work
  in
  if info = 0 then w
  else sbgv_err loc a b n ka kb z info
