(* File: utils.ml

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

     Florent Hoareau
     email: h.florent@gmail.com
     WWW: none

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

(** General auxiliary functions *)

open Printf
open Bigarray
open Lacaml_common

(* Zero-sized dummy vector (int) *)
let empty_int32_vec = create_int32_vec 0

(* Char indicating type of norm to retrieve for XlanYY routines *)
let get_norm_char = function `M -> 'M' | `O -> 'O' | `I -> 'I' | `F -> 'F'

(* Char indicating whether the "U"pper or "L"ower triangle of a matrix
   is stored *)
let get_uplo_char up = if up then 'U' else 'L'

(* Char indicating whether some operation operates on a "N"ormal,
   "T"ransposed or "C"onjugated transposed matrix. *)
let get_trans_char = function `N -> 'N' | `T -> 'T' | `C -> 'C'

(* Char indicating which side of the matrix B matrix A should be on *)
let get_side_char = function `L -> 'L' | `R -> 'R'

(* Char indicating whether a diagonal is unit or non-unit *)
let get_diag_char = function `U -> 'U' | `N -> 'N'

(* Char indicating whether/how the left/right singular vectors
   should be computed *)
let get_s_d_job_char = function `A -> 'A' | `S -> 'S' | `O -> 'O' | `N -> 'N'

(* Char indicating whether the eigen"V"ectors are computed or "N"ot *)
let get_job_char = function true -> 'V' | _ -> 'N'
let job_char_true = get_job_char true
let job_char_false = get_job_char false


(** Preallocated strings (names) *)

let a_str = "a"
let ab_str = "ab"
let alphas_str = "alphas"
let ap_str = "ap"
let b_str = "b"
let br_str = "br"
let bc_str = "bc"
let c_str = "c"
let cr_str = "cr"
let cc_str = "cc"
let d_str = "d"
let dl_str = "dl"
let du_str = "du"
let e_str = "e"
let ipiv_str = "ipiv"
let iseed_str = "iseed"
let k_str = "k"
let ka_str = "ka"
let kb_str = "kb"
let work_str = "work"
let lwork_str = "lwork"
let liwork_str = "liwork"
let k1_str = "k1"
let k2_str = "k2"
let kd_str = "kd"
let kl_str = "kl"
let ku_str = "ku"
let m_str = "m"
let n_str = "n"
let nrhs_str = "nrhs"
let ofs_str = "ofs"
let r_str = "r"
let s_str = "s"
let tau_str = "tau"
let u_str = "u"
let um_str = "um"
let un_str = "un"
let vm_str = "vm"
let vn_str = "vn"
let vs_str = "vs"
let vsr_str = "vsr"
let vsc_str = "vsc"
let vt_str = "vt"
let w_str = "w"
let wi_str = "wi"
let wr_str = "wr"
let x_str = "x"
let y_str = "y"
let z_str = "z"


(** Range checking *)

(** [raise_var_lt0 ~loc ~name var] @raise Invalid_argument to indicate
    that integer variable [var] with name [name] at location [loc] is lower
    than [0]. *)
let raise_var_lt0 ~loc ~name var =
  invalid_arg (sprintf "%s: %s < 0: %d" loc name var)

(** [check_var_lt0 ~loc ~name var] checks whether integer variable [var] with
    name [name] at location [loc] is lower than [0].  @raise Invalid_argument
    in that case. *)
let check_var_lt0 ~loc ~name var = if var < 0 then raise_var_lt0 ~loc ~name var

let check_var_within loc var_name var lb ub c =
  if var < lb then
    invalid_arg (sprintf "%s: %s %s < %s" loc var_name (c var) (c lb))
  else if var > ub then
    invalid_arg (sprintf "%s: %s %s > %s" loc var_name (c var) (c ub))
  else ()


(** Valueless vector checking and allocation functions (do not require a
    vector value as argument *)

(** [calc_vec_min_dim ~n ~ofs ~inc] @return minimum vector dimension given
    offset [ofs], increment [inc], and operation size [n] for a vector. *)
let calc_vec_min_dim ~n ~ofs ~inc =
  if n = 0 then ofs - 1 else ofs + (n - 1) * abs inc

(** [raise_vec_min_dim ~loc ~vec_name ~dim ~min_dim] @raise Invalid_argument
    to indicate that dimension [dim] of a vector with name [vec_name]
    exceeds the minimum [min_dim] at location [loc]. *)
let raise_vec_min_dim ~loc ~vec_name ~dim ~min_dim =
  invalid_arg (
    sprintf "%s: dim(%s): valid=[%d..[ got=%d" loc vec_name min_dim dim)

(** [check_vec_min_dim ~loc ~vec_name ~dim ~min_dim] checks whether vector
    with name [vec_name] and dimension [dim] satisfies minimum dimension
    [min_dim].  @raise Invalid_argument otherwise. *)
let check_vec_min_dim ~loc ~vec_name ~dim ~min_dim =
  if dim < min_dim then raise_vec_min_dim ~loc ~vec_name ~dim ~min_dim

(** [raise_vec_bad_ofs ~loc ~vec_name ~ofs ~max_ofs] @raise Invalid_argument
    to indicate that vector offset [ofs] is invalid (i.e. is outside of
    [1..max_ofs]). *)
let raise_vec_bad_ofs ~loc ~vec_name ~ofs ~max_ofs =
  invalid_arg (
    sprintf "%s: ofs%s: valid=[1..%d] got=%d" loc vec_name max_ofs ofs)

(** [bad_n ~n ~max_n] @return [true] iff [n] is smaller than zero or larger
    than [max_n]. *)
let bad_n ~n ~max_n = n < 0 || n > max_n

(** [bad_ofs ~ofs ~max_ofs] @return [true] iff [ofs] is smaller than one or
    exceeds [max_ofs]. *)
let bad_ofs ~ofs ~max_ofs = ofs < 1 || ofs > max_ofs

(** [bad_inc inc] @return [true] iff [inc] is illegal. *)
let bad_inc inc = inc = 0

(** [check_vec_ofs ~loc ~vec_name ~ofs ~max_ofs] checks whether vector
    offset [ofs] for vector of name [vec_name] is invalid (i.e. outside of
    [1..max_ofs]).  @raise Invalid_argument in that case. *)
let check_vec_ofs ~loc ~vec_name ~ofs ~max_ofs =
  if bad_ofs ~ofs ~max_ofs then raise_vec_bad_ofs ~loc ~vec_name ~ofs ~max_ofs

(** [check_vec_inc ~loc ~vec_name inc] checks whether vector increment [inc]
    for vector of name [vec_name] is invalid (i.e. [0]).  @raise
    Invalid_argument in that case. *)
let check_vec_inc ~loc ~vec_name inc =
  if bad_inc inc then invalid_arg (sprintf "%s: inc%s = 0" loc vec_name)

(** [calc_vec_max_n ~dim ~ofs ~inc] @return maximum operation length [n]
    for a vector given the dimension [dim] of the vector, the offset [ofs],
    and increment [inc].  Assumes that the offset has already been validated
    to not exceed [dim], i.e. the returned [max_n] is at least [1]. *)
let calc_vec_max_n ~dim ~ofs ~inc = 1 + (dim - ofs) / abs inc

(** [calc_vec_opt_max_n ?ofs ?inc dim] @return maximum operation length [n]
    for a vector given the dimension [dim] of the vector, the optional offset
    [ofs], and optional increment [inc].  Assumes that the offset has already
    been validated to not exceed [dim], i.e. the returned [max_n] is at least
    [1]. *)
let calc_vec_opt_max_n ?(ofs = 1) ?(inc = 1) dim = calc_vec_max_n ~dim ~ofs ~inc

(** [raise_max_len ~loc ~len_name ~len ~max_len] @raise Invalid_argument
    that the maximum operation size (e.g. [m] or [n] for vectors and matrices)
    has been exceeded. *)
let raise_max_len ~loc ~len_name ~len ~max_len =
  invalid_arg (sprintf "%s: %s: valid=[0..%d] got=%d" loc len_name max_len len)

(** [check_vec_dim ~loc ~vec_name ~dim ~ofs ~inc ~n_name ~n] checks the vector
    operation length in parameter [n] with name [n_name] at location [loc]
    for vector with name [vec_name] and dimension [dim] given the operation
    offset [ofs] and increment [inc].  @raise Invalid_argument if any
    arguments are invalid. *)
let check_vec_dim ~loc ~vec_name ~dim ~ofs ~inc ~n_name ~n =
  check_vec_inc ~loc ~vec_name inc;
  check_var_lt0 ~loc ~name:n_name n;
  if n = 0 then check_vec_ofs ~loc ~vec_name ~ofs ~max_ofs:(dim + 1)
  else begin
    check_vec_ofs ~loc ~vec_name ~ofs ~max_ofs:dim;
    let max_n = calc_vec_max_n ~dim ~ofs ~inc in
    if n > max_n then raise_max_len ~loc ~len_name:n_name ~len:n ~max_len:max_n
  end

(** [get_vec_n ~loc ~vec_name ~dim ~ofs ~inc ~n_name n] checks or infers
    the vector operation length in the option parameter [n] with name [n_name]
    at location [loc] for vector with name [vec_name] and dimension [dim] given
    the operation offset [ofs] and increment [inc].  @raise Invalid_argument
    if any arguments are invalid. *)
let get_vec_n ~loc ~vec_name ~dim ~ofs ~inc ~n_name = function
  | None when dim = 0 ->
      check_vec_inc ~loc ~vec_name inc;
      if ofs = 1 then dim else raise_vec_bad_ofs ~loc ~vec_name ~ofs ~max_ofs:1
  | None ->
      check_vec_inc ~loc ~vec_name inc;
      if ofs = dim + 1 then 0
      else begin
        check_vec_ofs ~loc ~vec_name ~ofs ~max_ofs:dim;
        calc_vec_max_n ~dim ~ofs ~inc
      end
  | Some n -> check_vec_dim ~loc ~vec_name ~dim ~ofs ~inc ~n_name ~n; n

(** [get_vec_min_dim ~loc ~vec_name ~ofs ~inc ~n] @return minimum vector
    dimension given offset [ofs], increment [inc], and operation size [n]
    for a vector named [vec_name] at location [loc].  @raise Invalid_argument
    if any of the parameters are illegal. *)
let get_vec_min_dim ~loc ~vec_name ~ofs ~inc ~n =
  check_vec_inc ~loc ~vec_name inc;
  if ofs >= 1 then calc_vec_min_dim ~ofs ~inc ~n
  else invalid_arg (sprintf "%s: ofs%s: valid=[1..] got=%d" loc vec_name ofs)

(** [get_vec_start_stop ~ofsx ~incx ~n] @return [(start, stop)] where [start]
    and [stop] reflect the start and stop of an iteration respectively. *)
let get_vec_start_stop ~ofsx ~incx ~n =
  if n = 0 then 0, 0
  else
    if incx > 0 then ofsx, ofsx + n * incx
    else ofsx - (n - 1) * incx, ofsx + incx


(** Valueless matrix checking and allocation functions (do not require a
    matrix value as argument *)

(** [raise_bad_mat_ofs ~loc ~name ~ofs_name ~ofs ~max_ofs] @raise
    Invalid_argument to indicate that a matrix offset [ofs] named [ofs_name]
    for a matrix having [name] is invalid (i.e. is outside of [1..max_ofs]). *)
let raise_bad_mat_ofs ~loc ~name ~ofs_name ~ofs ~max_ofs =
  invalid_arg (
    sprintf "%s: %s%s: valid=[1..%d] got=%d" loc name ofs_name max_ofs ofs)

(** [raise_mat_bad_r ~loc ~mat_name ~r ~max_r] @raise Invalid_argument
    to indicate that matrix row offset [r] is invalid (i.e. is outside of
    [1..max_r]). *)
let raise_mat_bad_r ~loc ~mat_name ~r ~max_r =
  raise_bad_mat_ofs ~loc ~name:mat_name ~ofs_name:r_str ~ofs:r ~max_ofs:max_r

(** [raise_mat_bad_c ~loc ~mat_name ~c ~max_c] @raise Invalid_argument
    to indicate that matrix column offset [c] is invalid (i.e. is outside of
    [1..max_c]). *)
let raise_mat_bad_c ~loc ~mat_name ~c ~max_c =
  raise_bad_mat_ofs ~loc ~name:mat_name ~ofs_name:c_str ~ofs:c ~max_ofs:max_c

(** [check_mat_r ~loc ~vec_name ~r ~max_r] checks whether matrix row
    offset [r] for vector of name [vec_name] is invalid (i.e. outside of
    [1..max_r]).  @raise Invalid_argument in that case. *)
let check_mat_r ~loc ~mat_name ~r ~max_r =
  if r < 1 || r > max_r then raise_mat_bad_r ~loc ~mat_name ~r ~max_r

(** [check_mat_c ~loc ~vec_name ~c ~max_c] checks whether matrix column
    offset [c] for vector of name [vec_name] is invalid (i.e. outside of
    [1..max_c]).  @raise Invalid_argument in that case. *)
let check_mat_c ~loc ~mat_name ~c ~max_c =
  if c < 1 || c > max_c then raise_mat_bad_c ~loc ~mat_name ~c ~max_c

(** [calc_mat_max_rows ~dim1 ~r] @return maximum row operation length [m] for a
    matrix given the dimension [dim1] of the matrix and the start row [r]. *)
let calc_mat_max_rows ~dim1 ~r = dim1 - r + 1

(** [calc_mat_opt_max_rows ?r dim1] @return maximum row operation length
    [m] for a matrix given the dimension [dim1] of the matrix and the optional
    start row [r].  Assumes that the offset has already been validated to
    not exceed [dim1], i.e. the returned [max_m] is at least [1]. *)
let calc_mat_opt_max_rows ?(r = 1) dim1 = calc_mat_max_rows ~dim1 ~r

(** [calc_mat_max_cols ~dim2 ~c] @return maximum column operation length
    [n] for a matrix given the dimension [dim1] of the matrix and the start
    column [c]. *)
let calc_mat_max_cols ~dim2 ~c = dim2 - c + 1

(** [calc_mat_opt_max_cols ?c dim1] @return maximum column operation length
    [m] for a matrix given the dimension [dim2] of the matrix and the optional
    start column [c].  Assumes that the offset has already been validated to
    not exceed [dim2], i.e. the returned [max_n] is at least [1]. *)
let calc_mat_opt_max_cols ?(c = 1) dim2 = calc_mat_max_cols ~dim2 ~c

(** [check_mat_rows ~loc ~mat_name ~dim1 ~r ~p ~param_name] checks the matrix
    row operation length in parameter [p] with name [param_name] at
    location [loc] for matrix with name [mat_name] and dimension [dim1]
    given the operation row [r].  @raise Invalid_argument if any arguments
    are invalid. *)
let check_mat_rows ~loc ~mat_name ~dim1 ~r ~p ~param_name =
  check_var_lt0 ~loc ~name:param_name p;
  if p = 0 then check_mat_r ~loc ~mat_name ~r ~max_r:(dim1 + 1)
  else begin
    check_mat_r ~loc ~mat_name ~r ~max_r:dim1;
    let max_rows = calc_mat_max_rows ~dim1 ~r in
    if p > max_rows then
      raise_max_len ~loc ~len_name:param_name ~len:p ~max_len:max_rows
  end

(** [check_mat_m ~loc ~mat_name ~dim1 ~r ~m] checks the matrix row operation
    length in parameter [m] at location [loc] for matrix with name [mat_name]
    and dimension [dim1] given the operation row [r].  @raise Invalid_argument
    if any arguments are invalid. *)
let check_mat_m ~loc ~mat_name ~dim1 ~r ~m =
  check_mat_rows ~loc ~mat_name ~dim1 ~r ~p:m ~param_name:m_str

(** [check_mat_cols ~loc ~mat_name ~dim2 ~c ~p ~param_name] checks the
    matrix column operation length in parameter [p] with name [param_name]
    at location [loc] for matrix with name [mat_name] and dimension [dim2]
    given the operation column [c].  @raise Invalid_argument if any arguments
    are invalid. *)
let check_mat_cols ~loc ~mat_name ~dim2 ~c ~p ~param_name =
  check_var_lt0 ~loc ~name:param_name p;
  if p = 0 then check_mat_c ~loc ~mat_name ~c ~max_c:(dim2 + 1)
  else begin
    check_mat_c ~loc ~mat_name ~c ~max_c:dim2;
    let max_cols = calc_mat_max_cols ~dim2 ~c in
    if p > max_cols then
      raise_max_len ~loc ~len_name:param_name ~len:p ~max_len:max_cols
  end

(** [check_mat_n ~loc ~mat_name ~dim2 ~c ~n] checks the matrix column
    operation length in parameter [n] at location [loc] for matrix with
    name [mat_name] and dimension [dim2] given the operation column [c].
    @raise Invalid_argument if any arguments are invalid. *)
let check_mat_n ~loc ~mat_name ~dim2 ~c ~n =
  check_mat_cols ~loc ~mat_name ~dim2 ~c ~p:n ~param_name:n_str

(** [check_mat_mn ~loc ~mat_name ~dim1 ~dim2 ~r ~c ~m ~n] checks the matrix
    operation lengths in parameters [m] and [n] at location [loc] for matrix
    with name [mat_name] and dimensions [dim1] and [dim2] given the operation
    row [r] and column [c].  @raise Invalid_argument if any arguments are
    invalid. *)
let check_mat_mn ~loc ~mat_name ~dim1 ~dim2 ~r ~c ~m ~n =
  check_mat_m ~loc ~mat_name ~dim1 ~r ~m;
  check_mat_n ~loc ~mat_name ~dim2 ~c ~n

(** [get_mat_rows ~loc ~mat_name ~dim1 ~r p ~param_name] checks or infers
    the matrix row operation length in the option parameter [p] with
    name [param_name] at location [loc] for matrix with name [mat_name]
    and dimension [dim1] given the row operation offset [r].  @raise
    Invalid_argument if any arguments are invalid. *)
let get_mat_rows ~loc ~mat_name ~dim1 ~r ~p ~param_name =
  match p with
  | None when dim1 = 0 ->
      if r = 1 then dim1 else raise_mat_bad_r ~loc ~mat_name ~r ~max_r:1
  | None ->
      let max_r = dim1 + 1 in
      check_mat_r ~loc ~mat_name ~r ~max_r;
      max_r - r
  | Some p -> check_mat_rows ~loc ~mat_name ~dim1 ~r ~p ~param_name; p

(** [get_mat_dim1 ~loc ~mat_name ~dim1 ~r ~m ~m_name] checks or infers the
    matrix row operation length in the option parameter [m] with name [m_name]
    at location [loc] for matrix with name [mat_name] and dimension [dim1]
    given the row operation offset [r].  @raise Invalid_argument if any
    arguments are invalid. *)
let get_mat_dim1 ~loc ~mat_name ~dim1 ~r ~m ~m_name =
  get_mat_rows ~loc ~mat_name ~dim1 ~r ~p:m ~param_name:m_name

(** [get_mat_m ~loc ~mat_name ~dim1 ~r ~m] checks or infers the matrix row
    operation length in the option parameter [m] at location [loc] for matrix
    with name [mat_name] and dimension [dim1] given the row operation offset
    [r].  @raise Invalid_argument if any arguments are invalid. *)
let get_mat_m ~loc ~mat_name ~dim1 ~r ~m =
  get_mat_dim1 ~loc ~mat_name ~dim1 ~r ~m_name:m_str ~m

(** [get_mat_cols ~loc ~mat_name ~dim2 ~c ~param_name p] checks or infers
    the matrix column operation length in the option parameter [p] with
    name [param_name] at location [loc] for matrix with name [mat_name]
    and dimension [dim2] given the column operation offset [c].  @raise
    Invalid_argument if any arguments are invalid. *)
let get_mat_cols ~loc ~mat_name ~dim2 ~c ~p ~param_name =
  match p with
  | None when dim2 = 0 ->
      if c = 1 then dim2 else raise_mat_bad_c ~loc ~mat_name ~c ~max_c:1
  | None ->
      let max_c = dim2 + 1 in
      check_mat_c ~loc ~mat_name ~c ~max_c;
      max_c - c
  | Some p -> check_mat_cols ~loc ~mat_name ~dim2 ~c ~p ~param_name; p

(** [get_mat_dim2 ~loc ~mat_name ~dim2 ~c ~n ~n_name] checks or infers the
    matrix column operation length in the option parameter [n] with name
    [n_name] at location [loc] for matrix with name [mat_name] and dimension
    [dim2] given the column operation offset [c].  @raise Invalid_argument
    if any arguments are invalid. *)
let get_mat_dim2 ~loc ~mat_name ~dim2 ~c ~n ~n_name =
  get_mat_cols ~loc ~mat_name ~dim2 ~c ~p:n ~param_name:n_name

(** [get_mat_n ~loc ~mat_name ~dim2 ~c ~n] checks or infers the matrix column
    operation length in the option parameter [n] at location [loc] for matrix
    with name [mat_name] and dimension [dim2] given the column operation
    offset [c].  @raise Invalid_argument if any arguments are invalid. *)
let get_mat_n ~loc ~mat_name ~dim2 ~c ~n =
  get_mat_dim2 ~loc ~mat_name ~dim2 ~c ~n ~n_name:n_str

(** [get_mat_min_dim1 ~loc ~mat_name ~r ~m] @return the minimum row dimension
    of a matrix with name [mat_name] at location [loc] given row [r] and
    row operation length [m].  @raise Invalid_argument if any arguments
    are invalid. *)
let get_mat_min_dim1 ~loc ~mat_name ~r ~m =
  if r > 0 then r + m - 1
  else invalid_arg (sprintf "%s: %sr < 1: %d" loc mat_name r)

(** [get_mat_min_dim2 ~loc ~mat_name ~c ~n] @return the minimum column
    dimension of a matrix with name [mat_name] at location [loc] given column
    [c] and row operation length [n].  @raise Invalid_argument if any
    arguments are invalid. *)
let get_mat_min_dim2 ~loc ~mat_name ~c ~n =
  if c > 0 then c + n - 1
  else invalid_arg (sprintf "%s: %sc < 1: %d" loc mat_name c)

(** [check_mat_min_dim1 ~loc ~mat_name ~dim1 ~min_dim1] checks the minimum
    row dimension [min_dim1] of a matrix with name [mat_name] at location
    [loc] given its row dimension [dim1].  @raise Invalid_argument if
    any arguments are invalid. *)
let check_mat_min_dim1 ~loc ~mat_name ~dim1 ~min_dim1 =
  if dim1 < min_dim1 then
    invalid_arg (
      sprintf "%s: dim1(%s): valid=[%d..[ got=%d" loc mat_name min_dim1 dim1)

(** [check_mat_min_dim2 ~loc ~mat_name ~dim2 ~min_dim2] checks the minimum
    column dimension [min_dim2] of a matrix with name [mat_name] at location
    [loc] given its column dimension [dim2].  @raise Invalid_argument if
    any arguments are invalid. *)
let check_mat_min_dim2 ~loc ~mat_name ~dim2 ~min_dim2 =
  if dim2 < min_dim2 then
    invalid_arg (
      sprintf "%s: dim2(%s): valid=[%d..[ got=%d" loc mat_name min_dim2 dim2)

(** [check_mat_min_dim2 ~loc ~mat_name ~dim2 ~min_dim2] checks the minimum
    column dimension [min_dim2] of a matrix with name [mat_name] at location
    [loc] given its column dimension [dim2].  @raise Invalid_argument if
    any arguments are invalid. *)
let check_mat_min_dims ~loc ~mat_name ~dim1 ~dim2 ~min_dim1 ~min_dim2 =
  check_mat_min_dim1 ~loc ~mat_name ~dim1 ~min_dim1;
  check_mat_min_dim2 ~loc ~mat_name ~dim2 ~min_dim2


(** (Old) Vector checking and allocation functions *)

let check_vec loc vec_name vec min_dim =
  check_vec_min_dim ~loc ~vec_name ~dim:(Array1.dim vec) ~min_dim

(** [check_vec_is_perm loc vec_name vec n] checks whether [vec]
    is a valid permutation vector. *)
let check_vec_is_perm loc vec_name vec n =
  let dim = Array1.dim vec in
  if dim <> n then
    invalid_arg (sprintf "%s: dim(%s): valid=%d got=%d" loc vec_name n dim)
  else
    let ub = Int32.of_int n in
    for i = 1 to dim do
      let r = Array1.get vec i in
      check_var_within loc (sprintf "%s(%d)" k_str i) r 1l ub Int32.to_string
  done

let get_vec loc vec_name vec ofs inc n vec_create =
  let min_dim = get_vec_min_dim ~loc ~vec_name ~ofs ~inc ~n in
  match vec with
  | Some vec -> check_vec loc vec_name vec min_dim; vec
  | None -> vec_create min_dim

(** [get_dim_vec loc vec_name ofs inc vec n_name n] if the dimension [n]
   is given, check that the vector [vec] is big enough, otherwise return
   the maximal [n] for the given vector [vec]. *)
let get_dim_vec loc vec_name ofs inc vec n_name n =
  get_vec_n ~loc ~vec_name ~dim:(Array1.dim vec) ~ofs ~inc ~n_name n

let check_vec_empty ~loc ~vec_name ~dim =
  if dim = 0 then
    invalid_arg (sprintf "%s: dimension of vector %s is zero" loc vec_name)
  else ()



(** (Old) Matrix checking and allocation functions *)

let get_mat loc mat_name mat_create r c mat m n =
  let min_dim1 = get_mat_min_dim1 ~loc ~mat_name ~r ~m in
  let min_dim2 = get_mat_min_dim2 ~loc ~mat_name ~c ~n in
  match mat with
  | None -> mat_create min_dim1 min_dim2
  | Some mat ->
      let dim1 = Array2.dim1 mat in
      let dim2 = Array2.dim2 mat in
      check_mat_min_dims ~loc ~mat_name ~dim1 ~dim2 ~min_dim1 ~min_dim2;
      mat

let check_dim1_mat loc mat_name mat mat_r m_name m =
  let dim1 = Array2.dim1 mat in
  check_mat_rows ~loc ~mat_name ~dim1 ~r:mat_r ~p:m ~param_name:m_name

let check_dim2_mat loc mat_name mat mat_c n_name n =
  let dim2 = Array2.dim2 mat in
  check_mat_cols ~loc ~mat_name ~dim2 ~c:mat_c ~p:n ~param_name:n_name

let check_dim_mat loc mat_name mat_r mat_c mat m n =
  check_dim1_mat loc mat_name mat mat_r m_str m;
  check_dim2_mat loc mat_name mat mat_c n_str n

let get_dim1_mat loc mat_name mat r m_name m =
  let dim1 = Array2.dim1 mat in
  get_mat_dim1 ~loc ~mat_name ~dim1 ~r ~m ~m_name

let get_dim2_mat loc mat_name mat c n_name n =
  let dim2 = Array2.dim2 mat in
  get_mat_dim2 ~loc ~mat_name ~dim2 ~c ~n ~n_name

let check_mat_empty ~loc ~mat_name ~dim1 ~dim2 =
  if dim1 = 0 then
    invalid_arg (sprintf "%s: dim1 of matrix %s is zero" loc mat_name)
  else if dim2 = 0 then
    invalid_arg (sprintf "%s: dim2 of matrix %s is zero" loc mat_name)
  else ()


let get_vec_inc loc vec_name = function
  | Some inc -> check_vec_inc ~loc ~vec_name inc; inc
  | None -> 1

let get_vec_ofs loc var = function
  | Some ofs when ofs < 1 -> invalid_arg (sprintf "%s: ofs%s < 1" loc var)
  | Some ofs -> ofs
  | None -> 1

(**)

(* Fetches problem-dependent parameters for LAPACK-functions *)
external ilaenv : int -> string -> string -> int -> int -> int -> int -> int
  = "lacaml_ilaenv_stub_bc" "lacaml_ilaenv_stub" "noalloc"

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

let calc_unpacked_dim loc n_vec =
  let n = truncate (sqrt (float (8 * n_vec + 1)) *. 0.5) in
  if (n * n + n) / 2 <> n_vec then
    failwith (sprintf "%s: illegal vector length: %d" loc n_vec)
  else n

(* Calculate the dimension of a packed square matrix given the vector length *)
let get_unpacked_dim loc ?n n_vec =
  match n with
  | None -> calc_unpacked_dim loc n_vec
  | Some n ->
      let n_unpacked = calc_unpacked_dim loc n_vec in
      if n < 0 || n > n_unpacked then
        invalid_arg (sprintf "%s: n: valid=[0..%d] got=%d" loc n_unpacked n)
      else n

let get_vec_geom loc var ofs inc =
  get_vec_ofs loc var ofs, get_vec_inc loc var inc

(* A symmetric band (SB) or triangular band (TB) matrix has physical size
   [k+1]*[n] for a logical matrix of size [n]*[n].  Check and return the [k]
   (possibly also given by the optional argument [k]). *)
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

(* Makes sure that [mat] is a square matrix and [n] is within range *)
let get_n_of_square loc mat_name r c mat n =
  let n = get_dim2_mat loc mat_name mat c n_str n in
  check_dim1_mat loc mat_name mat r n_str n;
  n

let get_n_of_a loc ar ac a n = get_n_of_square loc a_str ar ac a n

let get_nrhs_of_b loc n br bc b nrhs =
  let nrhs = get_dim2_mat loc b_str b bc nrhs_str nrhs in
  check_dim1_mat loc b_str b br n_str n;
  nrhs


(* ORGQR - Auxiliary Functions *)

let orgqr_err ~loc ~m ~n ~k ~work ~a ~err =
  let msg =
    match err with
    | -1 -> sprintf "m: valid=[0..[ got=%d" m
    | -2 -> sprintf "n: valid=[0..%d] got=%d" m n
    | -3 -> sprintf "k: valid=[0..%d] got=%d" n k
    | -5 -> sprintf "dim2(a): valid=[%d..[ got=%d" n (Array2.dim2 a)
    | -8 ->
        sprintf "dim1(work): valid=[%d..[ got=%d" (max 1 n) (Array1.dim work)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n))
  in
  invalid_arg (sprintf "%s: %s" loc msg)

let orgqr_get_params loc ?m ?n ?k ~tau ~ar ~ac a =
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let k = get_dim_vec loc tau_str 1 1 tau k_str k in
  m, n, k


(* ORMQR - Auxiliary Functions *)

let ormqr_err ~loc ~side ~m ~n ~k ~lwork ~a ~c ~err =
  let nq, nw =
    match side with
    | `L -> m, n
    | `R -> n, m
  in
  let msg =
    match err with
    | -3 -> sprintf "m: valid=[0..[ got=%d" m
    | -4 -> sprintf "n: valid=[0..[ got=%d" n
    | -5 -> sprintf "k: valid=[0..%d] got=%d" k nq
    | -7 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 nq) (Array2.dim1 a)
    | -10 -> sprintf "dim1(c): valid=[%d..[ got=%d" (max 1 m) (Array2.dim1 c)
    | -12 ->
        let min_lwork = max 1 nw in
        sprintf "lwork: valid=[%d..[ got=%d" min_lwork lwork
    | _ -> raise (InternalError (sprintf "%s: error code %d" loc err))
  in
  invalid_arg (sprintf "%s: %s" loc msg)

let ormqr_get_params loc ~side ?m ?n ?k ~tau ~ar ~ac a ~cr ~cc c =
  let m = get_dim1_mat loc c_str c cr m_str m in
  let n = get_dim2_mat loc c_str c cc n_str n in
  let k = get_dim2_mat loc a_str a ac k_str k in
  begin match side with
  | `L ->
      if m < k then failwith (sprintf "%s: m(%d) < k(%d)" loc m k);
      check_dim1_mat loc a_str a ar m_str (max 1 m)
  | `R ->
      if n < k then failwith (sprintf "%s: n(%d) < k(%d)" loc n k);
      check_dim1_mat loc a_str a ar n_str (max 1 n)
  end;
  check_vec loc tau_str tau k;
  m, n, k


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
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let nrhs = get_dim2_mat loc b_str b bc nrhs_str nrhs in
  check_dim1_mat loc b_str b br m_str (max m n);
  m, n, nrhs


(* ??ev -- auxiliary functions *)

let xxev_get_params loc ar ac a n vectors up =
  let n = get_n_of_a loc ar ac a n in
  let jobz = get_job_char vectors in
  let uplo = get_uplo_char up in
  n, jobz, uplo

let xxev_get_wx vec_create loc wname ofsw w n =
  match w with
  | None -> 1, vec_create n
  | Some w -> check_vec loc wname w (ofsw - 1 + n); ofsw, w


(* geev -- auxiliary functions *)

let geev_get_job_side loc mat_empty mat_create mat_name n r c mat_opt =
  match mat_opt with
  | None ->
      if r < 1 then failwith (sprintf "%s: %sr < 1" loc mat_name)
      else if c < 1 then failwith (sprintf "%s: %sc < 1" loc mat_name)
      else r, c, mat_create (n + r - 1) (n + c - 1), job_char_true, true
  | Some None -> 1, 1, mat_empty, job_char_false, false
  | Some (Some mat) ->
      check_dim1_mat loc mat_name mat r n_str n;
      check_dim2_mat loc mat_name mat c n_str n;
      r, c, mat, job_char_true, true

let geev_gen_get_params loc mat_empty mat_create ar ac a n
      leftr leftc left rightr rightc right =
  let n = get_n_of_a loc ar ac a n in
  let leftr, leftc, vl, jobvl, lvs =
    geev_get_job_side loc mat_empty mat_create "vl" n leftr leftc left in
  let rightr, rightc, vr, jobvr, rvs =
    geev_get_job_side loc mat_empty mat_create "vr" n rightr rightc right in
  n, leftr, leftc, vl, jobvl, rightr, rightc, vr, jobvr, lvs || rvs


(* g?mv -- auxiliary functions *)

let gXmv_get_params loc vec_create m n ofsx incx x ofsy incy y trans =
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  let lx, ly, trans_char =
    let trans_char = get_trans_char trans in
    if trans = `N then n, m, trans_char else m, n, trans_char in
  check_vec loc x_str x (ofsx + (lx - 1) * abs incx);
  let y = get_vec loc y_str y ofsy incy ly vec_create in
  ofsx, incx, ofsy, incy, y, trans_char


(* symv -- auxiliary functions *)

let symv_get_params loc vec_create ar ac a n ofsx incx x ofsy incy y up =
  let n = get_dim1_mat loc a_str a ar n_str n in
  check_dim2_mat loc a_str a ac n_str n;
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  let ofsy, incy = get_vec_geom loc y_str ofsy incy in
  check_vec loc x_str x (ofsx + (n - 1) * abs incx);
  let y = get_vec loc y_str y ofsy incy n vec_create in
  check_vec loc y_str y (ofsy + (n - 1) * abs incy);
  n, ofsx, incx, ofsy, incy, y, get_uplo_char up


(* tr?v -- auxiliary functions *)

let trXv_get_params loc ar ac a n ofsx incx x up trans unit_triangular =
  let n = get_dim1_mat loc a_str a ar n_str n in
  check_dim2_mat loc a_str a ac n_str n;
  let trans_char = get_trans_char trans in
  let diag_char = get_diag_char unit_triangular in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  check_vec loc x_str x (ofsx + (n - 1) * abs incx);
  n, ofsx, incx, get_uplo_char up, trans_char, diag_char


(* tp?v -- auxiliary functions *)

let tpXv_get_params loc ofsap ap ?n ofsx incx x up trans unit_triangular =
  let ofsap = get_vec_ofs loc ap_str ofsap in
  let n = get_unpacked_dim loc ?n (Array1.dim ap - ofsap + 1) in
  let trans_char = get_trans_char trans in
  let diag_char = get_diag_char unit_triangular in
  let ofsx, incx = get_vec_geom loc x_str ofsx incx in
  check_vec loc x_str x (ofsx + (n - 1) * abs incx);
  n, ofsap, ofsx, incx, get_uplo_char up, trans_char, diag_char


(* gemm -- auxiliary functions *)

let get_c loc mat_create cr cc c m n = get_mat loc c_str mat_create cr cc c m n

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

let gemm_get_params loc mat_create ar ac a transa br bc b cr transb cc c m n k =
  let m = get_rows_mat_tr loc a_str a ar ac transa m_str m in
  let n = get_cols_mat_tr loc b_str b br bc transb n_str n in
  let k = get_inner_dim loc a_str a ar ac transa b_str b br bc transb k_str k in
  let transa = get_trans_char transa in
  let transb = get_trans_char transb in
  let c = get_c loc mat_create cr cc c m n in
  m, n, k, transa, transb, c


(* symm -- auxiliary functions *)

let check_mat_square loc mat_str mat mat_r mat_c n =
  check_dim1_mat loc mat_str mat mat_r n_str n;
  check_dim2_mat loc mat_str mat mat_c n_str n

let symm_get_params loc mat_create ar ac a br bc b cr cc c m n side up =
  let m = get_dim1_mat loc b_str b br m_str m in
  let n = get_dim2_mat loc b_str b bc n_str n in
  if side = `L then check_mat_square loc a_str a ar ac m
  else check_mat_square loc a_str a ar ac n;
  let side_char = get_side_char side in
  let uplo_char = get_uplo_char up in
  let c = get_c loc mat_create cr cc c m n in
  m, n, side_char, uplo_char, c


(* trmm -- auxiliary functions *)

let trXm_get_params loc ar ac a br bc b m n side up transa diag =
  let m = get_dim1_mat loc b_str b br m_str m in
  let n = get_dim2_mat loc b_str b bc n_str n in
  if side = `L then check_mat_square loc a_str a ar ac m
  else check_mat_square loc a_str a ar ac n;
  let side_char = get_side_char side in
  let uplo_char = get_uplo_char up in
  let transa = get_trans_char transa in
  let diag_char = get_diag_char diag in
  m, n, side_char, uplo_char, transa, diag_char


(* syrk -- auxiliary functions *)

let syrk_get_params loc mat_create ar ac a cr cc c n k up trans =
  let n = get_rows_mat_tr loc a_str a ar ac trans n_str n in
  let k = get_cols_mat_tr loc a_str a ar ac trans k_str k in
  let trans_char = get_trans_char trans in
  let uplo_char = get_uplo_char up in
  let c = get_c loc mat_create cr cc c n n in
  n, k, uplo_char, trans_char, c


(* syr2k -- auxiliary functions *)

let syr2k_get_params loc mat_create ar ac a br bc b cr cc c n k up trans =
  let n = get_rows_mat_tr loc a_str a ar ac trans n_str n in
  let k = get_cols_mat_tr loc a_str a ar ac trans k_str k in
  begin match trans with
  | `N ->
      check_dim1_mat loc b_str b br n_str n;
      check_dim2_mat loc b_str b bc k_str k;
  | _ ->
      check_dim1_mat loc b_str b br k_str k;
      check_dim2_mat loc b_str b bc n_str n;
  end;
  let trans_char = get_trans_char trans in
  let uplo_char = get_uplo_char up in
  let c = get_c loc mat_create cr cc c n n in
  n, k, uplo_char, trans_char, c


(* ?lange -- auxiliary functions *)

let xlange_get_params loc m n ar ac a =
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  m, n


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

let xxtri_singular_err loc err =
  failwith (sprintf "%s: singular on index %i" loc err)

let xxtri_err loc n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)


(* ??con -- auxiliary functions *)

let xxcon_err loc n a err =
  let msg =
    match err with
    | -2 -> sprintf "n: valid=[0..[ got=%d" n
    | -4 -> sprintf "dim1(a): valid=%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)


(* geXrf -- auxiliary functions *)

let geXrf_get_params loc m n ar ac a =
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  m, n


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
  | None -> create_int32_vec (min m n)
  | Some ipiv ->
      check_vec loc ipiv_str ipiv (min m n);
      ipiv


(* sytrf -- auxiliary functions *)

let sytrf_get_ipiv loc ipiv n =
  match ipiv with
  | None -> create_int32_vec n
  | Some ipiv ->
      check_vec loc ipiv_str ipiv n;
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

let potrf_err loc n a err =
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


(* trtrs -- auxiliary functions *)

let trtrs_err loc n nrhs a b err =
  let msg =
    match err with
    | -4 -> sprintf "n: valid=[0..[ got=%d" n
    | -5 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
    | -7 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | -9 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)


(* tbtrs -- auxiliary functions *)

let tbtrs_err loc n nrhs kd ab b err =
  let msg =
    match err with
    | -4 -> sprintf "n: valid=[0..[ got=%d" n
    | -5 -> sprintf "kd: valid=[0..[ got=%d" kd
    | -6 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
    | -8 -> sprintf "dim1(ab): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 ab)
    | -10 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
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


(* trtri -- auxiliary functions *)

let trtri_err loc n a err =
  let msg =
    match err with
    | -3 -> sprintf "n: valid=[0..[ got=%d" n
    | -5 -> sprintf "dim1(a): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 a)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)


(* geqrf -- auxiliary functions *)

let geqrf_err loc m n a err =
  let msg =
    match err with
    | -1 -> sprintf "m: valid=[0..[ got=%d" m
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


(* gees -- auxiliary functions *)

let gees_err loc n err jobvs sort =
  if err > 0 && err <= n then
    failwith (sprintf "%s: %d eigenvalue elements did not converge" loc err)
  else if err = n + 1 then
    failwith (
      sprintf "%s: eigenvalues not reordered, too close to separate" loc)
  else if err = n + 2 then
    failwith (
      sprintf "%s: after reordering, roundoff changed values of some \
                   complex eigenvalues so that leading eigenvalues in \
                   the Schur form no longer satisfy SELECT" loc)
  else
    let msg =
      match err with
      | -1 -> sprintf "JOBVS: valid=['N', V'] got='%c'" jobvs
      | -2 -> sprintf "SORT: valid=['N', S'] got='%c'" sort
      | -4 -> sprintf "n: valid=[0..[ got=%d" n
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n))
    in
    invalid_arg (sprintf "%s: %s" loc msg)

let dummy_select_fun _ = false

let gees_get_params_generic
      loc mat_create mat_empty jobvs sort n ar ac a vsr vsc vs =
  let n = get_n_of_a loc ar ac a n in
  let jobvs, min_ldvs =
    match jobvs with
    | `No_Schur_vectors -> 'N', 1
    | `Compute_Schur_vectors -> 'V', n
  in
  let vs =
    match vs with
    | Some vs ->
        check_dim1_mat loc vs_str vs vsr vsr_str min_ldvs;
        check_dim2_mat loc vs_str vs vsc vsc_str n;
        vs
    | None when jobvs = 'N' -> mat_empty
    | None -> mat_create min_ldvs n
  in
  let sort, select, select_fun =
    match sort with
    | `No_sort -> 'N', 0, dummy_select_fun
    | `Select_left_plane -> 'S', 0, dummy_select_fun
    | `Select_right_plane -> 'S', 1, dummy_select_fun
    | `Select_interior_disk -> 'S', 2, dummy_select_fun
    | `Select_exterior_disk -> 'S', 3, dummy_select_fun
    | `Select_custom select_fun -> 'S', 4, select_fun
  in
  jobvs, sort, select, select_fun, n, vs

let gees_get_params_real
      loc vec_create mat_create mat_empty
      jobvs sort n ar ac a wr wi vsr vsc vs =
  let jobvs, sort, select, select_fun, n, vs =
    gees_get_params_generic
      loc mat_create mat_empty jobvs sort n ar ac a vsr vsc vs
  in
  let wr =
    match wr with
    | None -> vec_create n
    | Some wr -> check_vec loc wr_str wr n; wr
  in
  let wi =
    match wi with
    | None -> vec_create n
    | Some wi -> check_vec loc wi_str wi n; wi
  in
  jobvs, sort, select, select_fun, n, vs, wr, wi

let gees_get_params_complex
      loc vec_create mat_create mat_empty jobvs sort n ar ac a w vsr vsc vs =
  let jobvs, sort, select, select_fun, n, vs =
    gees_get_params_generic
      loc mat_create mat_empty jobvs sort n ar ac a vsr vsc vs
  in
  let w =
    match w with
    | None -> vec_create n
    | Some w -> check_vec loc w_str w n; w
  in
  jobvs, sort, select, select_fun, n, vs, w


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
            (match jobu with 'A' | 'S' -> max 1 m | _ -> 1)
            (Array2.dim1 u)
      | -11 ->
          sprintf "dim1(vt): valid=[%d..[ got=%d"
            (
              match jobvt with
              | 'A' -> max 1 n
              | 'S' -> max 1 (min m n)
              | _ -> 1
            )
            (Array2.dim1 vt)
      | -13 -> sprintf "lwork: valid=[%d..[ got=%d" 1 lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let gesvd_get_params
    loc vec_create mat_create jobu jobvt m n ar ac a s ur uc u vtr vtc vt =
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let s = get_vec loc s_str s 1 1 (min m n) vec_create in
  let um, un =
    match jobu with
    | `A -> m, m
    | `S -> m, min m n
    | `O | `N -> 1, 1 in  (* LDU >= 1 even when U not referenced *)
  let u =
    match u with
    | Some u ->
        check_dim1_mat loc u_str u ur um_str um;
        check_dim2_mat loc u_str u uc un_str un;
        u
    | None -> mat_create um un in
  let vm, vn =
    match jobvt with
    | `A -> n, n
    | `S -> min m n, n
    | `O | `N ->  1, 1 in  (* LDVT >= 1 even when VT not referenced *)
  let vt =
    match vt with
    | Some vt ->
        check_dim1_mat loc vt_str vt vtr vm_str vm;
        check_dim2_mat loc vt_str vt vtc vn_str vn;
        vt
    | None -> mat_create vm vn in
  let jobu_c = get_s_d_job_char jobu in
  let jobvt_c = get_s_d_job_char jobvt in
  jobu_c, jobvt_c, m, n, s, u, vt


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
            (
              if jobz = 'A' || jobz = 'S' || (jobz = 'O' && m < n)
              then max 1 m
              else 1
            )
            (Array2.dim1 u)
      | -10 ->
          sprintf "dim1(vt): valid=[%d..[ got=%d"
            (
              if jobz = 'A' || (jobz = 'O' && m >= n) then max 1 n
              else if jobz = 'S' then max 1 (min m n)
              else 1
            )
            (Array2.dim1 vt)
      | -12 -> sprintf "lwork: valid=[%d..[ got=%d" 1 lwork
      | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
    invalid_arg (sprintf "%s: %s" loc msg)

let gesdd_get_params
      loc vec_create mat_create jobz m n ar ac a s ur uc u vtr vtc vt =
  let m = get_dim1_mat loc a_str a ar m_str m in
  let n = get_dim2_mat loc a_str a ac n_str n in
  let min_m_n = min m n in
  let s = get_vec loc s_str s 1 1 min_m_n vec_create in
  let um, un, vm, vn =
    match jobz with
    | `A -> m, m, n, n
    | `S -> m, min_m_n, min_m_n, n
    | `O -> if m >= n then 1, 1, n, n else m, m, m, n
    | `N  -> 1, 1, 1, 1 in  (* LDU >= 1 even when U not referenced *)
  let u =
    match u with
    | Some u ->
        check_dim1_mat loc u_str u ur um_str um;
        check_dim2_mat loc u_str u uc un_str un;
        u
    | None -> mat_create um un in
  let vt =
    match vt with
    | Some vt ->
        check_dim1_mat loc vt_str vt vtr vm_str vm;
        check_dim2_mat loc vt_str vt vtc vn_str vn;
        vt
    | None -> mat_create vm vn in
  let jobz_c = get_s_d_job_char jobz in
  jobz_c, m, n, s, u, vt


(* ??sv -- auxiliary functions *)

let xxsv_err loc n nrhs b err =
  let msg =
    match err with
    | -1 -> sprintf "n: valid=[0..[ got=%d" n
    | -2 -> sprintf "nrhs: valid=[0..[ got=%d" nrhs
    | -7 -> sprintf "dim1(b): valid=[%d..[ got=%d" (max 1 n) (Array2.dim1 b)
    | n -> raise (InternalError (sprintf "%s: error code %d" loc n)) in
  invalid_arg (sprintf "%s: %s" loc msg)

let xxsv_lu_err loc err =
  failwith (sprintf "%s: U(%i,%i)=0 in the LU factorization" loc err err)

let xxsv_pos_err loc err =
  let msg =
    sprintf
      "%s: the leading minor of order %i is not positive definite" loc err in
  failwith msg

let xxsv_ind_err loc err =
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
  | None -> create_int32_vec n
  | Some ipiv ->
      check_vec loc ipiv_str ipiv n;
      ipiv

let xxsv_get_params loc ar ac a n br bc b nrhs =
  let n = get_n_of_a loc ar ac a n in
  let nrhs = get_nrhs_of_b loc n br bc b nrhs in
  n, nrhs
