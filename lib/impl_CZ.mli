(* File: impl_CZ.mli

   Copyright (C) 2005-

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

open Bigarray
open Common
open Complexxx

(** {6 BLAS-1 interface} *)

(*
val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
(** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)
*)


(** {6 LAPACK interface} *)

(* LANSY *)

val lansy_min_lwork : int -> norm4 -> int
(** [lansy_min_lwork m norm]
    @return the minimum length of the work array used by the [lansy]-function.
    @param norm type of norm that will be computed by [lansy]
    @param n the number of columns (and rows) in the matrix *)

val lansy :
  ?n : int ->
  ?up : bool ->
  ?norm : norm4 ->
  ?work : rvec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> float
(** [lansy ?n ?up ?norm ?work ?ar ?ac a] see LAPACK documentation!
    @param norm default = `O
    @param up default = true (reference upper triangular part of [a])
    @param n default = number of columns of matrix [a]
    @param work default = allocated work space for norm `I *)

(* GECON *)

val gecon_min_lwork : int -> int
(** [gecon_min_lwork n] @return the minimum length of the work array
    used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to
             the [gecon]-function *)

val gecon_min_lrwork : int -> int
(** [gecon_min_lrwork n] @return the minimum length of the rwork array
    used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to [gecon]-function *)

val gecon :
  ?n : int ->
  ?norm : norm2 ->
  ?anorm : float ->
  ?work : vec ->
  ?rwork : rvec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> float
(** [gecon ?n ?norm ?anorm ?work ?rwork ?ar ?ac a]
    @return estimate of the reciprocal of the condition number of matrix [a]
    @param n default = available number of columns of matrix [a]
    @param norm default = 1-norm
    @param anorm default = norm of the matrix [a] as returned by [lange]
    @param work default = automatically allocated workspace
    @param rwork default = automatically allocated workspace
    @param ar default = 1
    @param ac default = 1 *)

(* SYCON *)

val sycon_min_lwork : int -> int
(** [sycon_min_lwork n] @return the minimum length of the work array
    used by the [sycon]-function.
    @param n the logical dimensions of the matrix given to
             the [sycon]-function *)

val sycon :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?anorm : float ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
(** [sycon ?n ?up ?ipiv ?anorm ?work ?ar ?ac a]
    @return estimate of the reciprocal of the
            condition number of symmetric matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of the factorization of [a] is stored
    @param ipiv default = vec of length [n]
    @param anorm default = 1-norm of the matrix [a] as returned by [lange]
    @param work default = automatically allocated workspace *)

(* POCON *)

val pocon_min_lwork : int -> int
(** [pocon_min_lwork n] @return the minimum length of the work array
    used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to
             the [pocon]-function *)

val pocon_min_lrwork : int -> int
(** [pocon_min_lrwork n] @return the minimum length of the rwork array
    used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to [pocon]-function *)

val pocon :
    ?n : int ->
    ?up : bool ->
    ?anorm : float ->
    ?work : vec ->
    ?rwork : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
(** [pocon ?n ?up ?anorm ?work ?rwork ?ar ?ac a]
    @return estimate of the reciprocal of the condition number of
            complex Hermitian positive definite matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of Cholesky factorization
                        of [a] is stored
    @param work default = automatically allocated workspace
    @param rwork default = automatically allocated workspace
    @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)


(** General SVD routines *)

val gesvd_min_lwork : m : int -> n : int -> int
(** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
    used by the [gesvd]-function for matrices with [m] rows and [n]
    columns. *)

val gesvd_lrwork : m : int -> n : int -> int
(** [gesvd_lrwork m n] @return the (minimum) length of the rwork array
    used by the [gesvd]-function. *)

val gesvd_opt_lwork :
  ?m : int -> ?n : int ->
  ?jobu : svd_job ->
  ?jobvt : svd_job ->
  ?s : rvec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?ar : int -> ?ac : int -> mat
  -> int

val gesvd :
  ?m : int -> ?n : int ->
  ?jobu : svd_job ->
  ?jobvt : svd_job ->
  ?s : rvec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?work : vec ->
  ?rwork : rvec ->
  ?ar : int -> ?ac : int -> mat
  -> rvec * mat * mat


(** General eigenvalue problem (simple drivers) *)

val geev_min_lwork : int -> int
(** [geev_min_lwork n] @return the minimum length of the work array
    used by the [geev]-function.
    @param n the logical dimensions of the matrix given to [geev]-function *)

val geev_min_lrwork : int -> int
(** [geev_min_lrwork n] @return the minimum length of the rwork array
    used by the [geev]-function.
    @param n the logical dimensions of the matrix given to [geev]-function *)

val geev_opt_lwork :
  ?n : int ->
  ?leftr : int -> ?leftc  : int -> ?left : mat option ->
  ?rightr : int -> ?rightc : int -> ?right : mat option ->
  ?ofsw : int -> ?w : vec ->
  ?ar : int -> ?ac : int -> mat ->
  int
 (** [geev ?work ?rwork ?n ?leftr ?leftc ?left
       ?rightr ?rightc ?right ?ofsw w ?ar ?ac a]
    See [geev]-function for details about arguments.
    @return "optimal" work size *)

val geev :
  ?n : int ->
  ?work : vec ->
  ?rwork : vec ->
  ?leftr : int -> ?leftc  : int -> ?left : mat option ->
  ?rightr : int -> ?rightc : int -> ?right : mat option ->
  ?ofsw : int -> ?w : vec ->
  ?ar : int -> ?ac : int -> mat ->
  mat * vec * mat
(** [geev ?work ?rwork ?n
      ?leftr ?leftc ?left
      ?rightr ?rightc ?right
      ?ofsw w
      ?ar ?ac a]
    @return [(lv, w, rv)], where [lv] and [rv] correspond to the left and
      right eigenvectors respectively, [w] to the eigenvalues. [lv] ([rv])
      is the empty matrix if [left] ([right]) is set to [None].
    @raise Failure if the function fails to converge
    @param n default = available number of columns of matrix [a]
    @param work default = automatically allocated workspace
    @param rwork default = automatically allocated workspace
    @param left default = Automatically allocated left eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some lv] if you want to provide the storage.
                          You can set [leftr], [leftc] in the last case.
    (See LAPACK GEEV docs for details about storage of complex eigenvectors)
    @param right default = Automatically allocated right eigenvectors.
                           Pass [None] if you do not want to compute them,
                           [Some rv] if you want to provide the storage.
                           You can set [rightr], [rightc] in the last case.
    @param w default = automatically allocate eigenvalues
    @param a the matrix whose eigensystem is computed *)
