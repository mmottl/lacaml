(* File: impl_SD.mli

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
open Floatxx

(** {6 BLAS-1 interface} *)

val dot :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  x : vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> float
(** [dot ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
(** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val asum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
(** [asum ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 BLAS-2 interface} *)

val sbmv :
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?n : int ->
  ?k : int ->
  ?up : bool ->
  ?alpha : float ->
  ?beta : float ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
(** [sbmv ?ofsy ?incy ?y ?ar ?ac a ?n ?k ?up ?alpha ?beta ?ofsx ?incx x] see
    BLAS documentation!
    @return vector [y], which is overwritten.
    @param ofsy default = 1
    @param incy default = 1
    @param ar default = 1
    @param ac default = 1
    @param y default = uninitialized vector of minimal length (see BLAS)
    @param n default = number of available columns to the right of [ac].
    @param k default = number of available rows in matrix [a] - 1
    @param up default = true i.e., upper band of [a] is supplied
    @param alpha default = 1.0
    @param beta default = 0.0
    @param ofsx default = 1
    @param incx default = 1 *)

val syr :
  ?alpha : float ->
  ?up : bool ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> mat
(** [syr ?alpha ?up ?ofsx ?incx x ?n ?ar ?ac a] see BLAS documentation!
    @return vector [a], which is overwritten
    @param alpha default = 1.0
    @param up default = true i.e., upper triangle of [a] is supplied
    @param ofsx default = 1
    @param incx default = 1
    @param ar default = 1
    @param ac default = 1
    @param n default = number of rows of [a] *)

(** {6 LAPACK interface} *)

(** Auxiliary routines *)

val lansy_min_lwork : int -> norm4 -> int
(** [lansy_min_lwork m norm]
    @return the minimum length of the work array used by the [lansy]-function.
    @param norm type of norm that will be computed by [lansy]
    @param n the number of columns (and rows) in the matrix *)

val lansy :
  ?n : int ->
  ?up : bool ->
  ?norm : norm4 ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> float
(** [lansy ?norm ?up ?n ?ar ?ac ?work a] see LAPACK documentation!
    @param norm default = `O
    @param up default = true (reference upper triangular part of [a])
    @param n default = number of columns of matrix [a]
    @param work default = allocated work space for norm `I *)

val lamch :  [ `E | `S | `B | `P | `N | `R | `M | `U | `L | `O ] -> float
(** [lamch cmach] see LAPACK documentation! *)


(** Linear equations (computational routines) *)

(* GECON *)

val gecon_min_lwork : int -> int
(** [gecon_min_lwork n] @return the minimum length of the work array
    used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to
             the [gecon]-function *)

val gecon_min_liwork : int -> int
(** [gecon_min_liwork n] @return the minimum length of the iwork array
    used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to [gecon]-function *)

val gecon :
  ?n : int ->
  ?norm : norm2 ->
  ?anorm : float ->
  ?work : vec ->
  ?iwork : int_vec ->
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
    @param iwork default = automatically allocated workspace
    @param ar default = 1
    @param ac default = 1 *)

(* SYCON *)

val sycon_min_lwork : int -> int
(** [sycon_min_lwork n] @return the minimum length of the work array
    used by the [sycon]-function.
    @param n the logical dimensions of the matrix given to
             the [sycon]-function *)

val sycon_min_liwork : int -> int
(** [sycon_min_liwork n] @return the minimum length of the iwork array
    used by the [sycon]-function.
    @param n the logical dimensions of the matrix given to [sycon]-function *)

val sycon :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?anorm : float ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
(** [sycon ?n ?up ?ipiv ?anorm ?work ?iwork ?ar ?ac a]
    @return estimate of the reciprocal of the condition number
            of symmetric matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of the factorization of [a] is stored
    @param ipiv default = vec of length [n]
    @param anorm default = 1-norm of the matrix [a] as returned by [lange]
    @param work default = automatically allocated workspace
    @param iwork default = automatically allocated workspace *)

(* POCON *)

val pocon_min_lwork : int -> int
(** [pocon_min_lwork n] @return the minimum length of the work array
    used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to
             the [pocon]-function *)

val pocon_min_liwork : int -> int
(** [pocon_min_liwork n] @return the minimum length of the iwork array
    used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to [pocon]-function *)

val pocon :
    ?n : int ->
    ?up : bool ->
    ?anorm : float ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
(** [pocon ?n ?up ?anorm ?work ?iwork ?ar ?ac a]
    @return estimate of the reciprocal of the condition number of
            symmetric positive definite matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of Cholesky factorization
                        of [a] is stored
    @param work default = automatically allocated workspace
    @param iwork default = automatically allocated workspace
    @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)

(** Least squares (expert drivers) *)

val gelsy_min_lwork : m : int -> n : int -> nrhs : int -> int
(** [gelsy_min_lwork ~m ~n ~nrhs] @return the minimum length of the
    work-array used by the [gelsy]-function if the logical dimensions
    of the matrix are [m] rows and [n] columns and if there are [nrhs]
    right hand side vectors. *)

val gelsy_opt_lwork :
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gelsy_opt_lwork ?m ?n ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
    length of the work-array used by the [gelsy]-function given matrix
    [a], optionally its logical dimensions [m] and [n] and given right
    hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param nrhs default = available number of columns in matrix [b] *)

val gelsy :
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?rcond : float ->
  ?jpvt : int_vec ->
  ?work : vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gelsy ?m ?n ?ar ?ac a ?rcond ?jpvt ?ofswork ?work ?nrhs b] see LAPACK
    documentation!  @return the effective rank of [a].
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param rcond default = (-1) => machine precision
    @param jpvt default = vec of length [n]
    @param work default = vec of optimum length (-> [gelsy_opt_lwork])
    @param nrhs default = available number of columns in matrix [b] *)

val gelsd_min_lwork : m : int -> n : int -> nrhs : int -> int
(** [gelsd_min_lwork ~m ~n ~nrhs] @return the minimum length of the
    work-array used by the [gelsd]-function if the logical dimensions
    of the matrix are [m] and [n] and if there are [nrhs] right hand
    side vectors. *)

val gelsd_opt_lwork :
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gelsd_opt_lwork ?m ?n ?ar ?ac a ?nrhs b] @return the optimum length of
    the work-array used by the [gelsd]-function given matrix [a],
    optionally its logical dimensions [m] and [n] and given right hand
    side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param nrhs default = available number of columns in matrix [b] *)

val gelsd_min_iwork : int -> int -> int
(** [gelsd_min_iwork m n] @return the minimum (= optimum) length
    of the iwork-array used by the [gelsd]-function if the logical
    dimensions of the matrix are [m] and [n]. *)

val gelsd :
  ?m : int ->
  ?n : int ->
  ?rcond : float ->
  ?ofss : int ->
  ?s : vec ->
  ?work : vec ->
  ?iwork : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gelsd ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs b]
    see LAPACK documentation!
    @return the effective rank of [a].
    @raise Failure if the function fails to converge.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param rcond default = (-1) => machine precision
    @param ofss default = 1 or ignored if [s] is not given
    @param s default = vec of length [min rows cols]
    @param work default = vec of optimum length (-> [gelsd_opt_lwork])
    @param iwork default = vec of optimum (= minimum) length
    @param nrhs default = available number of columns in matrix [b] *)

val gelss_min_lwork : m : int -> n : int -> nrhs : int -> int
(** [gelss_min_lwork ~m ~n ~nrhs] @return the minimum length of the
    work-array used by the [gelss]-function if the logical dimensions
    of the matrix are [m] rows and [n] columns and if there are [nrhs]
    right hand side vectors. *)

val gelss_opt_lwork :
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?m : int ->
  ?n : int ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gelss_opt_lwork ?ar ?ac a ?m ?n ?nrhs ?br ?bc b] @return the optimum
    length of the work-array used by the [gelss]-function given matrix
    [a], optionally its logical dimensions [m] and [n] and given right
    hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param nrhs default = available number of columns in matrix [b] *)

val gelss :
  ?m : int ->
  ?n : int ->
  ?rcond : float ->
  ?ofss : int ->
  ?s : vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gelss ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs ?br ?bc b]
    see LAPACK documentation!
    @return the effective rank of [a].
    @raise Failure if the function fails to converge.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param rcond default = (-1) => machine precision
    @param ofss default = 1 or ignored if [s] is not given
    @param s default = vec of length [min m n]
    @param work default = vec of optimum length (-> [gelss_opt_lwork])
    @param nrhs default = available number of columns in matrix [b] *)


(** General SVD routines *)

val gesvd_min_lwork : m : int -> n : int -> int
(** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
    used by the [gesvd]-function for matrices with [m] rows and [n]
    columns. *)

val gesvd_opt_lwork :
  ?m : int -> ?n : int ->
  ?jobu : svd_job ->
  ?jobvt : svd_job ->
  ?s : vec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?ar : int -> ?ac : int -> mat
  -> int

val gesvd :
  ?m : int -> ?n : int ->
  ?jobu : svd_job ->
  ?jobvt : svd_job ->
  ?s : vec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?work : vec ->
  ?ar : int -> ?ac : int -> mat
  -> vec * mat * mat

val gesdd_liwork : m : int -> n : int -> int

val gesdd_min_lwork : ?jobz : svd_job -> m : int -> n : int -> unit -> int
(** [gesdd_min_lwork ?jobz ~m ~n] @return the minimum length of the
    work array used by the [gesdd]-function for matrices with [m] rows
    and [n] columns for SVD-job [jobz]. *)

val gesdd_opt_lwork :
  ?m : int -> ?n : int ->
  ?jobz : svd_job ->
  ?s : vec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?iwork : int_vec ->
  ?ar : int -> ?ac : int -> mat
  -> int

val gesdd :
  ?m : int -> ?n : int ->
  ?jobz : svd_job ->
  ?s : vec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?work : vec ->
  ?iwork : int_vec ->
  ?ar : int -> ?ac : int -> mat
  -> vec * mat * mat


(** General eigenvalue problem (simple drivers) *)

val geev_min_lwork : ?vectors : bool -> int -> int
(** [geev_min_lwork vectors n] @return the minimum length of the
    work array used by the [geev]-function. [vectors] indicates whether
    eigenvectors are supposed to be computed.
    @param n the logical dimensions of the matrix given to [geev]-function
    @param vectors default = true *)

val geev_opt_lwork :
  ?n : int ->
  ?vlr : int -> ?vlc : int -> ?vl : mat option ->
  ?vrr : int -> ?vrc : int -> ?vr : mat option ->
  ?ofswr : int -> ?wr : vec ->
  ?ofswi : int -> ?wi : vec ->
  ?ar : int -> ?ac : int -> mat ->
  int
 (** [geev_opt_lwork
       ?n
       ?vlr ?vlc ?vl
       ?vrr ?vrc ?vr
       ?ofswr wr
       ?ofswi wi
       ?ar ?ac a]
    See [geev]-function for details about arguments.
    @return "optimal" size of work array. *)

val geev :
  ?n : int ->
  ?work : vec ->
  ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
  ?vrr : int -> ?vrc : int -> ?vr : mat option ->
  ?ofswr : int -> ?wr : vec ->
  ?ofswi : int -> ?wi : vec ->
  ?ar : int -> ?ac : int -> mat ->
  mat * vec * vec * mat
(** [geev ?work ?n
      ?vlr ?vlc ?vl
      ?vrr ?vrc ?vr
      ?ofswr wr ?ofswi wi
      ?ar ?ac a]
    @return ([lv], [wr], [wi], [rv]), where [wr] and [wv] are the real
      and imaginary components of the eigenvalues, and [lv] and [rv]
      are the left and right eigenvectors. [lv] ([rv]) is the empty
      matrix if [vl] ([vr]) is set to [None].
    @raise Failure if the function fails to converge
    @param n default = available number of columns of matrix [a]
    @param work default = automatically allocated workspace
    @param vl default = Automatically allocated left eigenvectors.
                        Pass [None] if you do not want to compute them,
                        [Some lv] if you want to provide the storage.
                        You can set [vlr], [vlc] in the last case.
    (See LAPACK GEEV docs for details about storage of complex eigenvectors)
    @param vr default = Automatically allocated right eigenvectors.
                        Pass [None] if you do not want to compute them,
                        [Some rv] if you want to provide the storage.
                        You can set [vrr], [vrc] in the last case.
    @param wr default = vector of size [n]; real components of the eigenvalues
    @param wi default = vector of size [n];
                        imaginary components of the eigenvalues
    @param a the matrix whose eigensystem is computed *)


(** Symmetric-matrix eigenvalue and singular value problems (simple drivers) *)

val syev_min_lwork : int -> int
(** [syev_min_lwork n] @return the minimum length of the work-array
    used by the [syev]-function if the logical dimensions of the matrix
    are [n]. *)

val syev_opt_lwork :
  ?n : int ->
  ?vectors : bool ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [syev_opt_lwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
    length of the work-array used by the [syev]-function given matrix
    [a], optionally its logical dimension [n] and whether the eigenvectors
    must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syev :
  ?n : int ->
  ?vectors : bool ->
  ?up : bool ->
  ?work : vec ->
  ?ofsw : int ->
  ?w : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> vec
(** [syev ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a]
    @return the vector [w] of eigenvalues in ascending order.
    @raise Failure if the function fails to converge.
    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param up default = true i.e., upper triangle of [a] is stored
    @param work default = vec of optimum length (-> [syev_opt_lwork])
    @param ofsw default = 1 ot ignored if [w] is not given
    @param w default = vec of length [n] *)

val syevd_min_lwork : vectors : bool -> int -> int
(** [syevd_min_lwork vectors n] @return the minimum length of the
    work-array used by the [syevd]-function if the logical dimensions of
    the matrix are [n] and given whether eigenvectors should be computed
    ([vectors]). *)

val syevd_min_liwork : vectors : bool -> int -> int
(** [syevd_min_liwork vectors n] @return the minimum length of the
    iwork-array used by the [syevd]-function if the logical dimensions of
    the matrix are [n] and given whether eigenvectors should be computed
    ([vectors]). *)

val syevd_opt_lwork :
  ?n : int ->
  ?vectors : bool ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [syevd_opt_lwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
    length of the work-array used by the [syevd]-function given matrix
    [a], optionally its logical dimension [n] and whether the eigenvectors
    must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevd_opt_liwork :
  ?n : int ->
  ?vectors : bool ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [syevd_opt_liwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
    length of the iwork-array used by the [syevd]-function given matrix
    [a], optionally its logical dimension [n] and whether the eigenvectors
    must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevd_opt_l_li_work :
  ?n : int ->
  ?vectors : bool ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int * int
(** [syevd_opt_l_li_iwork ?n ?vectors ?up ?ar ?ac a] @return the tuple
    of optimum lengths of the work- and iwork-arrays respectively,
    used by the [syevd]-function given matrix [a], optionally its
    logical dimension [n] and whether the eigenvectors must be computed
    ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevd :
  ?n : int ->
  ?vectors : bool ->
  ?up : bool ->
  ?work : vec ->
  ?iwork : int_vec ->
  ?ofsw : int ->
  ?w : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> vec
(** [syevd ?n ?vectors ?up ?ofswork ?work ?iwork ?ofsw ?w ?ar ?ac a]
    @return the vector [w] of eigenvalues in ascending order.
    @raise Failure if the function fails to converge.
    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param up default = true i.e., upper triangle of [a] is stored
    @param work default = vec of optimum length (-> [syev_opt_lwork])
    @param iwork default = int_vec of optimum length (-> [syevd_opt_liwork])
    @param ofsw default = 1 ot ignored if [w] is not given
    @param w default = vec of length [n] *)


(** Symmetric-matrix eigenvalue and singular value problems (expert &
    RRR drivers) *)

val syevr_min_lwork : int -> int
(** [syevr_min_lwork n] @return the minimum length of the
    work-array used by the [syevr]-function if the logical dimensions
    of the matrix are [n]. *)

val syevr_min_liwork : int -> int
(** [syevr_min_liwork n] @return the minimum length of the
    iwork-array used by the [syevr]-function if the logical dimensions
    of the matrix are [n]. *)

val syevr_opt_lwork :
  ?n : int ->
  ?vectors : bool ->
  ?range : [ `A | `V of float * float | `I of int * int ] ->
  ?up : bool ->
  ?abstol : float ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [syevr_opt_lwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a] @return
    the optimum length of the work-array used by the [syevr]-function
    given matrix [a], optionally its logical dimension [n] and whether
    the eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevr_opt_liwork :
  ?n : int ->
  ?vectors : bool ->
  ?range : [ `A | `V of float * float | `I of int * int ] ->
  ?up : bool ->
  ?abstol : float ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [syevr_opt_liwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a] @return
    the optimum length of the iwork-array used by the [syevr]-function
    given matrix [a], optionally its logical dimension [n] and whether
    the eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevr_opt_l_li_work :
  ?n : int ->
  ?vectors : bool ->
  ?range : [ `A | `V of float * float | `I of int * int ] ->
  ?up : bool ->
  ?abstol : float ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int * int
(** [syevr_opt_l_li_iwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a]
    @return the tuple of optimum lengths of the work- and iwork-arrays
    respectively, used by the [syevr]-function given matrix [a],
    optionally its logical dimension [n] and whether the eigenvectors
    must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevr :
  ?n : int ->
  ?vectors : bool ->
  ?range : [ `A | `V of float * float | `I of int * int ] ->
  ?up : bool ->
  ?abstol : float ->
  ?work : vec ->
  ?iwork : int_vec ->
  ?ofsw : int ->
  ?w : vec ->
  ?zr : int ->
  ?zc : int ->
  ?z : mat ->
  ?isuppz : int_vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int * vec * mat * int_vec
(** [syevr
      ?n ?vectors ?range ?up ?abstol ?work ?iwork
      ?ofsw ?w ?zr ?zc ?z ?isuppz ?ar ?ac a]
    [range] is either [`A] for computing all eigenpairs, [`V (vl, vu)]
    defines the lower and upper range of computed eigenvalues, [`I (il,
    iu)] defines the indexes of the computed eigenpairs, which are sorted
    in ascending order.
    @return the tuple [(m, w, z, isuppz)], where [m] is the number
            of computed eigenpairs, vector [w] contains the computed
            eigenvalues in ascending order, [z] contains the computed
            eigenvectors in same order, and [isuppz] indicates the
            nonzero elements in [z].
    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param range default = `A
    @param up default = true i.e., upper triangle of [a] is stored
    @param abstol default = result of calling [lamch `S]
    @param work default = vec of optimum length (-> [syev_opt_lwork])
    @param iwork default = int_vec of optimum length (-> [syevr_opt_liwork])
    @param ofsw default = 1 ot ignored if [w] is not given
    @param w default = vec of length [n]
    @param zr default = 1
    @param zc default = 1
    @param z default = matrix with minimal required dimension
    @param isuppz default = [int_vec] with minimal required dimension
    @param ar default = 1
    @param ac default = 1 *)
