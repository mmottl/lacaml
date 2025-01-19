(* File: impl_SD.mli

   Copyright © 2001-

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

open Common
open Floatxx

(** {4 BLAS-1 Interface} *)

val dot :
  ?n:int ->
  ?ofsx:int ->
  ?incx:int ->
  vec ->
  ?ofsy:int ->
  ?incy:int ->
  vec ->
  float
(** [dot ?n ?ofsx ?incx x ?ofsy ?incy y] see BLAS documentation!

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val asum : ?n:int -> ?ofsx:int -> ?incx:int -> vec -> float
(** [asum ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

(** {4 BLAS-2 Interface} *)

val sbmv :
  ?n:int ->
  ?k:int ->
  ?ofsy:int ->
  ?incy:int ->
  ?y:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?up:bool ->
  ?alpha:float ->
  ?beta:float ->
  ?ofsx:int ->
  ?incx:int ->
  vec ->
  vec
(** [sbmv ?n ?k ?ofsy ?incy ?y ?ar ?ac a ?up ?alpha ?beta ?ofsx ?incx x] see
    BLAS documentation!

    @return vector [y], which is overwritten.

    @param n default = number of available columns to the right of [ac].
    @param k default = number of available rows in matrix [a] - 1
    @param ofsy default = 1
    @param incy default = 1
    @param ar default = 1
    @param ac default = 1
    @param y default = uninitialized vector of minimal length (see BLAS)
    @param up default = true i.e., upper band of [a] is supplied
    @param alpha default = 1.0
    @param beta default = 0.0
    @param ofsx default = 1
    @param incx default = 1 *)

val ger :
  ?m:int ->
  ?n:int ->
  ?alpha:float ->
  ?ofsx:int ->
  ?incx:int ->
  vec ->
  ?ofsy:int ->
  ?incy:int ->
  vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  unit
(** [ger ?m ?n ?alpha ?ofsx ?incx x ?ofsy ?incy y n ?ar ?ac a] see BLAS
    documentation!

    @param m default = number of rows of [a]
    @param n default = number of columns of [a]
    @param alpha default = 1.0
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
    @param ar default = 1
    @param ac default = 1 *)

val syr :
  ?n:int ->
  ?alpha:float ->
  ?up:bool ->
  ?ofsx:int ->
  ?incx:int ->
  vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  unit
(** [syr ?n ?alpha ?up ?ofsx ?incx x ?ar ?ac a] see BLAS documentation!

    @param n default = number of rows of [a]
    @param alpha default = 1.0
    @param up default = true i.e., upper triangle of [a] is supplied
    @param ofsx default = 1
    @param incx default = 1
    @param ar default = 1
    @param ac default = 1 *)

(** {4 LAPACK Interface} *)

(** {5 Auxiliary Routines} *)

val lansy_min_lwork : int -> norm4 -> int
(** [lansy_min_lwork m norm]
    @return the minimum length of the work array used by the [lansy]-function.
    @param norm type of norm that will be computed by [lansy]
    @param n the number of columns (and rows) in the matrix *)

val lansy :
  ?n:int ->
  ?up:bool ->
  ?norm:norm4 ->
  ?work:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  float
(** [lansy ?norm ?up ?n ?ar ?ac ?work a] see LAPACK documentation!
    @param norm default = `O
    @param up default = true (reference upper triangular part of [a])
    @param n default = number of columns of matrix [a]
    @param work default = allocated work space for norm `I *)

val lamch : [ `E | `S | `B | `P | `N | `R | `M | `U | `L | `O ] -> float
(** [lamch cmach] see LAPACK documentation! *)

(** {5 Linear Equations (computational routines)} *)

(* ORGQR *)

val orgqr_min_lwork : n:int -> int
(** [orgqr_min_lwork ~n]

    @return
      the minimum length of the work-array used by the [orgqr]-function if the
      matrix has [n] columns. *)

val orgqr_opt_lwork :
  ?m:int -> ?n:int -> ?k:int -> tau:vec -> ?ar:int -> ?ac:int -> mat -> int
(** [orgqr_opt_lwork ?m ?n ?k ~tau ?ar ?ac a]

    @return
      the optimum length of the work-array used by the [orgqr]-function given
      matrix [a], optionally its logical dimensions [m] and [n], and the number
      of reflectors [k].

    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param k default = available number of elements in vector [tau] *)

val orgqr :
  ?m:int ->
  ?n:int ->
  ?k:int ->
  ?work:vec ->
  tau:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  unit
(** [orgqr ?m ?n ?k ?work ~tau ?ar ?ac a] see LAPACK documentation!

    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param k default = available number of elements in vector [tau] *)

(* ORMQR *)

val ormqr_opt_lwork :
  ?side:side ->
  ?trans:trans2 ->
  ?m:int ->
  ?n:int ->
  ?k:int ->
  tau:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?cr:int ->
  ?cc:int ->
  mat ->
  int
(** [ormqr_opt_lwork ?side ?trans ?m ?n ?k ~tau ?ar ?ac a ?cr ?cc c]
    @return
      the optimum length of the work-array used by the [ormqr]-function given
      matrix [a] and [b], optionally its logical dimensions [m] and [n], and the
      number of reflectors [k].

    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param k default = available number of elements in vector [tau] *)

val ormqr :
  ?side:side ->
  ?trans:trans2 ->
  ?m:int ->
  ?n:int ->
  ?k:int ->
  ?work:vec ->
  tau:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?cr:int ->
  ?cc:int ->
  mat ->
  unit
(** [ormqr ?side ?trans ?m ?n ?k ?work ~tau ?ar ?ac a ?cr ?cc c] see LAPACK
    documentation!

    @param side default = [`L]
    @param trans default = [`N]
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param k default = available number of elements in vector [tau] *)

(* GECON *)

val gecon_min_lwork : int -> int
(** [gecon_min_lwork n]

    @return the minimum length of the work array used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to the [gecon]-function
*)

val gecon_min_liwork : int -> int
(** [gecon_min_liwork n]

    @return the minimum length of the iwork array used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to [gecon]-function *)

val gecon :
  ?n:int ->
  ?norm:norm2 ->
  ?anorm:float ->
  ?work:vec ->
  ?iwork:int32_vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  float
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
(** [sycon_min_lwork n]

    @return the minimum length of the work array used by the [sycon]-function.
    @param n the logical dimensions of the matrix given to the [sycon]-function
*)

val sycon_min_liwork : int -> int
(** [sycon_min_liwork n]

    @return the minimum length of the iwork array used by the [sycon]-function.
    @param n the logical dimensions of the matrix given to [sycon]-function *)

val sycon :
  ?n:int ->
  ?up:bool ->
  ?ipiv:int32_vec ->
  ?anorm:float ->
  ?work:vec ->
  ?iwork:int32_vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  float
(** [sycon ?n ?up ?ipiv ?anorm ?work ?iwork ?ar ?ac a]
    @return
      estimate of the reciprocal of the condition number of symmetric matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of the factorization of [a] is stored
    @param ipiv default = vec of length [n]
    @param anorm default = 1-norm of the matrix [a] as returned by [lange]
    @param work default = automatically allocated workspace
    @param iwork default = automatically allocated workspace *)

(* POCON *)

val pocon_min_lwork : int -> int
(** [pocon_min_lwork n]

    @return the minimum length of the work array used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to the [pocon]-function
*)

val pocon_min_liwork : int -> int
(** [pocon_min_liwork n]

    @return the minimum length of the iwork array used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to [pocon]-function *)

val pocon :
  ?n:int ->
  ?up:bool ->
  ?anorm:float ->
  ?work:vec ->
  ?iwork:int32_vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  float
(** [pocon ?n ?up ?anorm ?work ?iwork ?ar ?ac a]
    @return
      estimate of the reciprocal of the condition number of symmetric positive
      definite matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up
      default = upper triangle of Cholesky factorization of [a] is stored
    @param work default = automatically allocated workspace
    @param iwork default = automatically allocated workspace
    @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)

(** {5 Least Squares (expert drivers)} *)

val gelsy_min_lwork : m:int -> n:int -> nrhs:int -> int
(** [gelsy_min_lwork ~m ~n ~nrhs]

    @return
      the minimum length of the work-array used by the [gelsy]-function if the
      logical dimensions of the matrix are [m] rows and [n] columns and if there
      are [nrhs] right hand side vectors. *)

val gelsy_opt_lwork :
  ?m:int ->
  ?n:int ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?nrhs:int ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [gelsy_opt_lwork ?m ?n ?ar ?ac a ?nrhs ?br ?bc b]

    @return
      the optimum length of the work-array used by the [gelsy]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param nrhs default = available number of columns in matrix [b] *)

val gelsy :
  ?m:int ->
  ?n:int ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?rcond:float ->
  ?jpvt:int32_vec ->
  ?work:vec ->
  ?nrhs:int ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [gelsy ?m ?n ?ar ?ac a ?rcond ?jpvt ?ofswork ?work ?nrhs b] see LAPACK
    documentation!

    @return the effective rank of [a]
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param rcond default = (-1) => machine precision
    @param jpvt default = vec of length [n]
    @param work default = vec of optimum length (-> [gelsy_opt_lwork])
    @param nrhs default = available number of columns in matrix [b] *)

val gelsd_min_lwork : m:int -> n:int -> nrhs:int -> int
(** [gelsd_min_lwork ~m ~n ~nrhs]

    @return
      the minimum length of the work-array used by the [gelsd]-function if the
      logical dimensions of the matrix are [m] and [n] and if there are [nrhs]
      right hand side vectors. *)

val gelsd_opt_lwork :
  ?m:int ->
  ?n:int ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?nrhs:int ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [gelsd_opt_lwork ?m ?n ?ar ?ac a ?nrhs b]

    @return
      the optimum length of the work-array used by the [gelsd]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param nrhs default = available number of columns in matrix [b] *)

val gelsd_min_iwork : int -> int -> int
(** [gelsd_min_iwork m n]

    @return
      the minimum (= optimum) length of the iwork-array used by the
      [gelsd]-function if the logical dimensions of the matrix are [m] and [n].
*)

val gelsd :
  ?m:int ->
  ?n:int ->
  ?rcond:float ->
  ?ofss:int ->
  ?s:vec ->
  ?work:vec ->
  ?iwork:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?nrhs:int ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [gelsd ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs b] see LAPACK
    documentation!
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

val gelss_min_lwork : m:int -> n:int -> nrhs:int -> int
(** [gelss_min_lwork ~m ~n ~nrhs]

    @return
      the minimum length of the work-array used by the [gelss]-function if the
      logical dimensions of the matrix are [m] rows and [n] columns and if there
      are [nrhs] right hand side vectors. *)

val gelss_opt_lwork :
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?m:int ->
  ?n:int ->
  ?nrhs:int ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [gelss_opt_lwork ?ar ?ac a ?m ?n ?nrhs ?br ?bc b]

    @return
      the optimum length of the work-array used by the [gelss]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param nrhs default = available number of columns in matrix [b] *)

val gelss :
  ?m:int ->
  ?n:int ->
  ?rcond:float ->
  ?ofss:int ->
  ?s:vec ->
  ?work:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?nrhs:int ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [gelss ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs ?br ?bc b] see
    LAPACK documentation!
    @return the effective rank of [a].
    @raise Failure if the function fails to converge.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param rcond default = (-1) => machine precision
    @param ofss default = 1 or ignored if [s] is not given
    @param s default = vec of length [min m n]
    @param work default = vec of optimum length (-> [gelss_opt_lwork])
    @param nrhs default = available number of columns in matrix [b] *)

(** {5 General Schur Factorization} *)

val gees :
  ?n:int ->
  ?jobvs:Common.schur_vectors ->
  ?sort:Common.eigen_value_sort ->
  ?wr:vec ->
  ?wi:vec ->
  ?vsr:int ->
  ?vsc:int ->
  ?vs:mat ->
  ?work:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int * vec * vec * mat
(** [gees ?n ?jobvs ?sort ?w ?vsr ?vsc ?vs ?work ?ar ?ac a] See [gees]-function
    for details about arguments.
    @return (sdim, wr, wi, vs) *)

(** {5 General SVD Routines} *)

val gesvd_min_lwork : m:int -> n:int -> int
(** [gesvd_min_lwork ~m ~n]

    @return
      the minimum length of the work array used by the [gesvd]-function for
      matrices with [m] rows and [n] columns. *)

val gesvd_opt_lwork :
  ?m:int ->
  ?n:int ->
  ?jobu:svd_job ->
  ?jobvt:svd_job ->
  ?s:vec ->
  ?ur:int ->
  ?uc:int ->
  ?u:mat ->
  ?vtr:int ->
  ?vtc:int ->
  ?vt:mat ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int

val gesvd :
  ?m:int ->
  ?n:int ->
  ?jobu:svd_job ->
  ?jobvt:svd_job ->
  ?s:vec ->
  ?ur:int ->
  ?uc:int ->
  ?u:mat ->
  ?vtr:int ->
  ?vtc:int ->
  ?vt:mat ->
  ?work:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  vec * mat * mat

val gesdd_liwork : m:int -> n:int -> int

val gesdd_min_lwork : ?jobz:svd_job -> m:int -> n:int -> unit -> int
(** [gesdd_min_lwork ?jobz ~m ~n]

    @return
      the minimum length of the work array used by the [gesdd]-function for
      matrices with [m] rows and [n] columns for SVD-job [jobz]. *)

val gesdd_opt_lwork :
  ?m:int ->
  ?n:int ->
  ?jobz:svd_job ->
  ?s:vec ->
  ?ur:int ->
  ?uc:int ->
  ?u:mat ->
  ?vtr:int ->
  ?vtc:int ->
  ?vt:mat ->
  ?iwork:int32_vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int

val gesdd :
  ?m:int ->
  ?n:int ->
  ?jobz:svd_job ->
  ?s:vec ->
  ?ur:int ->
  ?uc:int ->
  ?u:mat ->
  ?vtr:int ->
  ?vtc:int ->
  ?vt:mat ->
  ?work:vec ->
  ?iwork:int32_vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  vec * mat * mat

(** {5 General Eigenvalue Problem (simple drivers)} *)

val geev_min_lwork : ?vectors:bool -> int -> int
(** [geev_min_lwork vectors n]

    @return
      the minimum length of the work array used by the [geev]-function.
      [vectors] indicates whether eigenvectors are supposed to be computed.
    @param n the logical dimensions of the matrix given to [geev]-function
    @param vectors default = true *)

val geev_opt_lwork :
  ?n:int ->
  ?vlr:int ->
  ?vlc:int ->
  ?vl:mat option ->
  ?vrr:int ->
  ?vrc:int ->
  ?vr:mat option ->
  ?ofswr:int ->
  ?wr:vec ->
  ?ofswi:int ->
  ?wi:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int
(** [geev_opt_lwork ?n ?vlr ?vlc ?vl ?vrr ?vrc ?vr ?ofswr wr ?ofswi wi ?ar ?ac
     a] See [geev]-function for details about arguments.
    @return "optimal" size of work array. *)

val geev :
  ?n:int ->
  ?work:vec ->
  ?vlr:int ->
  ?vlc:int ->
  ?vl:mat option ->
  ?vrr:int ->
  ?vrc:int ->
  ?vr:mat option ->
  ?ofswr:int ->
  ?wr:vec ->
  ?ofswi:int ->
  ?wi:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  mat * vec * vec * mat
(** [geev ?work ?n ?vlr ?vlc ?vl ?vrr ?vrc ?vr ?ofswr ?wr ?ofswi ?wi ?ar ?ac a]
    @return
      ([lv], [wr], [wi], [rv]), where [wr] and [wv] are the real and imaginary
      components of the eigenvalues, and [lv] and [rv] are the left and right
      eigenvectors. [lv] ([rv]) is the empty matrix if [vl] ([vr]) is set to
      [None].
    @raise Failure if the function fails to converge
    @param n default = available number of columns of matrix [a]
    @param work default = automatically allocated workspace
    @param vl
      default = Automatically allocated left eigenvectors. Pass [None] if you do
      not want to compute them, [Some lv] if you want to provide the storage.
      You can set [vlr], [vlc] in the last case. (See LAPACK GEEV docs for
      details about storage of complex eigenvectors)
    @param vr
      default = Automatically allocated right eigenvectors. Pass [None] if you
      do not want to compute them, [Some rv] if you want to provide the storage.
      You can set [vrr], [vrc] in the last case.
    @param wr default = vector of size [n]; real components of the eigenvalues
    @param wi
      default = vector of size [n]; imaginary components of the eigenvalues
    @param a the matrix whose eigensystem is computed *)

(** {5 Symmetric-matrix Eigenvalue and Singular Value Problems (simple drivers)}
*)

val syev_min_lwork : int -> int
(** [syev_min_lwork n]

    @return
      the minimum length of the work-array used by the {!syev}-function if the
      logical dimensions of the matrix are [n]. *)

val syev_opt_lwork :
  ?n:int -> ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
(** [syev_opt_lwork ?n ?vectors ?up ?ar ?ac a]

    @return
      the optimum length of the work-array used by the {!syev}-function given
      matrix [a], optionally its logical dimension [n] and whether the
      eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syev :
  ?n:int ->
  ?vectors:bool ->
  ?up:bool ->
  ?work:vec ->
  ?ofsw:int ->
  ?w:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  vec
(** [syev ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a] computes all
    eigenvalues and, optionally, eigenvectors of the real symmetric matrix [a].

    @return the vector [w] of eigenvalues in ascending order.
    @raise Failure if the function fails to converge.
    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param up default = true i.e., upper triangle of [a] is stored
    @param work default = vec of optimum length (-> {!syev_opt_lwork})
    @param ofsw default = 1 or ignored if [w] is not given
    @param w default = vec of length [n] *)

val syevd_min_lwork : vectors:bool -> int -> int
(** [syevd_min_lwork vectors n]

    @return
      the minimum length of the work-array used by the {!syevd}-function if the
      logical dimensions of the matrix are [n] and given whether eigenvectors
      should be computed ([vectors]). *)

val syevd_min_liwork : vectors:bool -> int -> int
(** [syevd_min_liwork vectors n]

    @return
      the minimum length of the iwork-array used by the {!syevd}-function if the
      logical dimensions of the matrix are [n] and given whether eigenvectors
      should be computed ([vectors]). *)

val syevd_opt_lwork :
  ?n:int -> ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
(** [syevd_opt_lwork ?n ?vectors ?up ?ar ?ac a]

    @return
      the optimum length of the work-array used by the {!syevd}-function given
      matrix [a], optionally its logical dimension [n] and whether the
      eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevd_opt_liwork :
  ?n:int -> ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
(** [syevd_opt_liwork ?n ?vectors ?up ?ar ?ac a]

    @return
      the optimum length of the iwork-array used by the {!syevd}-function given
      matrix [a], optionally its logical dimension [n] and whether the
      eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevd_opt_l_li_work :
  ?n:int -> ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int * int
(** [syevd_opt_l_li_iwork ?n ?vectors ?up ?ar ?ac a]

    @return
      the tuple of optimum lengths of the work- and iwork-arrays respectively,
      used by the {!syevd}-function given matrix [a], optionally its logical
      dimension [n] and whether the eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevd :
  ?n:int ->
  ?vectors:bool ->
  ?up:bool ->
  ?work:vec ->
  ?iwork:int32_vec ->
  ?ofsw:int ->
  ?w:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  vec
(** [syevd ?n ?vectors ?up ?ofswork ?work ?iwork ?ofsw ?w ?ar ?ac a] computes
    all eigenvalues and, optionally, eigenvectors of the real symmetric matrix
    [a]. If eigenvectors are desired, it uses a divide and conquer algorithm.

    @return the vector [w] of eigenvalues in ascending order.
    @raise Failure if the function fails to converge.
    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param up default = true i.e., upper triangle of [a] is stored
    @param work default = vec of optimum length (-> {!syev_opt_lwork})
    @param iwork default = int32_vec of optimum length (-> {!syevd_opt_liwork})
    @param ofsw default = 1 or ignored if [w] is not given
    @param w default = vec of length [n] *)

val sbev_min_lwork : int -> int
(** [sbev_min_lwork n]

    @return
      the minimum length of the work-array used by the {!sbev}-function if the
      logical dimensions of the matrix are [n]. *)

val sbev :
  ?n:int ->
  ?kd:int ->
  ?zr:int ->
  ?zc:int ->
  ?z:mat ->
  ?up:bool ->
  ?work:vec ->
  ?ofsw:int ->
  ?w:vec ->
  ?abr:int ->
  ?abc:int ->
  mat ->
  vec
(** [sbev ?n ?vectors ?zr ?zc ?z ?up ?ofswork ?work ?ofsw ?w ?abr ?abc ab]
    computes all the eigenvalues and, optionally, eigenvectors of the real
    symmetric {i band} matrix [ab].

    @raise Failure if the function fails to converge.

    @return the vector [w] of eigenvalues in ascending order.
    @raise Failure if the function fails to converge.
    @param n default = available number of columns of matrix [ab]
    @param z
      matrix to contain the orthonormal eigenvectors of [ab], the [i]-th column
      of [z] holding the eigenvector associated with [w.{i}]. default = [None]
      i.e, eigenvectors are not computed
    @param kd default = number of rows in matrix [ab] - 1
    @param up default = true i.e., upper triangle of the matrix is stored
    @param work default = vec of minimal length (-> {!sbev_min_lwork})
    @param ofsw default = 1 or ignored if [w] is not given
    @param w default = vec of length [n]
    @param abr default = 1
    @param abc default = 1 *)

(** {5 Symmetric-matrix Eigenvalue and Singular Value Problems (expert & RRR
    drivers)} *)

val syevr_min_lwork : int -> int
(** [syevr_min_lwork n]

    @return
      the minimum length of the work-array used by the {!syevr}-function if the
      logical dimensions of the matrix are [n]. *)

val syevr_min_liwork : int -> int
(** [syevr_min_liwork n]

    @return
      the minimum length of the iwork-array used by the {!syevr}-function if the
      logical dimensions of the matrix are [n]. *)

val syevr_opt_lwork :
  ?n:int ->
  ?vectors:bool ->
  ?range:[ `A | `V of float * float | `I of int * int ] ->
  ?up:bool ->
  ?abstol:float ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int
(** [syevr_opt_lwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a]

    @return
      the optimum length of the work-array used by the {!syevr}-function given
      matrix [a], optionally its logical dimension [n] and whether the
      eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevr_opt_liwork :
  ?n:int ->
  ?vectors:bool ->
  ?range:[ `A | `V of float * float | `I of int * int ] ->
  ?up:bool ->
  ?abstol:float ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int
(** [syevr_opt_liwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a]

    @return
      the optimum length of the iwork-array used by the {!syevr}-function given
      matrix [a], optionally its logical dimension [n] and whether the
      eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevr_opt_l_li_work :
  ?n:int ->
  ?vectors:bool ->
  ?range:[ `A | `V of float * float | `I of int * int ] ->
  ?up:bool ->
  ?abstol:float ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int * int
(** [syevr_opt_l_li_iwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a]
    @return
      the tuple of optimum lengths of the work- and iwork-arrays respectively,
      used by the {!syevr}-function given matrix [a], optionally its logical
      dimension [n] and whether the eigenvectors must be computed ([vectors]).
    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored *)

val syevr :
  ?n:int ->
  ?vectors:bool ->
  ?range:[ `A | `V of float * float | `I of int * int ] ->
  ?up:bool ->
  ?abstol:float ->
  ?work:vec ->
  ?iwork:int32_vec ->
  ?ofsw:int ->
  ?w:vec ->
  ?zr:int ->
  ?zc:int ->
  ?z:mat ->
  ?isuppz:int32_vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  int * vec * mat * int32_vec
(** [syevr ?n ?vectors ?range ?up ?abstol ?work ?iwork ?ofsw ?w ?zr ?zc ?z
     ?isuppz ?ar ?ac a] [range] is either [`A] for computing all eigenpairs,
    [`V (vl, vu)] defines the lower and upper range of computed eigenvalues,
    [`I (il, iu)] defines the indexes of the computed eigenpairs, which are
    sorted in ascending order.
    @return
      the tuple [(m, w, z, isuppz)], where [m] is the number of computed
      eigenpairs, vector [w] contains the computed eigenvalues in ascending
      order, [z] contains the computed eigenvectors in same order, and [isuppz]
      indicates the nonzero elements in [z].
    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param range default = `A
    @param up default = true i.e., upper triangle of [a] is stored
    @param abstol default = result of calling [lamch `S]
    @param work default = vec of optimum length (-> {!syev_opt_lwork})
    @param iwork default = int32_vec of optimum length (-> {!syevr_opt_liwork})
    @param ofsw default = 1 or ignored if [w] is not given
    @param w default = vec of length [n]
    @param zr default = 1
    @param zc default = 1
    @param z default = matrix with minimal required dimension
    @param isuppz default = [int32_vec] with minimal required dimension
    @param ar default = 1
    @param ac default = 1 *)

val sygv_opt_lwork :
  ?n:int ->
  ?vectors:bool ->
  ?up:bool ->
  ?itype:[ `A_B | `AB | `BA ] ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?br:int ->
  ?bc:int ->
  mat ->
  int
(** [sygv_opt_lwork ?n ?vectors ?up ?ar ?ac a ?br ?bc b]

    @return
      the optimum length of the work-array used by the {!sygv}-function for the
      given matrices [a] and [b], optionally their logical dimension [n] and
      whether the eigenvectors must be computed ([vectors]).

    @param n default = available number of columns of matrix [a]
    @param vectors default = false, i.e. eigenvectors are not computed
    @param up default = true, i.e. upper triangle of [a] is stored

    @param itype
      specifies the problem type to be solved:
      - [`A_B] (default): a*x = (lambda)*a*x
      - [`AB]: a*b*x = (lambda)*x
      - [`BA]: b*a*x = (lambda)*x *)

val sygv :
  ?n:int ->
  ?vectors:bool ->
  ?up:bool ->
  ?work:vec ->
  ?ofsw:int ->
  ?w:vec ->
  ?itype:[ `A_B | `AB | `BA ] ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?br:int ->
  ?bc:int ->
  mat ->
  vec
(** [sygv ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a ?br ?bc b] computes
    all the eigenvalues, and optionally, the eigenvectors of a real generalized
    symmetric-definite eigenproblem, of the form [a*x=(lambda)*b*x],
    [a*b*x=(lambda)*x], or [b*a*x=(lambda)*x]. Here [a] and [b] are assumed to
    be symmetric and [b] is also positive definite.

    @return the vector [w] of eigenvalues in ascending order.

    @raise Failure if the function fails to converge.

    @param n default = available number of columns of matrix [a]
    @param vectors default = false i.e, eigenvectors are not computed
    @param up default = true i.e., upper triangle of [a] is stored
    @param work default = vec of optimum length (-> {!sygv_opt_lwork})
    @param ofsw default = 1 or ignored if [w] is not given
    @param w default = vec of length [n]

    @param itype
      specifies the problem type to be solved:
      - [`A_B] (default): a*x = (lambda)*a*x
      - [`AB]: a*b*x = (lambda)*x
      - [`BA]: b*a*x = (lambda)*x *)

val sbgv :
  ?n:int ->
  ?ka:int ->
  ?kb:int ->
  ?zr:int ->
  ?zc:int ->
  ?z:mat ->
  ?up:bool ->
  ?work:vec ->
  ?ofsw:int ->
  ?w:vec ->
  ?ar:int ->
  ?ac:int ->
  mat ->
  ?br:int ->
  ?bc:int ->
  mat ->
  vec
(** [sbgv ?n ?ka ?kb ?zr ?zc ?z ?up ?work ?ofsw ?w ?ar ?ac a ?br ?bc b] computes
    all the eigenvalues, and optionally, the eigenvectors of a real generalized
    symmetric-definite banded eigenproblem, of the form [a*x=(lambda)*b*x]. Here
    [a] and [b] are assumed to be symmetric and banded, and [b] is also positive
    definite.

    @return the vector [w] of eigenvalues in ascending order.

    @raise Failure if the function fails to converge.

    @param n default = available number of columns of matrix [a]
    @param ka
      the number of superdiagonals (or subdiagonals if [up = false]) of the
      matrix [a]. Default = [dim1 a - ar].
    @param kb same as [ka] but for the matrix [b].
    @param z default = [None] i.e, eigenvectors are not computed
    @param up default = [true] i.e., upper triangle of [a] is stored
    @param work default = vec of optimum length ([3 * n])
    @param ofsw default = 1 or ignored if [w] is not given
    @param w default = vec of length [n] *)
