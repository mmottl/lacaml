(* File: impl_SDCZ.mli

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

(** Interface to BLAS/LAPACK *)

open Bigarray
open Common
open Numberxx

(** {6 BLAS-1 interface} *)

val swap :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  x : vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> unit
(** [swap ?n ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
    @param n default = 1
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val scal : ?n : int -> num_type -> ?ofsx : int -> ?incx : int -> vec -> unit
(** [scal ?n alpha ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val copy :
  ?n : int ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
(** [copy ?n ?ofsy ?incy ?y ?ofsx ?incx x] see BLAS documentation!
    @return vector [y], which is overwritten.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1 *)

val axpy :
  ?n : int ->
  ?alpha : num_type ->
  ?ofsx : int ->
  ?incx : int ->
  x : vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> unit
(** [axpy ?n ?alpha ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param alpha default = 1.0
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val iamax : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> int
(** [iamax ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val amax :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> num_type
(** [amax ?n ?ofsx ?incx x] @return the greater of the absolute
    values of the elements of the vector [x].
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 BLAS-2 interface} *)

val gemv :
  ?m : int ->
  ?n : int ->
  ?beta : float  ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?trans : trans3 ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
(** [gemv ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @return vector [y], which is overwritten.
    @param m default = number of available rows in matrix [a]
    @param n default = available columns in matrix [a]
    @param beta default = 0.0
    @param ofsy default = 1
    @param incy default = 1
    @param y default = vec with minimal required length (see BLAS)
    @param trans default = `N
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val gbmv :
  ?m : int ->
  ?n : int ->
  ?beta : float ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?trans : trans3 ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int ->
  int ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
(** [gbmv
      ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a kl ku ?ofsx ?incx x]
    see BLAS documentation!
    @return vector [y], which is overwritten.
    @param m default = same as [n] (i.e., [a] is a square matrix)
    @param n default = available number of columns in matrix [a]
    @param beta default = 0.0
    @param ofsy default = 1
    @param incy default = 1
    @param y default = vector with minimal required length (see BLAS)
    @param trans default = `N
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val symv :
  ?n : int ->
  ?beta : float ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?up : bool ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
(** [symv ?n ?beta ?ofsy ?incy ?y ?up ?alpha ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @return vector [y], which is overwritten.
    @param n default = dimension of symmetric matrix [a]
    @param beta default = 0.0
    @param ofsy default = 1
    @param incy default = 1
    @param y default = vector with minimal required length (see BLAS)
    @param up default = true (upper triangular portion of [a] is accessed)
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val trmv :
  ?n : int ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> unit
(** [trmv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @param n default = dimension of triangular matrix [a]
    @param trans default = `N
    @param diag default = false (not a unit triangular matrix)
    @param up default = true (upper triangular portion of [a] is accessed)
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val trsv :
  ?n : int ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> unit
(** [trsv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @param n default = dimension of triangular matrix [a]
    @param trans default = `N
    @param diag default = false (not a unit triangular matrix)
    @param up default = true (upper triangular portion of [a] is accessed)
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 BLAS-3 interface} *)

val gemm :
  ?m : int ->
  ?n : int ->
  ?k : int ->
  ?beta : float ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?transa : trans3 ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?transb : trans3 ->
  ?br : int ->
  ?bc : int ->
  mat
  -> mat
(** [gemm ?m ?n ?k ?beta ?cr ?cc ?c ?transa ?alpha ?ar ?ac a ?transb ?br ?bc b]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param m default = number of rows of [a] (or tr [a]) and [c]
    @param n default = number of columns of [b] (or tr [b]) and [c]
    @param k default = number of columns of [a] (or tr [a]) and
                       number of rows of [b] (or tr [b])
    @param beta default = 0.0
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param transa default = `N
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param transb default = `N
    @param br default = 1
    @param bc default = 1 *)

val symm :
  ?m : int ->
  ?n : int ->
  ?side : side ->
  ?up : bool ->
  ?beta : float ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat
  -> mat
(** [symm ?m ?n ?side ?up ?beta ?cr ?cc ?c ?alpha ?ar ?ac a ?br ?bc b]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param m default = number of rows of [c]
    @param n default = number of columns of [c]
    @param side default = `L (left - multiplication is [a][b])
    @param up default = true (upper triangular portion of [a] is accessed)
    @param beta default = 0.0
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1 *)

val trmm :
  ?m : int ->
  ?n : int ->
  ?side : side ->
  ?up : bool ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?br : int ->
  ?bc : int ->
  b : mat ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> unit
(** [trmm ?m ?n ?side ?up ?trans ?diag ?br ?bc ~b ?alpha ?ar ?ac a]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param m default = number of rows of [c]
    @param n default = number of columns of [c]
    @param side default = `L (left - multiplication is [a][b])
    @param up default = true (upper triangular portion of [a] is accessed)
    @param trans default = `N
    @param diag default = `N (non-unit)
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1 *)

val syrk :
  ?n : int ->
  ?k : int ->
  ?up : bool ->
  ?beta : float ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?trans : trans3 ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> mat
(** [syrk ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param n default = number of rows of [a] (or [a]'), [c]
    @param k default = number of columns of [a] (or [a]')
    @param up default = true (upper triangular portion of [c] is accessed)
    @param beta default = 0.0
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param trans default = `N
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1 *)

val syr2k :
  ?n : int ->
  ?k : int ->
  ?up : bool ->
  ?beta : float ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?trans : trans3 ->
  ?alpha : float ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat
  -> mat
(** [syr2k ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a ?br ?bc b]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param n default = number of rows of [a] (or [a]'), [c]
    @param k default = number of columns of [a] (or [a]')
    @param up default = true (upper triangular portion of [c] is accessed)
    @param beta default = 0.0
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param trans default = `N
    @param alpha default = 1.0
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)


(** {6 LAPACK interface} *)

(** Auxiliary routines *)

val lange_min_lwork : int -> norm4 -> int
(** [lange_min_lwork m norm]
    @return the minimum length of the work array used by the [lange]-function.
    @param m the number of rows in the matrix
    @param norm type of norm that will be computed by [lange] *)

val lange :
  ?m : int ->
  ?n : int ->
  ?norm : norm4 ->
  ?work : rvec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> float
(** [lange ?m ?n ?norm ?work ?ar ?ac a]
    @param m default = number of rows of matrix [a]
    @param n default = number of columns of matrix [a]
    @param norm default = `O
    @param work default = allocated work space for norm `I
    @param ar default = 1
    @param ac default = 1 *)


(** Linear equations (computational routines) *)

val getrf :
  ?m : int ->
  ?n : int ->
  ?ipiv : int_vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int_vec
(** [getrf ?m ?n ?ipiv ?ar ?ac a] @return [ipiv].
    @raise Failure if the matrix is singular.
    @param m default = number of rows in matrix [a]
    @param n default = number of columns in matrix [a]
    @param ipiv = vec of length [min(m, n)]
    @param ar default = 1
    @param ac default = 1 *)

val getrs :
  ?n : int ->
  ?ipiv : int_vec ->
  ?trans : trans3 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [getrs ?n ?ipiv ?trans ?ar ?ac a ?nrhs ?br ?bc b].  Note that matrix
    [a] will be passed to [getrf] if [ipiv] was not provided.
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param ipiv default = result from [getrf] applied to [a]
    @param trans default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val getri_min_lwork : int -> int
(** [getri_min_lwork n] @return the minimum length of the
    work array used by the [getri]-function if the matrix has [n] columns. *)

val getri_opt_lwork :
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [getri_opt_lwork ?n ?ar ?ac a] @return the optimal size of the
    work array used by the [getri]-function.
    @param n default = number of columns of matrix [a]
    @param ar default = 1
    @param ac default = 1 *)

val getri :
  ?n : int ->
  ?ipiv : int_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> unit
(** [getri ?n ?ipiv ?work ?ar ?ac a]
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param ipiv default = vec of length [m] from getri
    @param work default = vec of optimum length
    @param ar default = 1
    @param ac default = 1 *)

val sytrf_min_lwork : unit -> int
(** [sytrf_min_lwork ()] @return the minimum length of the
    work array used by the [sytrf]-function. *)

val sytrf_opt_lwork :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [sytrf_opt_lwork ?n ?up ?ar ?ac a] @return the optimal size of the
    work array used by the [sytrf]-function.
    @param n default = number of columns of matrix [a]
    @param up default = true (store upper triangle in [a])
    @param a the matrix [a]
    @param ar default = 1
    @param ac default = 1 *)

val sytrf :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int_vec
(** [sytrf ?n ?up ?ipiv ?work ?ar ?ac a]
    @raise Failure if D in A = U*D*U' or L*D*L' is singular.
    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ipiv = vec of length n
    @param work default = vec of optimum length
    @param ar default = 1
    @param ac default = 1 *)

val sytrs :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int_vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [sytrs ?n ?up ?ipiv ?ar ?ac a ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ipiv default = vec of length [n]
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val sytri_min_lwork : int -> int
(** [sytri_min_lwork n] @return the minimum length of the
    work array used by the [sytri]-function if the matrix has [n] columns. *)

val sytri :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> unit
(** [sytri ?n ?up ?ipiv ?work ?ar ?ac a]
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ipiv default = vec of length [n] from sytrf
    @param work default = vec of optimum length
    @param ar default = 1
    @param ac default = 1 *)

val potrf :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  ?jitter : num_type ->
  mat
  -> unit
(** [potrf ?n ?up ?ar ?ac ?jitter a] factorizes symmetric positive
    definite matrix [a] (or the designated submatrix) using Cholesky
    factorization.

    Due to rounding errors ill-conditioned matrices may actually appear
    as if they were not positive definite, thus leading to an exception.
    One remedy for this problem is to add a small [jitter] to the
    diagonal of the matrix, which will usually allow Cholesky to complete
    successfully (though at a small bias).  For extremely ill-conditioned
    matrices it is recommended to use (symmetric) eigenvalue decomposition
    instead of this function for a numerically more stable factorization.

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ar default = 1
    @param ac default = 1
    @param jitter default = nothing
*)

val potrs :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  ?factorize : bool ->
  ?jitter : num_type ->
  mat
  -> unit
(** [potrs ?n ?up ?ar ?ac a ?nrhs ?br ?bc ?factorize ?jitter b]

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1
    @param factorize default = true (calls potrf implicitly)
    @param jitter default = nothing
*)

val potri :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  ?factorize : bool ->
  ?jitter : num_type ->
  mat
  -> unit
(** [potri ?n ?up ?ar ?ac ?factorize ?jitter a]

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true (upper triangle stored in [a])
    @param ar default = 1
    @param ac default = 1
    @param factorize default = true (calls potrf implicitly)
    @param jitter default = nothing
*)

val trtrs :
  ?n : int ->
  ?up : bool ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [trtrs ?n ?up ?trans ?diag ?ar ?ac a ?nrhs ?br ?bc b]

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true
    @param trans default = `N
    @param diag default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1
*)

val trtri :
  ?n : int ->
  ?up : bool ->
  ?diag : diag ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> unit
(** [trtri ?n ?up ?diag ?ar ?ac a]

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true (upper triangle stored in [a])
    @param diag default = `N
    @param ar default = 1
    @param ac default = 1
*)

val geqrf_opt_lwork :
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> int
(** [geqrf_opt_lwork ?m ?n ?ar ?ac a] @return the optimum
    length of the work-array used by the [geqrf]-function given matrix
    [a] and optionally its logical dimensions [m] and [n].

    @param m default = number of rows in matrix [a]
    @param n default = number of columns in matrix [a]
    @param ar default = 1
    @param ac default = 1
*)

val geqrf_min_lwork : n : int -> int
(** [geqrf_min_lwork ~n] @return the minimum length of the
    work-array used by the [geqrf]-function if the matrix has [n]
    columns. *)

val geqrf :
  ?m : int ->
  ?n : int ->
  ?work : vec ->
  ?tau : vec ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> vec
(** [geqrf ?m ?n ?work ?tau ?ar ?ac a] @return [tau].
    @param m default = number of rows in matrix [a]
    @param n default = number of columns in matrix [a]
    @param work default = vec of optimum length
    @param tau default = vec of required length
    @param ar default = 1
    @param ac default = 1 *)


(** Linear equations (simple drivers) *)

val gesv :
  ?n : int ->
  ?ipiv : int_vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [gesv ?n ?ipiv ?ar ?ac a ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [a]
    @param ipiv default = vec of length [n]
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val gbsv :
  ?n : int ->
  ?ipiv : int_vec ->
  ?abr : int ->
  ?abc : int ->
  mat ->
  int ->
  int ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [gbsv ?n ?ipiv ?abr ?abc ab kl ku ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [ab]
    @param ipiv default = vec of length [n]
    @param abr default = 1
    @param abc default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val gtsv :
  ?n : int ->
  ?ofsdl : int ->
  vec ->
  ?ofsd : int ->
  vec ->
  ?ofsdu : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available length of vector [d]
    @param ofsdl default = 1
    @param ofsd default = 1
    @param ofsdu default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val posv :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [posv ?n ?up ?ar ?ac a ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [a]
    @param up default = true i.e., upper triangle of [a] is stored
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val ppsv :
  ?n : int ->
  ?up : bool ->
  ?ofsap : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [ppsv ?n ?up ?ofsap ap ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
    @param up default = true i.e., upper triangle of [ap] is stored
    @param ofsap default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val pbsv :
  ?n : int ->
  ?up : bool ->
  ?kd : int ->
  ?abr : int ->
  ?abc : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [pbsv ?n ?up ?kd ?abr ?abc ab ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [ab]
    @param up default = true i.e., upper triangle of [ab] is stored
    @param kd default = available number of rows in matrix [ab] - 1
    @param abr default = 1
    @param abc default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val ptsv :
  ?n : int ->
  ?ofsd : int ->
  vec ->
  ?ofse : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [ptsv ?n ?ofsd d ?ofse e ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available length of vector [d]
    @param ofsd default = 1
    @param ofse default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val sysv_opt_lwork :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [sysv_opt_lwork ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
    length of the work-array used by the [sysv]-function given matrix
    [a], optionally its logical dimension [n] and given right hand side
    matrix [b] with an optional number [nrhs] of vectors.
    @param n default = available number of columns in matrix [a]
    @param up default = true i.e., upper triangle of [a] is stored
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val sysv :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [sysv ?n ?up ?ipiv ?work ?ar ?ac a ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [a]
    @param up default = true i.e., upper triangle of [a] is stored
    @param ipiv default = vec of length [n]
    @param work default = vec of optimum length (-> [sysv_opt_lwork])
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val spsv :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int_vec ->
  ?ofsap : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [spsv ?n ?up ?ipiv ?ofsap ap ?nrhs ?br ?bc b]
    @raise Failure if the matrix is singular.
    @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
    @param up default = true i.e., upper triangle of [ap] is stored
    @param ipiv default = vec of length [n]
    @param ofsap default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)


(** Least squares (simple drivers) *)

val gels_min_lwork : m : int -> n : int -> nrhs : int -> int
(** [gels_min_lwork ~m ~n ~nrhs] @return the minimum length of the
    work-array used by the [gels]-function if the logical dimensions
    of the matrix are [m] rows and [n] columns and if there are [nrhs]
    right hand side vectors. *)

val gels_opt_lwork :
  ?m : int ->
  ?n : int ->
  ?trans : trans2 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> int
(** [gels_opt_lwork ?m ?n ?trans ?ar ?ac a ?nrhs ?br ?bc b] @return
    the optimum length of the work-array used by the [gels]-function given
    matrix [a], optionally its logical dimensions [m] and [n] and given
    right hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param trans default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val gels :
  ?m : int ->
  ?n : int ->
  ?work : vec ->
  ?trans : trans2 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [gels ?m ?n ?work ?trans ?ar ?ac a ?nrhs ?br ?bc b] see
    LAPACK documentation!
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param work default = vec of optimum length (-> [gels_opt_lwork])
    @param trans default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)
