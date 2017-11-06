(* File: mat_SDCZ.mli

   Copyright (C) 2002-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

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

(* Matrix operations *)

open Common
open Numberxx
open Types.Mat

(** {6 Creation of matrices and accessors} *)

val create : int -> int -> mat
(** [create m n] @return a matrix containing [m] rows and [n] columns. *)

val make : int -> int -> num_type -> mat
(** [make m n x] @return a matrix containing [m] rows and [n] columns
    initialized with value [x]. *)

val make0 : int -> int -> mat
(** [make0 m n x] @return a matrix containing [m] rows and [n] columns
    initialized with the zero element. *)

val of_array : num_type array array -> mat
(** [of_array ar] @return a matrix initialized from the array of arrays
    [ar].  It is assumed that the OCaml matrix is in row major order
    (standard). *)

val to_array : mat -> num_type array array
(** [to_array mat] @return an array of arrays initialized from matrix
    [mat]. *)

val of_list : num_type list list -> mat
(** [of_list ls] @return a matrix initialized from the list of lists
    [ls].  Each sublist of [ls] represents a row of the desired matrix,
    and must be of the same length. *)

val to_list : mat -> num_type list list
(** [to_array mat] @return [mat] in row major order as lists. *)

val of_col_vecs : vec array -> mat
(** [of_col_vecs ar] @return a matrix whose columns are initialized from
    the array of vectors [ar].  The vectors must be of same length. *)

val to_col_vecs : mat -> vec array
(** [to_col_vecs mat] @return an array of column vectors initialized
    from matrix [mat]. *)

val of_col_vecs_list : vec list -> mat
(** [of_col_vecs_list ar] @return a matrix whose columns are initialized from
    the list of vectors [ar]. The vectors must be of same length. *)

val to_col_vecs_list : mat -> vec list
(** [to_col_vecs_list mat] @return a list of column vectors initialized
    from matrix [mat]. *)

val as_vec : mat -> vec
(** [as_vec mat] @return a vector containing all elements of the
    matrix in column-major order.  The data is shared. *)

val init_rows : int -> int -> (int -> int -> num_type) -> mat
(** [init_cols m n f] @return a matrix containing [m] rows and [n]
    columns, where each element at [row] and [col] is initialized by the
    result of calling [f row col]. The elements are passed row-wise. *)

val init_cols : int -> int -> (int -> int -> num_type) -> mat
(** [init_cols m n f] @return a matrix containing [m] rows and [n]
    columns, where each element at [row] and [col] is initialized by the
    result of calling [f row col]. The elements are passed column-wise. *)

val create_mvec : int -> mat
(** [create_mvec m] @return a matrix with one column containing [m] rows. *)

val make_mvec : int -> num_type -> mat
(** [make_mvec m x] @return a matrix with one column containing [m] rows
    initialized with value [x]. *)

val mvec_of_array : num_type array -> mat
(** [mvec_of_array ar] @return a matrix with one column
    initialized with values from array [ar]. *)

val mvec_to_array : mat -> num_type array
(** [mvec_to_array mat] @return an array initialized with values from
    the first (not necessarily only) column vector of matrix [mat]. *)

val from_col_vec : vec -> mat
(** [from_col_vec v] @return a matrix with one column representing vector [v].
    The data is shared. *)

val from_row_vec : vec -> mat
(** [from_row_vec v] @return a matrix with one row representing vector [v].
    The data is shared. *)

val empty : mat
(** [empty], the empty matrix. *)

val identity : int -> mat
(** [identity n] @return the [n]x[n] identity matrix. *)

val of_diag :
  ?n : int ->
  ?br : int -> ?bc : int -> ?b : mat ->
  ?ofsx : int -> ?incx : int -> vec ->
  mat
(** [of_diag ?n ?br ?bc ?b ?ofsx ?incx x] @return matrix [b] with diagonal
    elements in the designated sub-matrix coming from the designated sub-vector
    in [x].

    @param n default = greater [n] s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param br default = [1]
    @param bc default = [1]
    @param b default = minimal fresh matrix consistent with [n], [br], and [bc]
    @param ofsx default = 1
    @param incx default = 1
*)

val dim1 : mat -> int
(** [dim1 m] @return the first dimension of matrix [m] (number of rows). *)

val dim2 : mat -> int
(** [dim2 m] @return the second dimension of matrix [m] (number of columns). *)

val has_zero_dim : mat -> bool
(** [has_zero_dim mat] checks whether matrix [mat] has a dimension of size
    [zero].  In this case it cannot contain data. *)

val col : mat -> int -> vec
(** [col m n] @return the [n]th column of matrix [m] as a vector.
    The data is shared. *)

val copy_row : ?vec : vec -> mat -> int -> vec
(** [copy_row ?vec mat int] @return a copy of the [n]th row of matrix [m]
    in vector [vec].

    @param vec default = fresh vector of length [dim2 mat]
*)


(** {6 Matrix transformations} *)

val swap :
  ?patt : patt -> ?m : int -> ?n : int ->
  ?ar : int -> ?ac : int -> mat ->
  ?br : int -> ?bc : int -> mat ->
  unit
  (** [swap ?patt ?m ?n ?ar ?ac a ?br ?bc b]
      swaps the contents of (sub-matrices) [a] and [b].

      @param patt default = [`Full]
      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param ar default = [1]
      @param ac default = [1]
      @param br default = [1]
      @param bc default = [1]
  *)

val transpose_copy :
  ?m : int -> ?n : int -> ?br : int ->
  ?bc : int -> ?b : mat ->
  ?ar : int -> ?ac : int -> mat ->
  mat
(** [transpose_copy ?m ?n ?br ?bc ?b ?ar ?ac a] @return the transpose of
    (sub-)matrix [a].  If [b] is given, the result will be stored in there
    using offsets [br] and [bc], otherwise a fresh matrix will be used.
    NOTE: this operations does _not_ support in-place transposes!

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = [1]
    @param bc default = [1]
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = [1]
    @param ac default = [1]
*)

val detri : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> unit
(** [detri ?up ?n ?ar ?ac a] takes a triangular (sub-)matrix [a], i.e. one
    where only the upper (iff [up] is true) or lower triangle is defined,
    and makes it a symmetric matrix by mirroring the defined triangle
    along the diagonal.

    @param up default = [true]
    @param n default = [Mat.dim2 a]
    @param ar default = [1]
    @param ac default = [1]
*)

val packed : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> vec
(** [packed ?up ?n ?ar ?ac a] @return (sub-)matrix [a] in packed
    storage format.

    @param up default = [true]
    @param n default = [Mat.dim2 a]
    @param ar default = [1]
    @param ac default = [1]
*)

val unpacked : ?up : bool -> ?n : int -> vec -> mat
(** [unpacked ?up x] @return an upper or lower (depending on [up])
    triangular matrix from packed representation [vec].  The other
    triangle of the matrix will be filled with zeros.

    @param up default = [true]
    @param n default = [Vec.dim x]
*)


(** {6 Operations on one matrix} *)

val fill :
  ?patt : patt -> ?m : int -> ?n : int ->
  ?ar : int -> ?ac : int -> mat -> num_type -> unit
(** [fill ?patt ?m ?n ?ar ?ac a x] fills the specified sub-matrix in [a]
    with value [x]. *)

val sum :
  ?patt : patt -> ?m : int -> ?n : int ->
  ?ar : int -> ?ac : int -> mat -> num_type
(** [sum ?patt ?m ?n ?ar ?ac a] computes the sum of all elements in
    the [m]-by-[n] submatrix using pattern [patt], starting at row [ar] and
    column [ac].  *)

val add_const : num_type -> unop
(** [add_const c ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] adds constant [c] to the
    designated [m] by [n] submatrix in [a] using pattern [patt] and stores the
    result in the designated submatrix in [b].

    @param patt default = [`Full]
    @param m default = [Mat.dim1 a]
    @param n default = [Mat.dim2 a]
    @param ar default = [1]
    @param ac default = [1]
    @param br default = [1]
    @param bc default = [1]
    @param b default = fresh matrix of size [m] by [n]
*)

val neg : unop
(** [neg ?m ?n ?br ?bc ?b ?ar ?ac a] computes the negative of the elements in
    the [m] by [n] (sub-)matrix of the matrix [a] starting in row [ar]
    and column [ac].  If [b] is given, the result will be stored in there
    using offsets [br] and [bc], otherwise a fresh matrix will be used.
    The resulting matrix is returned.

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val reci : unop
(** [reci ?m ?n ?br ?bc ?b ?ar ?ac a] computes the reciprocal of the elements in
    the [m] by [n] (sub-)matrix of the matrix [a] starting in row [ar]
    and column [ac].  If [b] is given, the result will be stored in there
    using offsets [br] and [bc], otherwise a fresh matrix will be used.
    The resulting matrix is returned.

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val copy_diag :
  ?n : int ->
  ?ofsy : int -> ?incy : int -> ?y : vec ->
  ?ar : int -> ?ac : int -> mat ->
  vec
(** [copy_diag ?n ?ofsy ?incy ?y ?ar ?ac a] @return the diagonal of the
    (sub-)matrix [a] in a (sub-)vector.

    @param n default = greatest [n] that does not exceed matrix dimensions
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector of length [n]
    @param ar default = 1
    @param ac default = 1
*)

val trace : mat -> num_type
(** [trace m] @return the trace of matrix [m].  If [m] is not a
    square matrix, the sum of the longest possible sequence of
    diagonal elements will be returned. *)

val scal :
  ?patt : patt -> ?m : int -> ?n : int ->
  num_type -> ?ar : int -> ?ac : int -> mat -> unit
(** [scal ?patt ?m ?n alpha ?ar ?ac a] BLAS [scal] function for
    (sub-)matrices. *)

val scal_cols :
  ?patt : patt -> ?m : int -> ?n : int ->
  ?ar : int -> ?ac : int -> mat ->
  ?ofs : int -> vec ->
  unit
(** [scal_cols ?patt ?m ?n ?ar ?ac a ?ofs alphas] column-wise [scal]
    function for matrices. *)

val scal_rows :
  ?patt : patt -> ?m : int -> ?n : int ->
  ?ofs : int -> vec ->
  ?ar : int -> ?ac : int -> mat ->
  unit
(** [scal_rows ?patt ?m ?n ?ofs alphas ?ar ?ac a] row-wise [scal]
    function for matrices. *)

val syrk_trace :
  ?n : int ->
  ?k : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  num_type
(** [syrk_trace ?n ?k ?ar ?ac a] computes the trace of either [a' * a]
    or [a * a'], whichever is more efficient (results are identical), of the
    (sub-)matrix [a] multiplied by its own transpose.  This is the same as
    the square of the Frobenius norm of a matrix.  [n] is the number of rows
    to consider in [a], and [k] the number of columns to consider.

    @param n default = number of rows of [a]
    @param k default = number of columns of [a]
    @param ar default = [1]
    @param ac default = [1]
*)

val syrk_diag :
  ?n : int ->
  ?k : int ->
  ?beta : num_type ->
  ?ofsy : int ->
  ?y : vec ->
  ?trans : trans2 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  vec
(** [syrk_diag ?n ?k ?beta ?ofsy ?y ?trans ?alpha ?ar ?ac a]
    computes the diagonal of the symmetric rank-k product of the
    (sub-)matrix [a], multiplying it with [alpha] and adding [beta]
    times [y], storing the result in [y] starting at the specified
    offset.  [n] elements of the diagonal will be computed, and [k]
    elements of the matrix will be part of the dot product associated
    with each diagonal element.

    @param n default = number of rows of [a] (or tr[a])
    @param k default = number of columns of [a] (or tr[a])
    @param beta default = [0]
    @param ofsy default = [1]
    @param y default = fresh vector of size [n + ofsy - 1]
    @param trans default = [`N]
    @param alpha default = [1]
    @param ar default = [1]
    @param ac default = [1]
*)


(** {6 Operations on two matrices} *)

val add : binop
(** [add ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the sum of the [m]
    by [n] sub-matrix of the matrix [a] starting in row [ar] and column [ac]
    with the corresponding sub-matrix of the matrix [b] starting in row
    [br] and column [bc].  If [c] is given, the result will be stored in
    there starting in row [cr] and column [cc], otherwise a fresh matrix
    will be used.  The resulting matrix is returned.

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param cr default = 1
    @param cc default = 1
    @param c default = fresh matrix with [cr + m - 1] rows and
                       [cc + n - 1] columns
    @param br default = 1
    @param bc default = 1
    @param ar default = 1
    @param ac default = 1
*)

val sub : binop
(** [sub ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the difference of the
    [m] by [n] sub-matrix of the matrix [a] starting in row [ar] and column
    [ac] with the corresponding sub-matrix of the matrix [b] starting in row
    [br] and column [bc].  If [c] is given, the result will be stored in
    there starting in row [cr] and column [cc], otherwise a fresh matrix
    will be used.  The resulting matrix is returned.

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param cr default = 1
    @param cc default = 1
    @param c default = fresh matrix with [cr + m - 1] rows and
                       [cc + n - 1] columns
    @param br default = 1
    @param bc default = 1
    @param ar default = 1
    @param ac default = 1
*)

val mul : binop
(** [mul ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the element-wise
    product of the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] with the corresponding sub-matrix of the matrix
    [b] starting in row [br] and column [bc].  If [c] is given, the result
    will be stored in there starting in row [cr] and column [cc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    NOTE: please do not confuse this function with matrix multiplication!
    The LAPACK-function for matrix multiplication is called [gemm],
    e.g. [Lacaml.D.gemm].

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param cr default = 1
    @param cc default = 1
    @param c default = fresh matrix with [cr + m - 1] rows and
                       [cc + n - 1] columns
    @param br default = 1
    @param bc default = 1
    @param ar default = 1
    @param ac default = 1
*)

val div : binop
(** [div ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the division of the
    [m] by [n] sub-matrix of the matrix [a] starting in row [ar] and column
    [ac] with the corresponding sub-matrix of the matrix [b] starting in row
    [br] and column [bc].  If [c] is given, the result will be stored in
    there starting in row [cr] and column [cc], otherwise a fresh matrix
    will be used.  The resulting matrix is returned.

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param cr default = 1
    @param cc default = 1
    @param c default = fresh matrix with [cr + m - 1] rows and
                       [cc + n - 1] columns
    @param br default = 1
    @param bc default = 1
    @param ar default = 1
    @param ac default = 1
*)

val axpy :
  ?alpha : num_type ->
  ?patt : patt ->
  ?m : int ->
  ?n : int ->
  ?xr : int ->
  ?xc : int ->
  mat ->
  ?yr : int ->
  ?yc : int ->
  mat
  -> unit
(** [axpy ?alpha ?patt ?m ?n ?xr ?xc x ?yr ?yc y] BLAS [axpy] function for
    matrices.

    @param alpha default = [{ re = 1.; im = 0. }]
    @param patt default = [`Full]
    @param m default = greater n s.t. [xr + m - 1 <= dim1 x]
    @param n default = greater n s.t. [xc + n - 1 <= dim2 x]
    @param xr default = 1
    @param xc default = 1
    @param yr default = 1
    @param yc default = 1
*)

val gemm_diag :
  ?n : int ->
  ?k : int ->
  ?beta : num_type ->
  ?ofsy : int ->
  ?y : vec ->
  ?transa : trans3 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?transb : trans3 ->
  ?br : int ->
  ?bc : int ->
  mat ->
  vec
(** [gemm_diag ?n ?k ?beta ?ofsy ?y ?transa ?transb ?alpha ?ar ?ac a ?br ?bc b]
    computes the diagonal of the product of the (sub-)matrices [a]
    and [b] (taking into account potential transposing), multiplying
    it with [alpha] and adding [beta] times [y], storing the result in
    [y] starting at the specified offset.  [n] elements of the diagonal
    will be computed, and [k] elements of the matrices will be part of
    the dot product associated with each diagonal element.

    @param n default = number of rows of [a] (or tr [a]) and
                       number of columns of [b] (or tr [b])
    @param k default = number of columns of [a] (or tr [a]) and
                       number of rows of [b] (or tr [b])
    @param beta default = [0]
    @param ofsy default = [1]
    @param y default = fresh vector of size [n + ofsy - 1]
    @param transa default = [`N]
    @param alpha default = [1]
    @param ar default = [1]
    @param ac default = [1]
    @param transb default = [`N]
    @param br default = [1]
    @param bc default = [1]
*)

val gemm_trace :
  ?n : int ->
  ?k : int ->
  ?transa : trans3 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?transb : trans3 ->
  ?br : int ->
  ?bc : int ->
  mat ->
  num_type
(** [gemm_trace ?n ?k ?transa ?ar ?ac a ?transb ?br ?bc b] computes
    the trace of the product of the (sub-)matrices [a] and [b] (taking
    into account potential transposing).  When transposing [a], this
    yields the so-called Frobenius product of [a] and [b].  [n] is the
    number of rows (columns) to consider in [a] and the number of columns
    (rows) in [b].  [k] is the inner dimension to use for the product.

    @param n default = number of rows of [a] (or tr [a]) and
                       number of columns of [b] (or tr [b])
    @param k default = number of columns of [a] (or tr [a]) and
                       number of rows of [b] (or tr [b])
    @param transa default = [`N]
    @param ar default = [1]
    @param ac default = [1]
    @param transb default = [`N]
    @param br default = [1]
    @param bc default = [1]
*)

val symm2_trace :
  ?n : int ->
  ?upa : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?upb : bool ->
  ?br : int ->
  ?bc : int ->
  mat ->
  num_type
(** [symm2_trace ?n ?upa ?ar ?ac a ?upb ?br ?bc b] computes the
    trace of the product of the symmetric (sub-)matrices [a] and
    [b].  [n] is the number of rows and columns to consider in [a]
    and [b].

    @param n default = dimensions of [a] and [b]
    @param upa default = true (upper triangular portion of [a] is accessed)
    @param ar default = [1]
    @param ac default = [1]
    @param upb default = true (upper triangular portion of [b] is accessed)
    @param br default = [1]
    @param bc default = [1]
*)

val ssqr_diff :
  ?patt : patt ->
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat ->
  num_type
(** [ssqr_diff ?patt ?m ?n ?ar ?ac a ?br ?bc b] @return the sum of squared
    differences between the [m] by [n] sub-matrix of the matrix
    [a] starting in row [ar] and column [ac] with the corresponding
    sub-matrix of the matrix [b] starting in row [br] and column [bc].

    @param patt default = [`Full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)


(** {6 Iterators over matrices} *)

val map :
  (num_type -> num_type) ->
  ?m : int ->
  ?n : int ->
  ?br : int ->
  ?bc : int ->
  ?b : mat ->
  ?ar : int ->
  ?ac : int ->
  mat
  -> mat
(** [map f ?m ?n ?br ?bc ?b ?ar ?ac a]
    @return matrix with [f] applied to each element of [a].
    @param m default = number of rows of [a]
    @param n default = number of columns of [a]
    @param b default = fresh matrix of size m by n *)

val fold_cols : ('a -> vec -> 'a) -> ?n : int -> ?ac : int -> 'a -> mat -> 'a
(** [fold_cols f ?n ?ac acc a]
    @return accumulator resulting from folding over each column vector.
    @param ac default = 1
    @param n default = number of columns of [a] *)
