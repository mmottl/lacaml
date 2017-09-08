(* File: mat_SD.mli

   Copyright (C) 2001-

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

(** {5 Matrix operations} *)

open Lacaml__floatxx
open Types.Mat

(** {6 Creation of matrices} *)

val hilbert : int -> mat
(** [hilbert n] @return an [n]x[n] Hilbert matrix. *)

val hankel : int -> mat
(** [hankel n] @return an [n]x[n] Hankel matrix. *)

val pascal : int -> mat
(** [pascal n] @return an [n]x[n] Pascal matrix. *)

val rosser : unit -> mat
(** [rosser n] @return 8x8 Rosser matrix. *)

val toeplitz : vec -> mat
(** [toeplitz v] @return the Toeplitz matrix associated with [v].
    The constant diagonals are read from left to right from [v].
    @raise Invalid_argument if the length of [v] is not an odd number. *)

val vandermonde : vec -> mat
(** [vandermonde v] @return the Vandermonde matrix associated with [v]. *)

val wilkinson : int -> mat
(** [wilkinson n] @return the [n]x[n] Wilkinson matrix.
    @raise Invalid_argument if [n] is not an odd number >= 3. *)

val random :
  ?rnd_state : Random.State.t ->
  ?from : float -> ?range : float ->
  int -> int
  -> mat
(** [random ?rnd_state ?from ?range m n] @return an [m]x[n] matrix
    initialized with random elements sampled uniformly from [range]
    starting at [from].  A random state [rnd_state] can be passed.

    @param rnd_state default = Random.get_state ()
    @param from default = -1.0
    @param range default = 2.0 *)


(** {6 Unary matrix operations} *)

val abs : unop
(** [abs ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the absolute value of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting in
    row [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val signum : unop
(** [signum ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the sign value ([-1] for
    negative numbers, [0] (or [-0]) for zero, [1] for positive numbers,
    [nan] for [nan]) of the elements in the [m] by [n] sub-matrix of the
    matrix [a] starting in row [ar] and column [ac] and pattern [patt].  If [b]
    is given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val sqr : unop
(** [sqr ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the square of the elements
    in the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
    and column [ac] and pattern [patt].  If [b] is given, the result will
    be stored in there using offsets [br] and [bc], otherwise a fresh matrix
    will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val sqrt : unop
(** [sqrt ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the square root of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val cbrt : unop
(** [cbrt ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the cubic root of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val exp : unop
(** [exp ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the exponential of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val exp2 : unop
(** [exp2 ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the base-2 exponential of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val expm1 : unop
(** [expm1 ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes [exp a -. 1.] of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val log : unop
(** [log ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the logarithm of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val log10 : unop
(** [log10 ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the base-10 logarithm of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val log2 : unop
(** [log2 ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes base-2 logarithm of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val log1p : unop
(** [log1p ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes [log (1 + a)] of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val sin : unop
(** [sin ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the sine of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar] and column
    [ac] and pattern [patt].  If [b] is given, the result will be stored in
    there using offsets [br] and [bc], otherwise a fresh matrix will be used.
    The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val cos : unop
(** [cos ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the cosine of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val tan : unop
(** [tan ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the tangent of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val asin : unop
(** [asin ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the arc sine of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val acos : unop
(** [acos ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the arc cosine of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val atan : unop
(** [atan ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the arc tangent of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val sinh : unop
(** [sinh ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic sine of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val cosh : unop
(** [cosh ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic cosine of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val tanh : unop
(** [tanh ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic tangent of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val asinh : unop
(** [asinh ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic arc
    sine of the elements in the [m] by [n] sub-matrix of the matrix [a]
    starting in row [ar] and column [ac] and pattern [patt].  If [b] is
    given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val acosh : unop
(** [acosh ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic arc
    cosine of the elements in the [m] by [n] sub-matrix of the matrix [a]
    starting in row [ar] and column [ac] and pattern [patt].  If [b] is
    given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val atanh : unop
(** [atanh ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic arc
    tangent of the elements in the [m] by [n] sub-matrix of the matrix [a]
    starting in row [ar] and column [ac] and pattern [patt].  If [b] is
    given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val floor : unop
(** [floor ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the floor of the elements
    in the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
    and column [ac] and pattern [patt].  If [b] is given, the result will
    be stored in there using offsets [br] and [bc], otherwise a fresh matrix
    will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val ceil : unop
(** [ceil ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the ceiling of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt].  If [b] is given, the result
    will be stored in there using offsets [br] and [bc], otherwise a fresh
    matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val round : unop
(** [round ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] rounds the elements in the [m]
    by [n] sub-matrix of the matrix [a] starting in row [ar] and column
    [ac] and pattern [patt].  If [b] is given, the result will be stored in
    there using offsets [br] and [bc], otherwise a fresh matrix will be used.
    The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val trunc : unop
(** [trunc ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the truncation of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val erf : unop
(** [erf ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the error function of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt].  If [b] is given, the
    result will be stored in there using offsets [br] and [bc], otherwise
    a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val erfc : unop
(** [erfc ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the complementary error
    function of the elements in the [m] by [n] sub-matrix of the matrix
    [a] starting in row [ar] and column [ac] and pattern [patt].  If [b] is
    given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val logistic : unop
(** [logistic ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the logistic function
    [1/(1 + exp(-a)] of the elements in the [m] by [n] sub-matrix of the matrix
    [a] starting in row [ar] and column [ac] and pattern [patt].  If [b] is
    given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val relu : unop
(** [relu ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the rectified linear unit
    function [max(a, 0)] of the elements in the [m] by [n] sub-matrix of the
    matrix [a] starting in row [ar] and column [ac] and pattern [patt].  If [b]
    is given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val softplus : unop
(** [softplus ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the softplus function
    [log(1 + exp(x)] of the elements in the [m] by [n] sub-matrix of the matrix
    [a] starting in row [ar] and column [ac] and pattern [patt].  If [b] is
    given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)

val softsign : unop
(** [softsign ?patt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the softsign function
    [x / (1 + abs(x))] of the elements in the [m] by [n] sub-matrix of the
    matrix [a] starting in row [ar] and column [ac] and pattern [patt].  If [b]
    is given, the result will be stored in there using offsets [br] and [bc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param br default = 1
    @param bc default = 1
    @param b default = fresh matrix with [br + m - 1] rows and
                       [bc + n - 1] columns
    @param ar default = 1
    @param ac default = 1
*)


(** {6 Binary matrix operations} *)

val pow : binop
(** [pow ?patt ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes [pow(a, b)] for
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar] and
    column [ac] and pattern [patt] with the corresponding sub-matrix of
    the matrix [b] starting in row [br] and column [bc].  If [c] is given,
    the result will be stored in there starting in row [cr] and column [cc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
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

val atan2 : binop
(** [atan2 ?patt ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes
    [atan2(a, b)] for the [m] by [n] sub-matrix of the matrix [a] starting
    in row [ar] and column [ac] and pattern [patt] with the corresponding
    sub-matrix of the matrix [b] starting in row [br] and column [bc].
    If [c] is given, the result will be stored in there starting in row [cr]
    and column [cc], otherwise a fresh matrix will be used.  The resulting
    matrix is returned.

    NOTE: WARNING!  From a geometric point of view, the [atan2] function takes
    the y-coordinate in [a] and the x-coordinate in [b].  This confusion is
    a sad consequence of the C99-standard reversing the argument order for
    [atan2] for no good reason.

    @param patt default = [`full]
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

val hypot : binop
(** [hypot ?patt ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes [sqrt(a*a+b*b)]
    for the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
    and column [ac] and pattern [patt] with the corresponding sub-matrix of
    the matrix [b] starting in row [br] and column [bc].  If [c] is given,
    the result will be stored in there starting in row [cr] and column [cc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
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

val min2 : binop
(** [min2 ?patt ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the elementwise
    minimum of the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt] with the corresponding sub-matrix
    of the matrix [b] starting in row [br] and column [bc].  If [c] is given,
    the result will be stored in there starting in row [cr] and column [cc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
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

val max2 : binop
(** [max2 ?patt ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the elementwise
    maximum of the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] and pattern [patt] with the corresponding sub-matrix
    of the matrix [b] starting in row [br] and column [bc].  If [c] is given,
    the result will be stored in there starting in row [cr] and column [cc],
    otherwise a fresh matrix will be used.  The resulting matrix is returned.

    @param patt default = [`full]
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

val sum_prod :
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
(** [sum_prod ?patt ?m ?n ?ar ?ac a ?br ?bc b] @return the sum of elementwise
    products between the [m] by [n] sub-matrix of the matrix [a] starting in row
    [ar] and column [ac] with the corresponding sub-matrix of the matrix [b]
    starting in row [br] and column [bc].

    @param patt default = [`full]
    @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
    @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)



(** {6 Miscellaneous functions} *)

val log_sum_exp :
  ?patt : patt -> ?m : int -> ?n : int ->
  ?ar : int -> ?ac : int -> mat -> num_type
(** [log_sum_exp ?patt ?m ?n ?ar ?ac a] computes the logarithm of the sum of
    exponentials of all elements in the [m]-by-[n] submatrix using pattern
    [patt], starting at row [ar] and column [ac].

    @param patt default = [`full]
    @param m default = number of rows of [a]
    @param n default = number of columns of [a]
*)


(** {6 Ternary matrix operations} *)

val cpab :
  ?patt : patt ->
  ?m : int ->
  ?n : int ->
  ?cr : int ->
  ?cc : int ->
  mat ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [cpab ?patt ?m ?n ?cr ?cc c ?ar ?ac a ?br ?bc b] multiplies designated
    [m]-by-[n] range of elements of matrices [a] and [b] using pattern [patt]
    elementwise and adds the result to and stores it in the specified range
    in [c].  This function is useful for convolutions.  Similar to [Vec.zpxy].

    @param patt default = [`full]
    @param m default = number of rows of [a]
    @param n default = number of columns of [a]
    @param cr default = 1
    @param cc default = 1
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)

val cmab :
  ?patt : patt ->
  ?m : int ->
  ?n : int ->
  ?cr : int ->
  ?cc : int ->
  mat ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat
  -> unit
(** [cmab ?patt ?m ?n ?cr ?cc c ?ar ?ac a ?br ?bc b] multiplies designated
    [m]-by-[n] range of elements of matrices [a] and [b] elementwise using
    pattern [patt] and subtracts the result from and stores it in the
    specified range in [c].  This function is useful for convolutions.
    Similar to [Vec.zmxy].

    @param patt default = [`full]
    @param m default = number of rows of [a]
    @param n default = number of columns of [a]
    @param cr default = 1
    @param cc default = 1
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)
