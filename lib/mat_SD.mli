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
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** {5 Matrix operations} *)

open Lacaml_floatxx
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

val sqr : unop
(** [sqr ?m ?n ?br ?bc ?b ?ar ?ac a] computes the square of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
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

val sqrt : unop
(** [sqrt ?m ?n ?br ?bc ?b ?ar ?ac a] computes the square root of the
    elements in the [m] by [n] sub-matrix of the matrix [a] starting in
    row [ar] and column [ac].  If [b] is given, the result will be stored in
    there using offsets [br] and [bc], otherwise a fresh matrix will be used.
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

val exp : unop
(** [exp ?m ?n ?br ?bc ?b ?ar ?ac a] computes the exponential of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
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

val log : unop
(** [log ?m ?n ?br ?bc ?b ?ar ?ac a] computes the logarithm of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
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

val sin : unop
(** [sin ?m ?n ?br ?bc ?b ?ar ?ac a] computes the sine of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
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

val cos : unop
(** [cos ?m ?n ?br ?bc ?b ?ar ?ac a] computes the cosine of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
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

val tan : unop
(** [tan ?m ?n ?br ?bc ?b ?ar ?ac a] computes the tangent of the elements in
    the [m] by [n] sub-matrix of the matrix [a] starting in row [ar]
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

val tanh : unop
(** [tanh ?m ?n ?br ?bc ?b ?ar ?ac a] computes the hyperbolic tangent of
    the elements in the [m] by [n] sub-matrix of the matrix [a] starting in
    row [ar] and column [ac].  If [b] is given, the result will be stored in
    there using offsets [br] and [bc], otherwise a fresh matrix will be used.
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


(** {6 Ternary matrix operations} *)

val cpab :
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
(** [cpab ?m ?n ?cr ?cc c ?ar ?ac a ?br ?bc b] multiplies designated [m]-by-[n]
    range of elements of matrices [a] and [b] elementwise, and adds the
    result to and stores it in the specified range in [c].  This function
    is useful for convolutions.  Similar to [Vec.zpxy].

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
(** [cmab ?m ?n ?cr ?cc c ?ar ?ac a ?br ?bc b] multiplies designated [m]-by-[n]
    range of elements of matrices [a] and [b] elementwise, and subtracts the
    result from and stores it in the specified range in [c].  This function
    is useful for convolutions.  Similar to [Vec.zmxy].

    @param m default = number of rows of [a]
    @param n default = number of columns of [a]
    @param cr default = 1
    @param cc default = 1
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)
