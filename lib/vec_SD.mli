(* File: vec_SD.mli

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

(** {5 Vector operations} *)

open Lacaml_floatxx
open Types.Vec

(** {6 Creation of vectors} *)

val random :
  ?rnd_state : Random.State.t ->
  ?from : float -> ?range : float ->
  int
  -> vec
(** [random ?rnd_state ?from ?range n] @return a vector
    of size [n] initialized with random elements sampled uniformly from
    [range] starting at [from].  A random state [rnd_state] can be passed.

    @param rnd_state default = Random.get_state ()
    @param from default = -1.0
    @param range default = 2.0
*)

(** {6 Unary vector operations} *)

val abs : unop
(** [abs ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the absolute value
    of [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there
    using increments of [incy], otherwise a fresh vector will be
    used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val signum : unop
(** [signum ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the sign value ([-1] for
    negative numbers, [0] (or [-0]) for zero, [1] for positive numbers,
    [nan] for [nan]) of [n] elements of the vector [x] using [incx] as
    incremental steps.  If [y] is given, the result will be stored in there
    using increments of [incy], otherwise a fresh vector will be used.
    The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val sqr : unop
(** [sqr ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the square
    of [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there
    using increments of [incy], otherwise a fresh vector will be
    used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val sqrt : unop
(** [sqrt ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the square root
    of [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there
    using increments of [incy], otherwise a fresh vector will be
    used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val exp : unop
(** [exp ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the exponential
    of [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there
    using increments of [incy], otherwise a fresh vector will be
    used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val exp2 : unop
(** [exp ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the base-2 exponential
    of [n] elements of the vector [x] using [incx] as incremental steps.
    If [y] is given, the result will be stored in there using increments of
    [incy], otherwise a fresh vector will be used.  The resulting vector
    is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val expm1 : unop
(** [expm1 ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes [exp x -. 1.]
    for [n] elements of the vector [x] using [incx] as incremental steps.
    If [y] is given, the result will be stored in there using increments of
    [incy], otherwise a fresh vector will be used.  The resulting vector
    is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val log : unop
(** [log ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the logarithm
    of [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there
    using increments of [incy], otherwise a fresh vector will be
    used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val log10 : unop
(** [log10 ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the base-10 logarithm
    of [n] elements of the vector [x] using [incx] as incremental steps.
    If [y] is given, the result will be stored in there using increments of
    [incy], otherwise a fresh vector will be used.  The resulting vector
    is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val log1p : unop
(** [log1p ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes [log (1 + x)] for [n]
    elements of the vector [x] using [incx] as incremental steps.  If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val sin : unop
(** [sin ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the sine of [n] elements
    of the vector [x] using [incx] as incremental steps.   If [y] is given,
    the result will be stored in there using increments of [incy], otherwise
    a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val cos : unop
(** [cos ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the cosine of [n] elements
    of the vector [x] using [incx] as incremental steps.   If [y] is given,
    the result will be stored in there using increments of [incy], otherwise
    a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val tan : unop
(** [tan ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the tangent of [n] elements
    of the vector [x] using [incx] as incremental steps.   If [y] is given,
    the result will be stored in there using increments of [incy], otherwise
    a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val asin : unop
(** [asin ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the arc sine of [n] elements
    of the vector [x] using [incx] as incremental steps.   If [y] is given,
    the result will be stored in there using increments of [incy], otherwise
    a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val acos : unop
(** [acos ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the arc cosine of [n]
    elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val atan : unop
(** [atan ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the arc tangent of
    [n] elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val sinh : unop
(** [sinh ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the hyperbolic sine of
    [n] elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val cosh : unop
(** [cosh ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the hyperbolic cosine of
    [n] elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val tanh : unop
(** [tanh ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the hyperbolic tangent of
    [n] elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val asinh : unop
(** [asinh ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the hyperbolic arc sine of
    [n] elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val acosh : unop
(** [cosh ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the hyperbolic arc cosine of
    [n] elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val atanh : unop
(** [atanh ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the hyperbolic arc
    tangent of [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there using
    increments of [incy], otherwise a fresh vector will be used.  The resulting
    vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val floor : unop
(** [floor ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the floor of [n]
    elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val ceil : unop
(** [ceil ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the ceiling of [n]
    elements of the vector [x] using [incx] as incremental steps.   If [y]
    is given, the result will be stored in there using increments of [incy],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val erf : unop
(** [erf ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the error function for
    [n] elements of the vector [x] using [incx] as incremental steps.
    If [y] is given, the result will be stored in there using increments of
    [incy], otherwise a fresh vector will be used.  The resulting vector
    is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val erfc : unop
(** [erfc ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the complementary error
    function for [n] elements of the vector [x] using [incx] as incremental
    steps.   If [y] is given, the result will be stored in there using
    increments of [incy], otherwise a fresh vector will be used.  The resulting
    vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val logistic : unop
(** [logistic ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the logistict
    function [1/(1 + exp(-a)] for [n] elements of the vector [x] using [incx]
    as incremental steps.   If [y] is given, the result will be stored in
    there using increments of [incy], otherwise a fresh vector will be used.
    The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val relu : unop
(** [relu ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the rectified linear
    unit function [max(x, 0)] for [n] elements of the vector [x] using [incx]
    as incremental steps.   If [y] is given, the result will be stored in
    there using increments of [incy], otherwise a fresh vector will be used.
    The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val softplus : unop
(** [softplus ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the softplus function
    [log(1 + exp(x)] for [n] elements of the vector [x] using [incx]
    as incremental steps.   If [y] is given, the result will be stored in
    there using increments of [incy], otherwise a fresh vector will be used.
    The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)

val softsign : unop
(** [softsign ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the softsign function
    [x / (1 + abs(x))] for [n] elements of the vector [x] using [incx]
    as incremental steps.   If [y] is given, the result will be stored in
    there using increments of [incy], otherwise a fresh vector will be used.
    The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1
*)


(** {6 Binary vector operations} *)

val pow : binop
(** [pow ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] computes [pow(a, b)]
    of [n] elements of vectors [x] and [y] elementwise, using [incx] and
    [incy] as incremental steps respectively. If [z] is given, the result
    will be stored in there using increments of [incz], otherwise a fresh
    vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)

val atan2 : binop
(** [atan2 ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] computes
    [atan2(a, b)] of [n] elements of vectors [x] and [y] elementwise, using
    [incx] and [incy] as incremental steps respectively. If [z] is given,
    the result will be stored in there using increments of [incz], otherwise
    a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)

val hypot : binop
(** [hypot ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] computes
    [sqrt(x*x + y*y)] of [n] elements of vectors [x] and [y] elementwise,
    using [incx] and [incy] as incremental steps respectively. If [z] is
    given, the result will be stored in there using increments of [incz],
    otherwise a fresh vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)

val min2 : binop
(** [min2 ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] computes the
    minimum of [n] elements of vectors [x] and [y] elementwise, using [incx]
    and [incy] as incremental steps respectively. If [z] is given, the result
    will be stored in there using increments of [incz], otherwise a fresh
    vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)

val max2 : binop
(** [max2 ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] computes the
    maximum of [n] elements of vectors [x] and [y] elementwise, using [incx]
    and [incy] as incremental steps respectively. If [z] is given, the result
    will be stored in there using increments of [incz], otherwise a fresh
    vector will be used.  The resulting vector is returned.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)


(** {6 Miscellaneous functions} *)

val log_sum_exp : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
(** [log_sum_exp ?n ?ofsx ?incx x] computes the logarithm of the sum of
    exponentials of the [n] elements in vector [x], separated by [incx]
    incremental steps.

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
*)
