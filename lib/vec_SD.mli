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

val expm1 : unop
(** [expm1 ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the exponential
    minus 1 of [n] elements of the vector [x] using [incx] as incremental
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

val log1p : unop
(** [log1p ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the logarithm
    plus 1 of [n] elements of the vector [x] using [incx] as incremental
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
