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

open Bigarray
open Floatxx

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
    @param range default = 2.0 *)

val sqr :
  ?n : int ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
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

val sqrt :
  ?n : int ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> vec
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

val sort :
  ?cmp : (float -> float -> int) ->
  ?decr : bool ->
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> unit
(** [sort ?lt ?n ?ofsx ?incx x] sorts the array [x] in increasing
    order according to the "less than" function [lt].

    @param cmp a function such that [cmp a b < 0] if [a] is less than
       [b], [cmp a b = 0] if [a] equal [b] and [cmp a b > 0] if [a] is
       greater than [b] for the desired order.  Default: the usual
       order on floating point values (a special routine makes it
       fast).  Whatever the order you choose, NaNs considered larger
       than any other value (so they will be last in the sorted
       vector).
    @param decr sort in decreasing order (stays fast for the default [cmp]).
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
 *)
;;
