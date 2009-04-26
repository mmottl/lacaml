(* File: vec_SDCZ.mli

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

open Bigarray
open Numberxx

(** {6 Creation/conversion of vectors and dimension accessor} *)

val create : int -> vec
(** [create n] @return a vector with [n] rows. *)

val make : int -> num_type -> vec
(** [make n x] @return a vector with [n] rows initialized with value [x]. *)

val make0 : int -> vec
(** [make0 n x] @return a vector with [n] rows initialized with the zero
    element. *)

val init : int -> (int -> num_type) -> vec
(** [init n f] @return a vector containing [n] elements, where each
    element at position [i] is initialized by the result of calling
    [f i]. *)

val of_array : num_type array -> vec
(** [of_array ar] @return a vector initialized from array [ar]. *)

val to_array : vec -> num_type array
(** [to_array v] @return an array initialized from vector [v]. *)

val of_list : num_type list -> vec
(** [of_list l] @return a vector initialized from list [l]. *)

val to_list : vec -> num_type list
(** [to_list v] @return a list initialized from vector [v]. *)

val append : vec -> vec -> vec
(** [append v1 v2] @return the vector resulting from appending vector
    [v2] to [v1]. *)

val concat : vec list -> vec
(** [concat vs] @return the concatenation of vectors [vs]. *)

val empty : vec
(** [empty] the empty vector. *)

val linspace : ?y : vec -> num_type -> num_type -> int -> vec
(** [linspace ?z a b n] @return the vector [y] overwritten with [n]
    linearly spaced points between and including [a] and [b].
    @param y default = fresh vector of dim [n] *)

val logspace : ?y : vec -> num_type -> num_type -> ?base : float -> int -> vec
(** [logspace ?z a b base n] @return the vector [y] overwritten with [n]
    points logarithmically spaced using base [b] between and including
    [base] ** [a] and [base] ** [b].
    @param y default = fresh vector of dim [n]
    @param base default = 10.0 *)

val dim : vec -> int
(** [dim x] @return the dimension of vector [x]. *)


(** {6 Iterators over vectors} *)

val iter :
  (num_type -> unit) ->
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> unit
(** [iter ?n ?ofsx ?incx f x] applies function [f] in turn to all elements
    of vector [x].
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val iteri :
  (int -> num_type -> unit) ->
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> unit
(** [iteri ?n ?ofsx ?incx f x] same as [iter] but additionally passes
    the index of the element as first argument and the element itself
    as second argument. *)

val fold :
  ('a -> num_type -> 'a) ->
  'a ->
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> 'a
(** [fold f a ?n ?ofsx ?incx x] is
    [f (... (f (f a x.{ofsx}) x.{ofsx + incx}) ...) x.{ofsx + (n-1)*incx}]
    if [incx > 0] and the same in the reverse order of appearance of the
    [x] values if [incx < 0].
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 Operations on one vector} *)

val rev : vec -> vec
(** [rev x] reverses vector [x] (non-destructive). *)

val max : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
(** [max ?n ?ofsx ?incx x] computes the greater of the [n] elements
    in vector [x] (2-norm), separated by [incx] incremental steps. NaNs
    are ignored. If only NaNs are encountered, the negative infinity
    element will be returned.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val min : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
(** [min ?n ?ofsx ?incx x] computes the smaller of the [n] elements
    in vector [x] (2-norm), separated by [incx] incremental steps.
    NaNs are ignored. If only NaNs are encountered, the infinity element
    will be returned.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val sum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
(** [sum ?n ?ofsx ?incx x] computes the sum of the [n] elements in
    vector [x], separated by [incx] incremental steps.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val prod : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
(** [prod ?n ?ofsx ?incx x] computes the product of the [n] elements
    in vector [x], separated by [incx] incremental steps.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val sqr_nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
(** [sqr_nrm2 ?n ?c ?ofsx ?incx x] computes the square of the 2-norm
    (Euclidean norm) of vector [x] separated by [incx] incremental steps.
    This is equivalent to squaring the result of calling the BLAS-function
    [nrm2].
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val ssqr :
  ?n : int ->
  ?c : num_type ->
  ?ofsx : int ->
  ?incx : int ->
  vec
  -> num_type
(** [ssqr ?n ?c ?ofsx ?incx x] computes the sum of squared differences of
    the [n] elements in vector [x] from constant [c], separated by
    [incx] incremental steps.  Please do not confuse with {sqr_nrm2}!
    The current function returns a different result for complex numbers
    and is less numerically stable for real numbers!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param c default = zero
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 Operations on two vectors} *)

val add :
  ?n : int ->
  ?ofsz : int ->
  ?incz : int ->
  ?z : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> vec
(** [add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] adds [n]
    elements of vectors [x] and [y] elementwise, using [incx] and [incy]
    as incremental steps respectively. If [z] is given, the result will
    be stored in there using increments of [incz], otherwise a fresh
    vector will be used. The resulting vector is returned.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val sub :
  ?n : int ->
  ?ofsz : int ->
  ?incz : int ->
  ?z : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> vec
(** [sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] subtracts [n]
    elements of vectors [x] and [y] elementwise, using [incx] and [incy]
    as incremental steps respectively. If [z] is given, the result will
    be stored in there using increments of [incz], otherwise a fresh
    vector will be used. The resulting vector is returned.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val mul :
  ?n : int ->
  ?ofsz : int ->
  ?incz : int ->
  ?z : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> vec
(** [mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] multiplies
    [n] elements of vectors [x] and [y] elementwise, using [incx]
    and [incy] as incremental steps respectively. If [z] is given, the
    result will be stored in there using increments of [incz], otherwise
    a fresh vector will be used. The resulting vector is returned.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val div :
  ?n : int ->
  ?ofsz : int ->
  ?incz : int ->
  ?z : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> vec
(** [div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] divides [n]
    elements of vectors [x] and [y] elementwise, using [incx] and [incy]
    as incremental steps respectively. If [z] is given, the result will
    be stored in there using increments of [incz], otherwise a fresh
    vector will be used. The resulting vector is returned.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsz default = 1
    @param incz default = 1
    @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val ssqr_diff :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> num_type
(** [ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y] returns the sum of
    squared differences of [n] elements of vectors [x] and [y], using
    [incx] and [incy] as incremental steps respectively.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)
