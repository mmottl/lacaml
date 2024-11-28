(* File: common.ml

   Copyright © 2001-

   Markus Mottl <markus.mottl@gmail.com>

   Liam Stewart <liam@cs.toronto.edu>

   Christophe Troestler <Christophe.Troestler@umons.ac.be>

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

open Bigarray

exception InternalError of string

type int_vec = (int, int_elt, fortran_layout) Array1.t

let create_int_vec n = Array1.create int fortran_layout n

type int32_vec = (int32, int32_elt, fortran_layout) Array1.t

let create_int32_vec n = Array1.create int32 fortran_layout n

let mat_from_vec v =
  array2_of_genarray (reshape (genarray_of_array1 v) [| Array1.dim v; 1 |])

type trans2 = [ `N | `T ]
type side = [ `L | `R ]
type diag = [ `U | `N ]
type norm2 = [ `O | `I ]
type norm4 = [ `M | `O | `I | `F ]
type svd_job = [ `A | `S | `O | `N ]
type schur_vectors = [ `No_Schur_vectors | `Compute_Schur_vectors ]

type eigen_value_sort =
  [ `No_sort
  | `Select_left_plane
  | `Select_right_plane
  | `Select_interior_disk
  | `Select_exterior_disk
  | `Select_custom of Complex.t -> bool ]

module Types = struct
  module Vec = struct
    type 'vec unop =
      ?n:int ->
      ?ofsy:int ->
      ?incy:int ->
      ?y:'vec ->
      ?ofsx:int ->
      ?incx:int ->
      'vec ->
      'vec

    type 'vec binop =
      ?n:int ->
      ?ofsz:int ->
      ?incz:int ->
      ?z:'vec ->
      ?ofsx:int ->
      ?incx:int ->
      'vec ->
      ?ofsy:int ->
      ?incy:int ->
      'vec ->
      'vec
  end
  (* Vec *)

  module Mat = struct
    type patt =
      [ `Full (* Full matrix *)
      | `Utr (* Upper triangular or trapezoidal matrix *)
      | `Ltr (* lower triangular or trapezoidal matrix *)
      | `Upent of int (* Initial full rows of pentagonal matrix *)
      | `Lpent of int (* Initial full columns of pentagonal matrix *) ]

    type 'mat unop =
      ?patt:patt ->
      ?m:int ->
      ?n:int ->
      ?br:int ->
      ?bc:int ->
      ?b:'mat ->
      ?ar:int ->
      ?ac:int ->
      'mat ->
      'mat

    type 'mat binop =
      ?patt:patt ->
      ?m:int ->
      ?n:int ->
      ?cr:int ->
      ?cc:int ->
      ?c:'mat ->
      ?ar:int ->
      ?ac:int ->
      'mat ->
      ?br:int ->
      ?bc:int ->
      'mat ->
      'mat
  end
  (* Mat *)
end
(* Types *)
