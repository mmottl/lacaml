(* File: common.mli

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

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

(** [Lacaml.common] contains definitions independent of the precision. *)

open Bigarray

type trans2 = [ `N | `T ]
(** Transpose parameter (normal or transposed) *)

type side = [ `L | `R ]
(** Side parameter (left or right) *)

type diag = [ `U | `N ]
(** Diagonal parameter (unit or non-unit) *)

type norm2 = [ `O | `I ]
(** Type of 1-norm ([`O]) and infinity norm ([`I]) *)

type norm4 = [ norm2 | `M | `F ]
(** Type of 1-norm ([`O]), infinity norm ([`I]) and the Frobenius norm ([`F]).
    [`M] is the maximum of the absolute values (not a true matrix norm). *)

type svd_job = [ `A | `S | `O | `N ]
(** SVD computation flags *)

type schur_vectors = [ `No_Schur_vectors | `Compute_Schur_vectors ]
(** GEES job option *)

type eigen_value_sort = [
  | `No_sort
  | `Select_left_plane
  | `Select_right_plane
  | `Select_interior_disk
  | `Select_exterior_disk
  | `Select_custom of Complex.t -> bool
]
(** GEES eigenvalue sort option *)

exception InternalError of string
(** [InternalError msg] gets raised when BLAS or LAPACK exhibit undefined
    behaviour. *)

type int_vec = (int, int_elt, fortran_layout) Array1.t
(** Type of OCaml integer vectors. *)

val create_int_vec : int -> int_vec
(** [create_int_vec n] @return an int-vector with [n] rows. *)

type int32_vec = (int32, int32_elt, fortran_layout) Array1.t
(** Type of 32bit Fortran integer vectors. *)

val create_int32_vec : int -> int32_vec
(** [create_int32_vec n] @return an int32-vector with [n] rows. *)

val mat_from_vec : ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Array2.t
(** [mat_from_vec a] converts the vector [a] into a matrix with [Array1.dim a]
    rows and 1 column.  The data is shared between the two matrices. *)

(** Common types used for vector and matrix operations *)

module Types : sig
  module Vec : sig
    type 'vec unop =
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : 'vec ->
      ?ofsx : int ->
      ?incx : int ->
      'vec
      -> 'vec

    type 'vec binop =
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : 'vec ->
      ?ofsx : int ->
      ?incx : int ->
      'vec ->
      ?ofsy : int ->
      ?incy : int ->
      'vec
      -> 'vec
  end  (* Vec *)

  module Mat : sig
    type patt = [
      | `full  (* full matrix *)
      | `utri  (* upper triangular matrix *)
      | `ltri  (* lower triangular matrix *)
      | `upent of int  (* initial full rows *)
      | `lpent of int  (* initial full columns *)
    ]

    type 'mat unop =
      ?patt : patt ->
      ?m : int ->
      ?n : int ->
      ?br : int ->
      ?bc : int ->
      ?b : 'mat ->
      ?ar : int ->
      ?ac : int ->
      'mat
      -> 'mat

    type 'mat binop =
      ?patt : patt ->
      ?m : int ->
      ?n : int ->
      ?cr : int ->
      ?cc : int ->
      ?c : 'mat ->
      ?ar : int ->
      ?ac : int ->
      'mat ->
      ?br : int ->
      ?bc : int ->
      'mat
      -> 'mat
  end  (* Mat *)
end  (* Types *)
