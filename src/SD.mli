(* File: SD.mli

   Copyright © 2001-

   Markus Mottl <markus.mottl@gmail.com>

   Christophe Troestler <Christophe.Troestler@umons.ac.be>

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

(** This module [Lacaml.FPREC] contains linear algebra routines for real numbers
    (precision: floatxx). It is recommended to use this module by writing
    {[
      open Lacaml.FPREC
    ]}
    at the top of your file. *)

open Bigarray

(** {3 PrecDocxx-precision Real Module} *)

type prec = floatxx_elt
type num_type = float

type vec = (float, floatxx_elt, fortran_layout) Array1.t
(** Vectors (precision: floatxx). *)

type rvec = vec

type mat = (float, floatxx_elt, fortran_layout) Array2.t
(** Matrices (precision: floatxx). *)

type trans3 = [ `N | `T ]
(** Transpose parameter (normal or transposed). For complex matrices, conjugate
    transpose is also offered, hence the name. *)

val prec : (float, floatxx_elt) Bigarray.kind
(** Precision for this submodule {!FPREC}. Allows to write precision independent
    code. *)

module Vec : sig
  type t = vec

  include module type of Vec2_FPREC
  include module type of Vec4_FPREC
end

module Mat : sig
  type t = mat

  include module type of Mat2_FPREC
  include module type of Mat4_FPREC
end

include module type of Real_io
include module type of Impl2_FPREC
include module type of Impl4_FPREC
