(* File: float64.ml

   Copyright (C) 2005

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

open Bigarray

type prec = float64_elt
type num_type = float
type vec = (float, float64_elt, fortran_layout) Array1.t
type rvec = vec
type mat = (float, float64_elt, fortran_layout) Array2.t

type trans3 = [ `N | `T ]

let prec = float64
let zero = 0.0
let one = 1.0
let add = (+.)

let vec_create n = Array1.create prec fortran_layout n

let int_of_float64 = int_of_float

module Types = struct
  module Vec = struct
    type unop = vec Lacaml__common.Types.Vec.unop
    type binop = vec Lacaml__common.Types.Vec.binop
  end  (* Vec *)

  module Mat = struct
    type unop = mat Lacaml__common.Types.Mat.unop
    type binop = mat Lacaml__common.Types.Mat.binop
  end  (* Mat *)
end  (* Types *)
