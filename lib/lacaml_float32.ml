(* File: lacaml_float32.ml

   Copyright (C) 2005

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

(* $Id: lacaml_float32.ml,v 1.7 2005/07/11 18:45:11 mottl Exp $ *)

open Bigarray

type prec = float32_elt
type num_type = float
type vec = (float, prec, fortran_layout) Array1.t
type rvec = (float, float32_elt, fortran_layout) Array1.t
type mat = (float, prec, fortran_layout) Array2.t

type trans2 = [ `N | `T ]
type trans3 = [ `N | `T ]

let prec = float32
let zero = 0.0
let one = 1.0

let int_of_float32 = int_of_float
