(* File: lacaml_common.ml

   Copyright (C) 2003-2005

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

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

(* $Id: lacaml_common.ml,v 1.8 2006/01/18 15:03:40 mottl Exp $ *)

open Bigarray

exception InternalError of string

type int_vec = (int32, int32_elt, fortran_layout) Array1.t

let create_int_vec n = Array1.create int32 fortran_layout n

let mat_of_vec v =
  array2_of_genarray (reshape (genarray_of_array1 v) [| Array1.dim v; 1 |])

type norm2 = [ `O | `I ]
type norm4 = [ `M | `O | `I | `F ]

type svd_job = [ `A | `S | `O | `N ]
