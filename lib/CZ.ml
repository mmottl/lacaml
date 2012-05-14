(* File: CZ.ml

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

open Bigarray

(** Modules with functions specialized for simple (C) or double (Z)
    precision complex numbers. *)

include Complexxx

include Complex_io

include Lacaml_impl2_CPREC
include Lacaml_impl4_CPREC

module Vec = struct
  include Lacaml_vec2_CPREC
  include Lacaml_vec4_CPREC
end

module Mat = struct
  include Lacaml_mat2_CPREC
  include Lacaml_mat4_CPREC
end
