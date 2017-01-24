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
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** Modules with functions specialized for simple (C) or double (Z)
    precision complex numbers. *)

include Lacaml_complexxx

include Lacaml__complex_io

include Lacaml__impl2_CPREC
include Lacaml__impl4_CPREC

module Vec = struct
  type t = vec

  include Lacaml__vec2_CPREC
  include Lacaml__vec4_CPREC
end

module Mat = struct
  type t = mat

  include Lacaml__mat2_CPREC
  include Lacaml__mat4_CPREC
end
