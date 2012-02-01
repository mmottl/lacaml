(* File: SD.ml

   Copyright (C) 2010-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/an/

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

(** Modules with functions specialized for simple (S) or double (D)
    precision numbers. *)

include Floatxx

include Impl2_FPREC
include Impl4_FPREC

include Real_io

module Vec = struct
  include Vec2_FPREC
  include Vec4_FPREC
end

module Mat = struct
  include Mat2_FPREC
  include Mat4_FPREC
end
