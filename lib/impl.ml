(* File: lacaml.ml

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

(** Modules with functions specialized for (S)ingle and (D)ouble
    precision, and for (C)omplex and double complex (Z) numbers. *)

module S = struct
  include Float32

  include Impl2_S
  include Impl4_S

  module Vec = struct
    include Vec2_S
    include Vec4_S
  end

  module Mat = struct
    include Mat2_S
    include Mat4_S
  end
end

module D = struct
  include Float64

  include Impl2_D
  include Impl4_D

  module Vec = struct
    include Vec2_D
    include Vec4_D
  end

  module Mat = struct
    include Mat2_D
    include Mat4_D
  end
end

module C = struct
  include Complex32

  include Impl2_C
  include Impl4_C

  module Vec = struct
    include Vec2_C
    include Vec4_C
  end

  module Mat = struct
    include Mat2_C
    include Mat4_C
  end
end

module Z = struct
  include Complex64

  include Impl2_Z
  include Impl4_Z

  module Vec = struct
    include Vec2_Z
    include Vec4_Z
  end

  module Mat = struct
    include Mat2_Z
    include Mat4_Z
  end
end
