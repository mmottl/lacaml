(* File: lacaml.mli

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

(** Binding to BLAS and LAPACK libraries. *)

open Bigarray

(** Pretty-printing of vector and matrices. *)
module Io : sig
  include Io
end

module Common : sig
  include Common
end

(** Binding to the {{:http://www.netlib.org/blas/}BLAS} and
    {{:http://www.netlib.org/lapack/}LAPACK} libraries.  You can make
    use of this library by referring to the corresponding module you
    need for your precision and number type:
    {[
    open Lacaml.Impl.S
    open Lacaml.Impl.D
    open Lacaml.Impl.C
    open Lacaml.Impl.Z]}

    To use this library, you should be familiar with BLAS and LAPACK.  The
    following {{:http://www.netlib.org/blas/blasqr.ps}quick reference
    guide for the BLAS} and
    {{:http://www.netlib.org/lapack/lapackqref.ps}LAPACK quick
    reference} may be useful to you.  For the precise description of
    the functions, consult the man pages
    {{:http://www.math.utah.edu/software/lapack/}online} or, if you
    {{:http://www.netlib.org/lapack/manpages.tgz}installed} them on
    your machine (if you use Linux, they should be in the packages of
    your distribution), read them with Emacs: [M-x man] (under Unix)
    or [M-x woman] (all systems). *)
module Impl :
sig
  (** Pretty printing of real vector and matrices.  See the
      {!Lacaml.Io} module for more versatile functions. *)
  module Real_io : sig
    val pp_num : Format.formatter -> float -> unit
    (** [pp_num ppf el] is equivalent to [fprintf ppf "%G" el]. *)
    val pp_vec : (float, 'a) Io.pp_vec
    (** Pretty-printer for column vectors. *)
    val pp_mat : (float, 'a) Io.pp_mat
    (** Pretty-printer for matrices. *)
  end

  (** Pretty printing of complex vector and matrices.  See the
      {!Lacaml.Io} module for more versatile functions. *)
  module Complex_io : sig
    val pp_num : Format.formatter -> Complex.t -> unit
    (** [pp_num ppf el] is equivalent to [fprintf ppf "(%G, %Gi)"
        el.re el.im]. *)
    val pp_vec : (Complex.t, 'a) Io.pp_vec
    (** Pretty-printer for column vectors. *)
    val pp_mat : (Complex.t, 'a) Io.pp_mat
    (** Pretty-printer for matrices. *)
  end

  (** Double precision real BLAS and LAPACK functions. *)
  module D : sig
    type prec = float64_elt
    type num_type = float
    type vec = (float, float64_elt, fortran_layout) Array1.t
    (** Double precision vectors. *)
    type rvec = vec
    type mat = (float, float64_elt, fortran_layout) Array2.t
    (** Double precision matrices. *)

    type trans3 = [ `N | `T ]

    val prec : (float, float64_elt) Bigarray.kind
    (** Precision for this submodule {!D}.  Allows to write precision
        independent code. *)

    module Vec : sig
      include Vec2_D
      include Vec4_D
    end

    module Mat : sig
      include Mat2_D
      include Mat4_D
    end
    include module type of Real_io

    include Impl2_D
    include Impl4_D
  end

  (** Single precision real BLAS and LAPACK functions. *)
  module S : sig
    type prec = float32_elt
    type num_type = float
    type vec = (float, float32_elt, fortran_layout) Array1.t
    (** Single precision vectors. *)
    type rvec = vec
    type mat = (float, float32_elt, fortran_layout) Array2.t
    (** Single precision matrices. *)

    type trans3 = [ `N | `T ]

    val prec : (float, float32_elt) Bigarray.kind
    (** Precision for this submodule {!S}.  Allows to write precision
        independent code. *)

    module Vec : sig
      include Vec2_S
      include Vec4_S
    end

    module Mat : sig
      include Mat2_S
      include Mat4_S
    end
    include module type of Real_io

    include Impl2_S
    include Impl4_S
  end

  (** Double precision complex BLAS and LAPACK functions. *)
  module Z : sig
    type prec = complex64_elt
    type num_type = Complex.t
    type vec = (Complex.t, complex64_elt, fortran_layout) Array1.t
    (** Double precision vectors. *)
    type rvec = (float, float64_elt, fortran_layout) Array1.t
    (** Double precision vectors of reals. *)
    type mat = (Complex.t, complex64_elt, fortran_layout) Array2.t
    (** Double precision matrices. *)

    type trans3 = [ `C | `N | `T ]
    val prec : (Complex.t, complex64_elt) Bigarray.kind
    (** Precision for this submodule {!Z}.  Allows to write precision
        independent code. *)

    module Vec : sig
      include Vec2_Z
      include Vec4_Z
    end

    module Mat : sig
      include Mat2_Z
      include Mat4_Z
    end
    include module type of Complex_io

    include Impl2_Z
    include Impl4_Z
  end

  (** Single precision complex BLAS and LAPACK functions. *)
  module C : sig
    type prec = complex32_elt
    type num_type = Complex.t
    type vec = (Complex.t, complex32_elt, fortran_layout) Array1.t
    (** Single precision vectors. *)
    type rvec = (float, float32_elt, fortran_layout) Array1.t
    (** Single precision vectors of reals. *)
    type mat = (Complex.t, complex32_elt, fortran_layout) Array2.t
    (** Single precision matrices. *)

    type trans3 = [ `C | `N | `T ]
    val prec : (Complex.t, complex32_elt) Bigarray.kind
    (** Precision for this submodule {!C}.  Allows to write precision
        independent code. *)

    module Vec : sig
      include Vec2_C
      include Vec4_C
    end

    module Mat : sig
      include Mat2_C
      include Mat4_C
    end
    include module type of Complex_io

    include Impl2_C
    include Impl4_C
  end
end
