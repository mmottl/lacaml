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

(** Binding to the {{:http://www.netlib.org/blas/}BLAS} and
    {{:http://www.netlib.org/lapack/}LAPACK} libraries.  You can make
    use of this library by referring to the corresponding module you
    need for your precision and number type:
    {[
    open Lacaml.S
    open Lacaml.D
    open Lacaml.C
    open Lacaml.Z]}

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
    or [M-x woman] (all systems).
 *)

open Bigarray

(** {2 Pretty printing} *)

(** Pretty-printing of vector and matrices. *)
module Io : sig
  include module type of Io
end

(** Pretty printing of real vector and matrices.  See the
    {!Lacaml.Io} module for more versatile functions. *)
module Real_io : sig
  include module type of Real_io
end

(** Pretty printing of complex vector and matrices.  See the
    {!Lacaml.Io} module for more versatile functions. *)
module Complex_io : sig
  include module type of Complex_io
end


(** {2 Precision dependent modules} *)

(** Types and functions common to all precision dependent sub-modules. *)
module Common : sig
  include module type of Common
end
open Common

(** Utility functions *)
module Utils : sig
  include module type of Lacaml_utils
end

(** Double precision real BLAS and LAPACK functions. *)
module D : sig
  include module type of D
end

(** Single precision real BLAS and LAPACK functions. *)
module S : sig
  include module type of S
end

(** Double precision complex BLAS and LAPACK functions. *)
module Z : sig
  include module type of Z
end

(** Single precision complex BLAS and LAPACK functions. *)
module C : sig
  include module type of C
end
