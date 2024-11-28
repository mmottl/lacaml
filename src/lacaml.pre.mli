(* File: lacaml.mli -*-tuareg-*-

   Copyright © 2010-

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

(** Binding to the {{:http://www.netlib.org/blas/} BLAS} and
    {{:http://www.netlib.org/lapack/} LAPACK} libraries. You can make use of
    this library by referring to the corresponding module you need for your
    precision and number type:
    {[
      open Lacaml.S
      open Lacaml.D
      open Lacaml.C
      open Lacaml.Z
    ]}

    To use this library, you should be familiar with BLAS and LAPACK. The
    following
    {{:http://www.netlib.org/blas/blasqr.ps} quick reference guide for the BLAS}
    and {{:http://www.netlib.org/lapack/lapackqref.ps} LAPACK quick reference}
    may be useful to you. For the precise description of the functions, consult
    the man pages {{:http://www.math.utah.edu/software/lapack/} online} or, if
    you {{:http://www.netlib.org/lapack/manpages.tgz} installed} them on your
    machine (if you use Linux, they should be in the packages of your
    distribution), read them with Emacs: [M-x man] (under Unix) or [M-x woman]
    (all systems). *)

(** {2 Pretty printing} *)

module Io : module type of Io
(** Pretty-printing of vector and matrices. *)

(** {2 Precision dependent modules} *)

module Common : module type of Common
(** Types and functions common to all precision dependent sub-modules. *)

module D : module type of D
(** Double precision real BLAS and LAPACK functions. *)

module S : module type of S
(** Single precision real BLAS and LAPACK functions. *)

module Z : module type of Z
(** Double precision complex BLAS and LAPACK functions. *)

module C : module type of C
(** Single precision complex BLAS and LAPACK functions. *)

module Utils : module type of Utils
(** {2 Utility functions} *)
