(* File: real_io.mli

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

val pp_num : Format.formatter -> float -> unit
(** [pp_num ppf el] is equivalent to [fprintf ppf "%G" el]. *)

val pp_vec : (float, 'a) Io.pp_vec
(** Pretty-printer for column vectors. *)

val pp_mat : (float, 'a) Io.pp_mat
(** Pretty-printer for matrices. *)
