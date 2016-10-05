(* File: complex_io.mli

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

val pp_num : Format.formatter -> Complex.t -> unit
(** [pp_num ppf el] is equivalent to [fprintf ppf "(%G, %Gi)"
    el.re el.im]. *)

val pp_vec : (Complex.t, 'a) Lacaml_io.pp_vec
(** Pretty-printer for column vectors. *)

val pp_mat : (Complex.t, 'a) Lacaml_io.pp_mat
(** Pretty-printer for matrices. *)
