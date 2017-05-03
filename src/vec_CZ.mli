(* File: vec_CZ.mli

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

(** {5 Vector operations} *)

open Lacaml__complexxx

(** {6 Creation of vectors} *)

val random :
  ?rnd_state : Random.State.t ->
  ?re_from : float -> ?re_range : float ->
  ?im_from : float -> ?im_range : float ->
  int
  -> vec
(** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range n]
    @return a vector of size [n] initialized with random elements sampled
    uniformly from [re_range] and [im_range] starting at [re_from] and
    [im_from] for real and imaginary numbers respectively.  A random state
    [rnd_state] can be passed.

    @param rnd_state default = Random.get_state ()
    @param re_from default = -1.0
    @param re_range default = 2.0
    @param im_from default = -1.0
    @param im_range default = 2.0 *)
