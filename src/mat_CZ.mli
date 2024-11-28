(* File: mat_CZ.mli

   Copyright © 2001-

   Markus Mottl <markus.mottl@gmail.com>

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

(** {5 Matrix operations} *)

open Complexxx

(** {6 Creation of matrices} *)

val random :
  ?rnd_state:Random.State.t ->
  ?re_from:float ->
  ?re_range:float ->
  ?im_from:float ->
  ?im_range:float ->
  int ->
  int ->
  mat
(** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range m n]
    @return
      an [m]x[n] matrix initialized with random elements sampled uniformly from
      [re_range] and [im_range] starting at [re_from] and [im_from] for real and
      imaginary numbers respectively. A random state [rnd_state] can be passed.

    @param rnd_state default = Random.get_state ()
    @param re_from default = -1.0
    @param re_range default = 2.0
    @param im_from default = -1.0
    @param im_range default = 2.0 *)
