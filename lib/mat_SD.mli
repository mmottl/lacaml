(* File: mat_SD.mli

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

(** Matrix operations *)

open Bigarray
open Lacaml_floatxx

(** {6 Creation of matrices} *)

val hilbert : int -> mat
(** [hilbert n] @return an [n]x[n] Hilbert matrix. *)

val hankel : int -> mat
(** [hankel n] @return an [n]x[n] Hankel matrix. *)

val pascal : int -> mat
(** [pascal n] @return an [n]x[n] Pascal matrix. *)

val rosser : unit -> mat
(** [rosser n] @return 8x8 Rosser matrix. *)

val toeplitz : vec -> mat
(** [toeplitz v] @return the Toeplitz matrix associated with [v].
    The constant diagonals are read from left to right from [v].
    @raise Invalid_argument if the length of [v] is not an odd number. *)

val vandermonde : vec -> mat
(** [vandermonde v] @return the Vandermonde matrix associated with [v]. *)

val wilkinson : int -> mat
(** [wilkinson n] @return the [n]x[n] Wilkinson matrix.
    @raise Invalid_argument if [n] is not an odd number >= 3. *)

val random :
  ?rnd_state : Random.State.t ->
  ?from : float -> ?range : float ->
  int -> int
  -> mat
(** [random ?rnd_state ?from ?range m n] @return an [m]x[n] matrix
    initialized with random elements sampled uniformly from [range]
    starting at [from].  A random state [rnd_state] can be passed.

    @param rnd_state default = Random.get_state ()
    @param from default = -1.0
    @param range default = 2.0 *)
