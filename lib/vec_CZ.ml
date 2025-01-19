(* File: vec_CZ.ml

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

open Complex
open Vec4_CPREC

let random ?rnd_state ?(re_from = -1.) ?(re_range = 2.) ?(im_from = -1.)
    ?(im_range = 2.) n =
  let vec = create n in
  let state =
    match rnd_state with None -> Random.get_state () | Some state -> state
  in
  for row = 1 to n do
    vec.{row} <-
      {
        re = Random.State.float state re_range +. re_from;
        im = Random.State.float state im_range +. im_from;
      }
  done;
  if rnd_state = None then Random.set_state state;
  vec
