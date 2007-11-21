(* File: vec_CZ.ml

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

open Complex
open Vec4_CPREC

let random
      ?rnd_state
      ?(re_from = -1.) ?(re_range = 2.)
      ?(im_from = -1.) ?(im_range = 2.)
      n =
  let vec = create n in
  let state =
    match rnd_state with
    | None -> Random.get_state ()
    | Some state -> state in
  for row = 1 to n do
    vec.{row} <-
      {
        re = Random.State.float state re_range +. re_from;
        im = Random.State.float state im_range +. im_from;
      }
  done;
  if rnd_state = None then Random.set_state state;
  vec
