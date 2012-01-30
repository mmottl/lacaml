(* File: lin_eq.ml

   Copyright (C) 2004-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://www.umh.ac.be/math/an/

     Oleg Trott
     email: ot14@columbia.edu
     WWW: http://www.columbia.edu/~ot14

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

open Format
open Bigarray

open Lacaml.D
open Lacaml.Io

let () =
  let n = 10 in
  let kl = 2 and ku = 1 in
  let row = 2 in
  let ab = Mat.random ~from:(-500.) ~range:1000. (2 * kl + ku + row) n in
  let b = Vec.random n in
  let sol = copy b in
  gbsv (lacpy ab) kl ku ~abr:row (Mat.from_col_vec sol);
  let res = Vec.sub (gbmv ab kl ku ~ar:(kl + row) sol) b in
  printf "@[<2>Solving general band matrix:@\n@\n";
  printf "Sol: X  = @[%a@]@\n" pp_rfvec sol;
  printf "A X - b = %a@]@\n@\n" pp_rfvec res;

