(* File: eig.ml

   Copyright © 2010-

   Christophe Troestler email: Christophe.Troestler@umons.ac.be WWW:
   http://math.umons.ac.be/an/

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

(** Example based on http://www.nag.co.uk/lapack-ex/node97.html *)

open Format
open Lacaml.D
open Lacaml.Io

let a =
  Mat.of_array
    [|
      [| nan; nan; 0.42; 0.63 |];
      [| nan; 0.39; 0.79; 0.48 |];
      (* above diag *)
      [| 0.24; -0.11; -0.25; -0.03 |];
    |]
(* diag *)

let b =
  Mat.of_array [| [| nan; 0.95; -0.29; -0.33 |]; [| 2.07; 1.69; 0.65; 1.17 |] |]

let () =
  let eig = sbgv a b in
  printf "@[<2>Eigenvalues: @[%a@]@]@\n@\n" pp_rfvec eig
