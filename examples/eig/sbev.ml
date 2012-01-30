(* File: sbev.ml

   Copyright (C) 2011-

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

(** Example based on http://www.nag.co.uk/lapack-ex/node61.html *)

open Format
open Bigarray

open Lacaml.D
open Lacaml.Io

let a = Mat.of_array
  [| [|  nan; nan; 3.; 4.; 5. |];
     [|  nan;  2.; 3.; 4.; 5. |];   (* above diag *)
     [|   1.;  2.; 3.; 4.; 5. |] |] (* diag *)

let () =
  let z = Mat.create 5 5 in
  let eig = sbev a ~z in
  printf "@[<2>Eigenvalues: @[%a@]@]@\n" pp_rfvec eig;
  printf "@[<2>Eigenvectors (one per column): @\n";
  printf "@[%a@]@]@\n" pp_fmat z
