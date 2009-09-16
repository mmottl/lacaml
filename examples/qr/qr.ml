(* File: qr.ml

   Copyright (C) 2009-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

open Lacaml.Impl.D
open Lacaml.Io

let () = Random.self_init ()

let () =
  let m = 10 in
  let n = 5 in
  let a = Mat.random m n in
  printf "@[<2>A =@\n@\n@[%a@]@]@\n@\n" pp_mat a;
  let tau = geqrf a in
  let r = Mat.make0 n n in
  let r = lacpy ~m:n ~uplo:`U a ~b:r in
  orgqr ~tau a;
  printf "@[<2>Q =@\n@\n@[%a@]@]@\n@\n" pp_mat a;
  printf "@[<2>R =@\n@\n@[%a@]@]@\n@\n" pp_mat r;
  printf "@[<2>QR = A =@\n@\n@[%a@]@]@\n" pp_mat (gemm a r)
