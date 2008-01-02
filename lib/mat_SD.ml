(* File: mat_SD.ml

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

open Bigarray
open Mat4_FPREC
open Floatxx

let hilbert n =
  let mat = create n n in
  for col = 1 to n do
    for row = 1 to n do
      mat.{row, col} <- 1. /. float (row + col - 1)
    done
  done;
  mat

let hankel n =
  if n = 0 then empty
  else
    let mat = make n n 0. in
    let n1 = n + 1 in
    let rec loop c r =
      mat.{r, c} <- float (c + r - 1);
      if c + r >= n1 then
        if c = n then mat
        else loop (c + 1) 1
      else loop c (r + 1) in
    loop 1 1

let pascal n =
  let mat = make n n 1. in
  for r = 2 to n do
    for c = 2 to n do
      mat.{r, c} <- mat.{r - 1, c} +. mat.{r, c - 1}
    done
  done;
  mat

let rosser_ar =
  [|
    [|  611.;  196.; -192.;  407.;   -8.;  -52.;  -49.;   29.; |];
    [|  196.;  899.;  113.; -192.;  -71.;  -43.;   -8.;  -44.; |];
    [| -192.;  113.;  899.;  196.;   61.;   49.;    8.;   52.; |];
    [|  407.; -192.;  196.;  611.;    8.;   44.;   59.;  -23.; |];
    [|   -8.;  -71.;   61.;    8.;  411.; -599.;  208.;  208.; |];
    [|  -52.;  -43.;   49.;   44.; -599.;  411.;  208.;  208.; |];
    [|  -49.;   -8.;    8.;   59.;  208.;  208.;   99.; -911.; |];
    [|   29.;  -44.;   52.;  -23.;  208.;  208.; -911.;   99.; |];
  |]

let rosser () = Array2.of_array prec fortran_layout rosser_ar

let toeplitz v =
  let len = Array1.dim v in
  if len = 0 then empty
  else if len = 1 then make 1 1 v.{1}
  else (
    if len mod 2 <> 1 then
      invalid_arg "toeplitz: v has even number of elements";
    let n = (len + 1) / 2 in
    let mat = create n n in
    let rec loop r c i =
      mat.{r, c} <- v.{i};
      if r = n then
        if c = n then loop 1 2 (i + 1)
        else loop (n - c) 1 (i + 1)
      else
        if c = n then
          if r = 1 then mat
          else loop 1 (n - r + 2) (i + 1)
        else loop (r + 1) (c + 1) i in
    loop n 1 1)

let vandermonde v =
  let n = Array1.dim v in
  if n = 0 then empty
  else if n = 1 then make 1 1 1.0
  else (
    let mat = create n n in
    for i = 1 to n do mat.{i, 1} <- 1.0 done;
    for i = 1 to n do mat.{i, 2} <- v.{i} done;
    for pow = 2 to n - 1 do
      let fpow = float pow in
      for i = 1 to n do mat.{i, pow + 1} <- v.{i} ** fpow done
    done;
    mat)

let wilkinson n =
  if n < 3 then invalid_arg "wilkinson: n < 3";
  if n mod 2 <> 1 then invalid_arg "wilkinson: n is an even number";
  let mat = make n n 0.0 in
  for i = 2 to n do
    let i_1 = i - 1 in
    mat.{i, i_1} <- 1.0;
    mat.{i_1, i} <- 1.0
  done;
  let n_2 = n / 2 in
  let n1_2 = n_2 + 1 in
  for i = 1 to n_2 do mat.{i, i} <- float (n1_2 - i) done;
  for i = n1_2 + 1 to n do mat.{i, i} <- float (i - n1_2) done;
  mat

let random ?rnd_state ?(from = -1.) ?(range = 2.) m n =
  let mat = create m n in
  let state =
    match rnd_state with
    | None -> Random.get_state ()
    | Some state -> state in
  for row = 1 to m do
    for col = 1 to n do
      mat.{row, col} <- Random.State.float state range +. from
    done
  done;
  if rnd_state = None then Random.set_state state;
  mat
