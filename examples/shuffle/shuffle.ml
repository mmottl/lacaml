
open Format

open Lacaml.D
open Lacaml.Io
open Bigarray

let shuffle n =
  let arr = Array.init n (fun i -> i) in
  for i = 0 to n - 2 do
    arr.(i) <- arr.(i) + Random.int (n - i)
  done;
  arr

let () =
  let () = Random.self_init () in
  let r = 10 in
  let m = Mat.random r 3 in
  let ipiv =
    shuffle r
    |> Array.map (fun i -> Int32.of_int (i + 1))
    |> Array1.of_array Int32 Fortran_layout 
  in
  printf "Before: m = @[%a@]@\n@\n" pp_fmat m;
  printf "Shuffle by: @[%a@]@\n@\n" pp_ivec ipiv;
  laswp m ipiv;
  printf "After: m = @[%a@]@\n@\n" pp_fmat m
