
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

let permutation_vec n =
  shuffle n
  |> Array.map (fun i -> Int32.of_int (i + 1))
  |> Array1.of_array Int32 Fortran_layout

let () =
  let () = Random.self_init () in
  let r = 10 in
  let m = Mat.random r 3 in
  let ipiv = permutation_vec r in
  printf "Before: m = @[%a@]@\n@\n" pp_fmat m;
  printf "Shuffle rows (laswp) by: @[%a@]@\n@\n" pp_ivec ipiv;
  laswp m ipiv;
  printf "After: m = @[%a@]@\n@\n" pp_fmat m;
  let m = Mat.random 3 r in
  let k = permutation_vec r in
  printf "Before: m = @[%a@]@\n@\n" pp_fmat m;
  printf "Permute columns (lapmt) by: @[%a@]@\n@\n" pp_ivec ipiv;
  lapmt m k;
  printf "After: m = @[%a@]@\n@\n" pp_fmat m;
 
