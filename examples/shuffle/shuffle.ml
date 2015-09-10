
open Format

open Lacaml.D
open Lacaml.Io
open Bigarray

let shuffle arr =
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done;
  arr

let () =
  let () = Random.self_init () in
  let r = 10 in
  let m = Mat.random r 3 in
  let ipiv =
    Array.init r (fun i -> Int32.of_int (i + 1))
    |> shuffle
    |> Array1.of_array Int32 Fortran_layout 
  in
  printf "Before: m = @[%a@]@\n@\n" pp_fmat m;
  printf "Shuffle by: @[%a@]@\n@\n" pp_ivec ipiv;
  laswp m ipiv;
  printf "After: m = @[%a@]@\n@\n" pp_fmat m
