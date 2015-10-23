
open Format

open Lacaml.D
open Lacaml.Io
open Bigarray

let reorder arr =
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done;
  arr

let reordered n =
  Array.init n (fun i -> i)
  |> reorder

let shuffle n =
  let arr = Array.init n (fun i -> i) in
  for i = 0 to n - 2 do
    arr.(i) <- arr.(i) + Random.int (n - i)
  done;
  arr

let to_vec arr =
  Array.map (fun i -> Int32.of_int (i + 1)) arr
  |> Array1.of_array Int32 Fortran_layout

let () =
  let () = Random.self_init () in
  let r = 10 in
  let m = Mat.random r 3 in
  let ipiv = to_vec (shuffle r) in
  printf "Before: m = @[%a@]@\n@\n" pp_fmat m;
  printf "Shuffle rows (laswp) by: @[%a@]@\n@\n" pp_ivec ipiv;
  laswp m ipiv;
  printf "After: m = @[%a@]@\n@\n" pp_fmat m;
  let m = Mat.random 3 r in
  let k = to_vec (let a = reordered r in a.(3) <- 3; a) in
  let forward = if Random.int 2 = 0 then true else false in
  printf "Before: m = @[%a@]@\n@\n" pp_fmat m;
  printf "Permute columns (lapmt) %s by: @[%a@]@\n@\n%!"
    (if forward then "forward" else "backward")
    pp_ivec k;
  lapmt ~forward m k;
  printf "After: m = @[%a@]@\n@\n" pp_fmat m;
 
