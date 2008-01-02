(* File: io.ml

   Copyright (C) 2005

     Jane Street Holding, LLC
     Author: Markus Mottl
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
open Complex

let from_col_vec v = reshape_2 (genarray_of_array1 v) (Array1.dim v) 1
let from_row_vec v = reshape_2 (genarray_of_array1 v) 1 (Array1.dim v)

let pp_open ppf = pp_open_box ppf 0
let pp_close ppf = pp_close_box ppf ()
let pp_newline ppf = pp_force_newline ppf ()
let pp_space ppf = pp_print_string ppf " "

let pp_end_row_newline ppf _ = pp_newline ppf
let pp_end_row_space ppf _ = pp_space ppf
let pp_end_col_space ppf ~row:_ ~col:_ = pp_space ppf

let pp_padded_str ppf pad_c max_len str =
  let str_len = String.length str in
  let diff = max_len - str_len in
  if diff = 0 then pp_print_string ppf str
  else
    let out_str = String.make max_len pad_c in
    String.blit str 0 out_str diff str_len;
    pp_print_string ppf out_str

let some_space = Some ' '

let extract_buf buf buf_ppf =
  pp_print_flush buf_ppf ();
  let str = Buffer.contents buf in
  Buffer.clear buf;
  str, String.length str

let pp_el_buf pp_el buf buf_ppf el =
  pp_el buf_ppf el;
  extract_buf buf buf_ppf

let pp_buf pp buf buf_ppf =
  pp buf_ppf;
  extract_buf buf buf_ppf

let ignore2 _ _ = ()

let pp_mat_gen
    ?(pp_open = pp_open)
    ?(pp_close = pp_close)
    ?pp_head
    ?pp_foot
    ?(pp_end_row = pp_end_row_newline)
    ?(pp_end_col = pp_end_col_space)
    ?pp_left
    ?pp_right
    ?(pad = some_space)
    pp_el ppf mat =
  let m = Array2.dim1 mat in
  if m > 0 then (
    let n = Array2.dim2 mat in
    if n > 0 then (
      pp_open ppf;
      let do_pp_right =
        match pp_right with
        | None -> ignore2
        | Some pp_right ->
            let buf = Buffer.create 32 in
            let buf_ppf = formatter_of_buffer buf in
            (fun ppf row ->
              let str, n_str =
                pp_buf (fun ppf -> pp_right ppf row) buf buf_ppf in
              if n_str <> 0 then (
                pp_end_col ppf ~row ~col:n;
                pp_print_string ppf str)) in
      (match pad with
      | Some pad_c ->
          let buf = Buffer.create 32 in
          let buf_ppf = formatter_of_buffer buf in
          let heads, foots, max_lens =
            match pp_head, pp_foot with
            | None, None -> [||], [||], Array.make n 0
            | Some pp_head, None ->
                let heads = Array.make n "" in
                let make_max_init ix =
                  let head, head_len =
                    pp_buf (fun ppf -> pp_head ppf (ix + 1)) buf buf_ppf in
                  heads.(ix) <- head;
                  head_len in
                heads, [||], Array.init n make_max_init
            | None, Some pp_foot ->
                let foots = Array.make n "" in
                let make_max_init ix =
                  let foot, foot_len =
                    pp_buf (fun ppf -> pp_foot ppf (ix + 1)) buf buf_ppf in
                  foots.(ix) <- foot;
                  foot_len in
                [||], foots, Array.init n make_max_init
            | Some pp_head, Some pp_foot ->
                let heads = Array.make n "" in
                let foots = Array.make n "" in
                let make_max_init ix =
                  let head, head_len =
                    pp_buf (fun ppf -> pp_head ppf (ix + 1)) buf buf_ppf in
                  let foot, foot_len =
                    pp_buf (fun ppf -> pp_foot ppf (ix + 1)) buf buf_ppf in
                  heads.(ix) <- head;
                  foots.(ix) <- foot;
                  max head_len foot_len in
                heads, foots, Array.init n make_max_init in
          let many_strs =
            Array.init m (fun row ->
              Array.init n (fun col ->
                let str, str_len =
                  pp_el_buf pp_el buf buf_ppf mat.{row + 1, col + 1} in
                if str_len > max_lens.(col) then max_lens.(col) <- str_len;
                str)) in
          let n_1 = n - 1 in
          (match pp_left with
          | None ->
              let max_len0 = max_lens.(0) in
              if pp_head <> None then (
                pp_padded_str ppf pad_c max_len0 heads.(0);
                for col_ix = 1 to n_1 do
                  pp_end_col ppf ~row:0 ~col:col_ix;
                  pp_padded_str ppf pad_c max_lens.(col_ix) heads.(col_ix)
                done;
                do_pp_right ppf 0;
                pp_end_row ppf 0);
              let strs0 = many_strs.(0) in
              pp_padded_str ppf pad_c max_len0 strs0.(0);
              for col_ix = 1 to n_1 do
                pp_end_col ppf ~row:1 ~col:col_ix;
                pp_padded_str ppf pad_c max_lens.(col_ix) strs0.(col_ix)
              done;
              for row_ix = 1 to m - 1 do
                do_pp_right ppf row_ix;
                pp_end_row ppf row_ix;
                let strs = many_strs.(row_ix) in
                pp_padded_str ppf pad_c max_len0 strs.(0);
                let row = row_ix + 1 in
                for col_ix = 1 to n_1 do
                  pp_end_col ppf ~row ~col:col_ix;
                  pp_padded_str ppf pad_c max_lens.(col_ix) strs.(col_ix)
                done
              done;
              do_pp_right ppf m;
              if pp_foot <> None then (
                pp_end_row ppf m;
                pp_padded_str ppf pad_c max_len0 foots.(0);
                let m1 = m + 1 in
                for col_ix = 1 to n_1 do
                  pp_end_col ppf ~row:m1 ~col:col_ix;
                  pp_padded_str ppf pad_c max_lens.(col_ix) foots.(col_ix)
                done;
                do_pp_right ppf m1)
          | Some pp_left ->
              let max_len_row_labels_ref = ref 0 in
              let row_labels =
                Array.init m
                  (fun ix ->
                     let label, len =
                       pp_buf (fun ppf -> pp_left ppf (ix + 1)) buf buf_ppf in
                     max_len_row_labels_ref := max !max_len_row_labels_ref len;
                     label) in
              let foot0 =
                if pp_foot <> None then
                  let foot0, foot0_len =
                    pp_buf (fun ppf -> pp_left ppf (m + 1)) buf buf_ppf in
                  max_len_row_labels_ref :=
                    max !max_len_row_labels_ref foot0_len;
                  foot0
                else "" in
              let max_len_row_labels =
                if pp_head <> None then
                  let row_label0, row_label0_len =
                    pp_buf (fun ppf -> pp_left ppf 0) buf buf_ppf in
                  max_len_row_labels_ref :=
                    max !max_len_row_labels_ref row_label0_len;
                  let max_len_row_labels = !max_len_row_labels_ref in
                  pp_padded_str ppf pad_c max_len_row_labels row_label0;
                  for col_ix = 0 to n_1 do
                    pp_end_col ppf ~row:0 ~col:(col_ix + 1);
                    pp_padded_str ppf pad_c max_lens.(col_ix) heads.(col_ix)
                  done;
                  do_pp_right ppf 0;
                  pp_end_row ppf 0;
                  max_len_row_labels
                else !max_len_row_labels_ref in
              pp_padded_str ppf pad_c max_len_row_labels row_labels.(0);
              let strs0 = many_strs.(0) in
              for col_ix = 0 to n_1 do
                pp_end_col ppf ~row:1 ~col:col_ix;
                pp_padded_str ppf pad_c max_lens.(col_ix) strs0.(col_ix)
              done;
              for row_ix = 1 to m - 1 do
                do_pp_right ppf row_ix;
                pp_end_row ppf row_ix;
                pp_padded_str ppf pad_c max_len_row_labels row_labels.(row_ix);
                let strs = many_strs.(row_ix) in
                let row = row_ix + 1 in
                for col_ix = 0 to n_1 do
                  pp_end_col ppf ~row ~col:col_ix;
                  pp_padded_str ppf pad_c max_lens.(col_ix) strs.(col_ix)
                done
              done;
              do_pp_right ppf m;
              if pp_foot <> None then (
                pp_end_row ppf m;
                pp_padded_str ppf pad_c max_len_row_labels foot0;
                let m1 = m + 1 in
                for col_ix = 0 to n_1 do
                  pp_end_col ppf ~row:m1 ~col:col_ix;
                  pp_padded_str ppf pad_c max_lens.(col_ix) foots.(col_ix)
                done;
                do_pp_right ppf m1))
      | None ->
          (match pp_head with
          | None -> ()
          | Some pp_head ->
              (match pp_left with
              | None ->
                  pp_head ppf 1;
                  for col_ix = 1 to n - 1 do
                    pp_end_col ppf ~row:0 ~col:col_ix;
                    pp_head ppf (col_ix + 1)
                  done
              | Some pp_left ->
                  pp_left ppf 0;
                  for col_ix = 0 to n - 1 do
                    pp_end_col ppf ~row:0 ~col:col_ix;
                    pp_head ppf (col_ix + 1)
                  done);
              do_pp_right ppf 0;
              pp_end_row ppf 0);
          (match pp_left with
          | None ->
              pp_el ppf mat.{1, 1};
              for col = 2 to n do
                pp_end_col ppf ~row:1 ~col:(col - 1);
                pp_el ppf mat.{1, col}
              done;
              for row = 2 to m do
                let row_1 = row - 1 in
                do_pp_right ppf row_1;
                pp_end_row ppf row_1;
                pp_el ppf mat.{row, 1};
                for col = 2 to n do
                  pp_end_col ppf ~row ~col:(col - 1);
                  pp_el ppf mat.{row, col}
                done
              done
          | Some pp_left ->
              pp_left ppf 1;
              for col = 1 to n do
                pp_end_col ppf ~row:1 ~col:(col - 1);
                pp_el ppf mat.{1, col}
              done;
              for row = 2 to m do
                let row_1 = row - 1 in
                do_pp_right ppf row_1;
                pp_end_row ppf row_1;
                pp_left ppf row;
                for col = 1 to n do
                  pp_end_col ppf ~row ~col:(col - 1);
                  pp_el ppf mat.{row, col}
                done
              done);
          do_pp_right ppf m;
          match pp_foot with
          | None -> ()
          | Some pp_foot ->
              pp_end_row ppf m;
              let m1 = m + 1 in
              let col_start =
                match pp_left with
                | None -> pp_foot ppf 1; 1
                | Some pp_left -> pp_left ppf 0; 0 in
              for col_ix = col_start to n - 1 do
                pp_end_col ppf ~row:m1 ~col:col_ix;
                pp_foot ppf (col_ix + 1)
              done;
              do_pp_right ppf m1);
      pp_close ppf))


(* Pretty-printing elements *)

type 'el pp_el_default = (formatter -> 'el -> unit) ref

let pp_float_el_default_fun ppf el = fprintf ppf "%G" el
let pp_float_el_default = ref pp_float_el_default_fun

let pp_complex_el_default_fun ppf el = fprintf ppf "(%G, %Gi)" el.re el.im
let pp_complex_el_default = ref pp_complex_el_default_fun

let pp_float_el ppf el = !pp_float_el_default ppf el
let pp_complex_el ppf el = !pp_complex_el_default ppf el

let pp_print_int32 ppf n = fprintf ppf "%ld" n


(* Pretty-printing in standard style *)

(* Vectors *)

type ('el, 'elt) pp_vec =
  formatter ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit

let pp_fvec ppf vec = pp_mat_gen pp_float_el ppf (from_col_vec vec)
let pp_cvec ppf vec = pp_mat_gen pp_complex_el ppf (from_col_vec vec)
let pp_ivec ppf vec = pp_mat_gen pp_print_int32 ppf (from_col_vec vec)

let pp_rfvec ppf vec =
  let mat = from_row_vec vec in
  pp_mat_gen ~pp_end_row:pp_end_row_space ~pad:None pp_float_el ppf mat

let pp_rcvec ppf vec =
  let mat = from_row_vec vec in
  pp_mat_gen ~pp_end_row:pp_end_row_space ~pad:None pp_complex_el ppf mat

let pp_rivec ppf vec =
  let mat = from_row_vec vec in
  pp_mat_gen ~pp_end_row:pp_end_row_space ~pad:None pp_print_int32 ppf mat

(* Matrices *)

type ('el, 'elt) pp_mat =
  formatter ->
  ('el, 'elt, fortran_layout) Array2.t
  -> unit

let pp_fmat ppf mat = pp_mat_gen pp_float_el ppf mat
let pp_cmat ppf mat = pp_mat_gen pp_complex_el ppf mat
let pp_imat ppf mat = pp_mat_gen pp_print_int32 ppf mat


(* Labeled pretty-printing *)

(* Labeled matrices *)

type ('el, 'elt) pp_labeled_mat =
  ?pp_head : (formatter -> int -> unit) option ->
  ?pp_foot : (formatter -> int -> unit) option ->
  ?pp_left : (formatter -> int -> unit) option ->
  ?pp_right : (formatter -> int -> unit) option ->
  ?pad : char option ->
  unit ->
  formatter ->
  ('el, 'elt, fortran_layout) Array2.t
  -> unit

let get_pp_head_foot_mat = function
  | None -> Some pp_print_int
  | Some pp_head -> pp_head

let get_some_pp_left_right m =
  Some (fun ppf row_col ->
    if row_col > 0 && row_col <= m then pp_print_int ppf row_col)

let get_pp_left_right m = function
  | None -> get_some_pp_left_right m
  | Some pp_left -> pp_left

let pp_labeled_mat_gen
    pp_el ?pp_head ?pp_foot ?pp_left ?pp_right ?pad () ppf mat =
  let pp_head = get_pp_head_foot_mat pp_head in
  let pp_foot = get_pp_head_foot_mat pp_foot in
  let m = Array2.dim1 mat in
  let pp_left = get_pp_left_right m pp_left in
  let pp_right = get_pp_left_right m pp_right in
  pp_mat_gen ?pp_head ?pp_foot ?pp_left ?pp_right ?pad pp_el ppf mat

let pp_labeled_fmat ?pp_head = pp_labeled_mat_gen pp_float_el ?pp_head
let pp_labeled_cmat ?pp_head = pp_labeled_mat_gen pp_complex_el ?pp_head
let pp_labeled_imat ?pp_head = pp_labeled_mat_gen pp_print_int32 ?pp_head

(* String-labeled matrices *)

type ('el, 'elt) pp_lmat =
  ?print_head : bool ->
  ?print_foot : bool ->
  ?print_left : bool ->
  ?print_right : bool ->
  ?row_labels : string array ->
  ?col_labels : string array ->
  ?pad : char option ->
  unit ->
  formatter ->
  ('el, 'elt, fortran_layout) Array2.t
  -> unit

let get_some_pp_head_foot_mat col_labels =
  Some (fun ppf col -> pp_print_string ppf col_labels.(col - 1))

let get_pp_left_right_mat print m row_labels =
  if print then
    Some (fun ppf row ->
      if row > 0 && row <= m then pp_print_string ppf row_labels.(row - 1))
  else None

let pp_lmat_gen
      (pp_labeled_mat : ('el, 'elt) pp_labeled_mat)
      ?(print_head = true)
      ?(print_foot = true)
      ?(print_left = true)
      ?(print_right = true)
      ?row_labels
      ?col_labels
      ?pad
      () ppf mat =
  let pp_head, pp_foot =
    match col_labels with
    | Some col_labels when print_head ->
        if Array.length col_labels <> Array2.dim2 mat then
          invalid_arg
            "Io.pp_lmat_gen: dim(col_labels) <> dim2(mat)";
        let pp_head = get_some_pp_head_foot_mat col_labels in
        let pp_foot = if print_foot then pp_head else None in
        pp_head, pp_foot
    | Some col_labels when print_foot ->
        None, get_some_pp_head_foot_mat col_labels
    | _ -> None, None in
  let pp_left, pp_right =
    match row_labels with
    | Some row_labels ->
        let m = Array2.dim1 mat in
        if Array.length row_labels <> m then
          invalid_arg
            "Io.pp_lmat_gen: dim(row_labels) <> dim1(mat)";
        let pp_left = get_pp_left_right_mat print_left m row_labels in
        let pp_right = get_pp_left_right_mat print_right m row_labels in
        pp_left, pp_right
    | None -> None, None in
  pp_labeled_mat ~pp_head ~pp_foot ~pp_left ~pp_right ?pad () ppf mat

let pp_lfmat ?print_head = pp_lmat_gen pp_labeled_fmat ?print_head
let pp_lcmat ?print_head = pp_lmat_gen pp_labeled_cmat ?print_head
let pp_limat ?print_head = pp_lmat_gen pp_labeled_imat ?print_head

(* Labeled vectors *)

type ('el, 'elt) pp_labeled_vec =
  ?pp_head : (formatter -> int -> unit) ->
  ?pp_foot : (formatter -> int -> unit) ->
  ?pp_left : (formatter -> int -> unit) option ->
  ?pp_right : (formatter -> int -> unit) ->
  ?pad : char option ->
  unit ->
  formatter ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit

let pp_labeled_vec_gen
      pp_el ?pp_head ?pp_foot ?pp_left ?pp_right ?pad () ppf vec =
  let m = Array1.dim vec in
  let pp_left =
    match pp_left with
    | None -> get_some_pp_left_right m
    | Some None -> None
    | Some (Some pp_left) -> Some pp_left in
  let mat = from_col_vec vec in
  pp_mat_gen ?pp_head ?pp_foot ?pp_left ?pp_right ?pad pp_el ppf mat

let pp_labeled_fvec ?pp_head = pp_labeled_vec_gen pp_float_el ?pp_head
let pp_labeled_cvec ?pp_head = pp_labeled_vec_gen pp_complex_el ?pp_head
let pp_labeled_ivec ?pp_head = pp_labeled_vec_gen pp_print_int32 ?pp_head

let some_pp_print_int = Some pp_print_int

let pp_labeled_rvec_gen
      pp_el
      ?pp_head:this_pp_head
      ?pp_foot:this_pp_foot
      ?pp_left:this_pp_left
      ?pp_right:this_pp_right
      ?pad () ppf vec =
  let pp_head =
    match this_pp_left with
    | None -> some_pp_print_int
    | Some None -> None
    | Some (Some this_pp_left) -> Some this_pp_left in
  let pp_foot = this_pp_right in
  let pp_left = this_pp_head in
  let pp_right = this_pp_foot in
  let mat = from_row_vec vec in
  pp_mat_gen ?pp_head ?pp_foot ?pp_left ?pp_right ?pad pp_el ppf mat

let pp_labeled_rfvec ?pp_head = pp_labeled_rvec_gen pp_float_el ?pp_head
let pp_labeled_rcvec ?pp_head = pp_labeled_rvec_gen pp_complex_el ?pp_head
let pp_labeled_rivec ?pp_head = pp_labeled_rvec_gen pp_print_int32 ?pp_head

(* String-labeled vectors *)

type ('el, 'elt) pp_lvec =
  ?print_head : bool ->
  ?print_foot : bool ->
  ?print_left : bool ->
  ?print_right : bool ->
  ?labels : string array ->
  ?name : string ->
  ?pad : char option ->
  unit ->
  formatter ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit

let get_lvec_name = function
  | None -> None
  | Some name -> Some [| name |]

let pp_lvec_gen
      (pp_lmat : ('el, 'elt) pp_lmat)
      ?print_head ?print_foot ?print_left ?(print_right = false)
      ?labels:row_labels ?name ?pad () ppf vec =
  let mat = from_col_vec vec in
  let col_labels = get_lvec_name name in
  pp_lmat
    ?print_head ?print_foot ?print_left ~print_right
    ?row_labels ?col_labels ?pad () ppf mat

let pp_lfvec ?print_head = pp_lvec_gen pp_lfmat ?print_head
let pp_lcvec ?print_head = pp_lvec_gen pp_lcmat ?print_head
let pp_livec ?print_head = pp_lvec_gen pp_limat ?print_head

let pp_rlvec_gen
      (pp_lmat : ('el, 'elt) pp_lmat)
      ?print_head:print_left
      ?print_foot:print_right
      ?print_left:print_head
      ?print_right:this_print_right
      ?labels:col_labels ?name ?pad () ppf vec =
  let mat = from_row_vec vec in
  let row_labels = get_lvec_name name in
  let print_foot =
    match this_print_right with
    | None -> false
    | Some this_print_right -> this_print_right in
  pp_lmat
    ?print_head ~print_foot ?print_left ?print_right
    ?row_labels ?col_labels ?pad () ppf mat

let pp_rlfvec ?print_head = pp_rlvec_gen pp_lfmat ?print_head
let pp_rlcvec ?print_head = pp_rlvec_gen pp_lcmat ?print_head
let pp_rlivec ?print_head = pp_rlvec_gen pp_limat ?print_head


(* Pretty-printing in OCaml-style *)

(* Vectors *)

type ('el, 'elt) pp_el_ovec =
  formatter ->
  (formatter -> 'el -> unit) ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit

type ('el, 'elt) pp_ovec =
  formatter ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit

let pp_ocaml_open_vec ppf =
  pp_open_box ppf 2;
  pp_print_string ppf "[|";
  pp_force_newline ppf ()

let pp_ocaml_close_vec ppf =
  pp_print_string ppf ";";
  pp_close_box ppf ();
  pp_force_newline ppf ();
  pp_print_string ppf "|]"

let pp_end_row_semi ppf _ =
  pp_print_string ppf ";";
  pp_force_newline ppf ()

let pp_ovec ppf pp_el vec =
  if Array1.dim vec = 0 then pp_print_string ppf "[||]"
  else
    pp_mat_gen
      ~pp_open:pp_ocaml_open_vec
      ~pp_close:pp_ocaml_close_vec
      ~pp_end_row:pp_end_row_semi
      pp_el
      ppf
      (from_col_vec vec)

let pp_ocaml_open_rvec ppf =
  pp_open_box ppf 2;
  pp_print_string ppf "[| "

let pp_ocaml_close_rvec ppf =
  pp_print_string ppf " |]";
  pp_close_box ppf ()

let pp_end_row_semi_space ppf _ = pp_print_string ppf "; "

let pp_rovec ppf pp_el vec =
  if Array1.dim vec = 0 then pp_print_string ppf "[||]"
  else
    pp_mat_gen
      ~pp_open:pp_ocaml_open_rvec
      ~pp_close:pp_ocaml_close_rvec
      ~pp_end_row:pp_end_row_semi_space
      ~pad:None
      pp_el
      ppf
      (from_col_vec vec)

let pp_ofvec ppf vec = pp_ovec ppf pp_float_el vec
let pp_ocvec ppf vec = pp_ovec ppf pp_complex_el vec
let pp_oivec ppf vec = pp_ovec ppf pp_print_int32 vec

let pp_rofvec ppf vec = pp_rovec ppf pp_float_el vec
let pp_rocvec ppf vec = pp_rovec ppf pp_complex_el vec
let pp_roivec ppf vec = pp_rovec ppf pp_print_int32 vec

(* Matrices *)

type ('el, 'elt) pp_omat =
  formatter ->
  ('el, 'elt, fortran_layout) Array2.t
  -> unit

let pp_ocaml_open_mat ppf =
  pp_open_box ppf 2;
  pp_print_string ppf "[|";
  pp_force_newline ppf ();
  pp_print_string ppf "[| "

let pp_ocaml_close_mat ppf =
  pp_print_string ppf " |];";
  pp_close_box ppf ();
  pp_force_newline ppf ();
  pp_print_string ppf "|]"

let pp_ocaml_end_row_mat ppf _ =
  pp_print_string ppf " |];";
  pp_force_newline ppf ();
  pp_print_string ppf "[| "

let pp_end_col_semi_space ppf ~row:_ ~col:_ = pp_print_string ppf "; "

let pp_omat ppf pp_el mat =
  if Array2.dim1 mat = 0 || Array2.dim2 mat = 0 then pp_print_string ppf "[||]"
  else
    pp_mat_gen
      ~pp_open:pp_ocaml_open_mat
      ~pp_close:pp_ocaml_close_mat
      ~pp_end_row:pp_ocaml_end_row_mat
      ~pp_end_col:pp_end_col_semi_space
      pp_el
      ppf
      mat

let pp_ofmat ppf mat = pp_omat ppf pp_float_el mat
let pp_ocmat ppf mat = pp_omat ppf pp_complex_el mat
let pp_oimat ppf mat = pp_omat ppf pp_print_int32 mat
