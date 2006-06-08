(* File: lacaml_io.mli

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

(* $Id: lacaml_io.mli,v 1.7 2005/11/03 17:21:01 mottl Exp $ *)

open Format
open Bigarray

(** {6 Generic matrix printing function} *)

val pp_mat_gen :
  ?pp_open : (formatter -> unit) ->
  ?pp_close : (formatter -> unit) ->
  ?pp_head : (formatter -> int -> unit) ->
  ?pp_foot : (formatter -> int -> unit) ->
  ?pp_end_row : (formatter -> int -> unit) ->
  ?pp_end_col : (formatter -> row : int -> col : int -> unit) ->
  ?pp_left : (formatter -> int -> unit) ->
  ?pp_right : (formatter -> int -> unit) ->
  ?pad : char option ->
  (formatter -> 'el -> unit) ->
  formatter ->
  ('el, 'a, fortran_layout) Array2.t
  -> unit
(** [pp_mat_gen
       ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
       ?pp_left ?pp_right ?pad pp_el ppf mat]

    Generic printing of matrices (two-dimensional bigarrays).

    [pp_open ppf] is called whenever printing of a matrix [mat]
    is started, [pp_close ppf] whenever printing is complete.
    These functions are not called when the matrix is empty.

    [pp_head other_ppf col] is used to print a header for column [col]
    in matrix [mat].  This header is right-aligned and eventually padded
    using [Some pad]-character to match the matrix rows in the column
    beneath.  The passed formatter [other_ppf] is not identical to [ppf]!

    [pp_foot other_ppf col] is used to print a footer for column [col]
    in matrix [mat].  It is similar to [pp_head col other_ppf] otherwise.

    [pp_end_row ppf row] is called on row number [row] and formatter
    [ppf] whenever the end of a row has been reached.

    [pp_end_col ppf ~row ~col] is called on the row number [row], column
    number [col] and formatter [ppf] whenever the element at this position
    has been printed and if it is not the last element in the row.

    [pp_left ppf row] is called on row number [row] and formatter
    [ppf] to print labels to the left of each row.  The labels are
    right-aligned within a virtual column.

    [pp_right ppf row] is called on row number [row] and formatter
    [ppf] to print labels to the right of each row.  The labels are
    left-aligned.

    The character [pad] is used to pad matrix elements for right-aligning
    them appropriately.  If it is set to [None], no alignment will
    be performed.

    [pp_el other_ppf el] is called on formatter [other_ppf] (not
    [ppf]!) and each matrix element.

    [ppf] is the formatter to which all output is finally printed.

    [mat] is the matrix to be printed.

    @param pp_open default = open standard pretty-printing box
    @param pp_close default = close standard pretty-printing box
    @param pp_head default = no default
    @param pp_foot default = no default
    @param pp_end_row default = print newline (within pretty-printing box)
    @param pp_end_col default = print space
    @param pp_left default = no default
    @param pad default = Some ' '
*)


(** {6 Default pretty-printers used by the other pretty-printing functions} *)

(** Type of references for default printers of elements *)
type 'el pp_el_default = (formatter -> 'el -> unit) ref

val pp_float_el_default : float pp_el_default
(** fprintf ppf "%G" el *)

val pp_complex_el_default : Complex.t pp_el_default
(** fprintf ppf "(%G, %Gi)" el.re el.im *)


(** {6 Pretty-printing in standard style} *)

(** Type of standard pretty-printers for column vectors *)
type ('el, 'elt) pp_vec =
  formatter ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit
(** [pp_vec ppf vec] prints a vector [vec] to formatter [ppf]
    using the defaults. *)

val pp_fvec : (float, 'elt) pp_vec
val pp_cvec : (Complex.t, 'elt) pp_vec
val pp_ivec : (int, 'elt) pp_vec
val pp_rfvec : (float, 'elt) pp_vec
val pp_rcvec : (Complex.t, 'elt) pp_vec
val pp_rivec : (int, 'elt) pp_vec

(** Type of standard pretty-printers for matrices *)
type ('el, 'elt) pp_mat =
  formatter ->
  ('el, 'elt, fortran_layout) Array2.t
  -> unit
(** [pp_mat ppf mat] prints a matrix [mat] to formatter [ppf] using the
    defaults. *)

val pp_fmat : (float, 'elt) pp_mat
val pp_cmat : (Complex.t, 'elt) pp_mat
val pp_imat : (int, 'elt) pp_mat


(** {7 Labeled pretty-printing} *)

(** {8 Vectors} *)

(** Type of pretty-printers for labeled vectors *)
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
(** [pp_labeled_vec ?pp_head ?pp_foot ?pp_left ?pp_right ?pad () ppf vec]
    prints vector [vec] to formatter [ppf] labeling the header using
    function [pp_head], the footer using [pp_foot], the left side (of
    rows for column vectors; of columns for row vectors) using [pp_left],
    and the right side using [pp_right].  A [pad]-option can be passed.

    For column vectors the labels on the left side are right-aligned
    while those on the right side are left-aligned.

    @param pp_head default = no default (= no printing)
    @param pp_foot default = no default (= no printing)
    @param pp_left default = [Some pp_print_int] for vector rows/cols
                             (= not in header/footer row/col)
    @param pp_right default = no default (= no printing)
*)

val pp_labeled_fvec : (float, 'elt) pp_labeled_vec
val pp_labeled_cvec : (Complex.t, 'elt) pp_labeled_vec
val pp_labeled_ivec : (int, 'elt) pp_labeled_vec
val pp_labeled_rfvec : (float, 'elt) pp_labeled_vec
val pp_labeled_rcvec : (Complex.t, 'elt) pp_labeled_vec
val pp_labeled_rivec : (int, 'elt) pp_labeled_vec

(** Type of pretty-printers for string labeled vectors *)
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
(** [pp_lvec ?print_head ?print_foot ?print_left ?print_right
      ?labels ?name ?pad () ppf vec]
    prints vector [vec] to formatter [ppf] labeling the header with [name]
    if provided and if [print_head] is true, and labeling the footer with
    [name] if [print_foot] is true.  The left side (of rows for column
    vectors; of columns for row vectors) is labeled with [labels] if
    provided and if [print_left] is true, and the right side is labeled
    with [labels] if [print_right] is true.  A [pad]-option can be passed.

    For columns vectors the labels on the left side are right-aligned
    while those on the right side are left-aligned.

    It is the duty of the user to make sure that the array containing
    the labels is sufficiently large for the given vector.

    @param print_head default = [true]
    @param print_foot default = [true]
    @param print_left default = [true]
    @param print_right default = [false]
    @param labels default = no default (= no printing)
    @param header default = no default (= no printing)
*)

val pp_lfvec : (float, 'elt) pp_lvec
val pp_lcvec : (Complex.t, 'elt) pp_lvec
val pp_livec : (int, 'elt) pp_lvec
val pp_rlfvec : (float, 'elt) pp_lvec
val pp_rlcvec : (Complex.t, 'elt) pp_lvec
val pp_rlivec : (int, 'elt) pp_lvec


(** {8 Matrices} *)

(** Type of pretty-printers for labeled matrices *)
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
(** [pp_labeled_mat ?pp_head ?pp_foot ?pp_left ?pp_right ?pad () ppf mat]
    prints a matrix [mat] to formatter [ppf] labeling the header using
    function [pp_head], the footer using [pp_foot], the left side of rows
    using [pp_left], and the right one using [pp_right].  A [pad]-option
    can be passed.

    If [None] is passed as argument for the default printers, the
    corresponding labels will not be printed.

    @param pp_head default = [Some pp_print_int]
    @param pp_foot default = [Some pp_print_int]
    @param pp_left default = [Some pp_print_int] for matrix rows
                             (= not in header/footer row)
    @param pp_right default = [Some pp_print_int] for matrix rows
                             (= not in header/footer row)
*)

val pp_labeled_fmat : (float, 'elt) pp_labeled_mat
val pp_labeled_cmat : (Complex.t, 'elt) pp_labeled_mat
val pp_labeled_imat : (int, 'elt) pp_labeled_mat

(** Type of pretty-printers for string labeled matrices *)
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
(** [pp_lmat ?print_head ?print_foot ?print_left ?print_right
      ?row_labels ?col_labels ?pad () ppf mat]
    prints a matrix [mat] to formatter [ppf] labeling the header with
    the column labels in [col_labels] if provided and if [print_head] is
    true, and labeling the footer with the column labels if [print_foot]
    is true.  The left side of rows is labeled with the row labels
    [row_labels] if provided and if [print_left] is true, and the right
    side of rows is labeled with the row labels if [print_right] is true.
    A [pad]-option can be passed.

    It is the duty of the user to make sure that the arrays containing the
    row- and column labels are sufficiently large for the given matrix.

    @param print_head default = [true]
    @param print_foot default = [true]
    @param print_left default = [true]
    @param print_right default = [true]
    @param row_labels default = no default (= no printing)
    @param col_labels default = no default (= no printing)
*)

val pp_lfmat : (float, 'elt) pp_lmat
val pp_lcmat : (Complex.t, 'elt) pp_lmat
val pp_limat : (int, 'elt) pp_lmat


(** {6 Pretty-printing in OCaml-style} *)

(** Type of pretty-printers for OCaml-vectors *)
type ('el, 'elt) pp_el_ovec =
  formatter ->
  (formatter -> 'el -> unit) ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit
(** [pp_el_ovec ppf pp_el vec] prints the vector [vec] to formatter
    [ppf] in OCaml-style using the element printer [pp_el]. *)

val pp_ovec : ('el, 'elt) pp_el_ovec
(** [pp_ovec ppf pp_el vec] prints the column vector [vec] to formatter
    [ppf] in OCaml-style using the element printer [pp_el]. *)

val pp_rovec : ('el, 'elt) pp_el_ovec
(** [pp_rovec ppf pp_el vec] prints the row vector [vec] to formatter
    [ppf] in OCaml-style using the element printer [pp_el]. *)

(** Type of pretty-printers for OCaml-vectors of a given element type *)
type ('el, 'elt) pp_ovec =
  formatter ->
  ('el, 'elt, fortran_layout) Array1.t
  -> unit
(** [pp_ovec ppf vec] prints the vector [vec] to formatter [ppf] in
    OCaml-style. *)

val pp_ofvec : (float, 'elt) pp_ovec
val pp_ocvec : (Complex.t, 'elt) pp_ovec
val pp_oivec : (int, 'elt) pp_ovec

val pp_rofvec : (float, 'elt) pp_ovec
val pp_rocvec : (Complex.t, 'elt) pp_ovec
val pp_roivec : (int, 'elt) pp_ovec

val pp_omat :
  formatter ->
  (formatter -> 'el -> unit) ->
  ('el, 'c, fortran_layout) Array2.t
  -> unit
(** [pp_omat ppf pp_el mat] prints matrix [mat] to formatter [ppf]
    in OCaml-style using the element printer [pp_el]. *)

(** Type of pretty-printers for OCaml-matrices of a given element type *)
type ('el, 'elt) pp_omat =
  formatter ->
  ('el, 'elt, fortran_layout) Array2.t
  -> unit
(** [pp_omat ppf mat] prints the matrix [mat] to formatter [ppf] in
    OCaml-style. *)

val pp_ofmat : (float, 'elt) pp_omat
val pp_ocmat : (Complex.t, 'elt) pp_omat
val pp_oimat : (int, 'elt) pp_omat