/* File: lacaml_macros.h

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef LACAML_MACROS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include "f2c.h"
#include "utils_c.h"

#define STR_NX(name, arg) name##arg
#define STR(name, arg) STR_NX(name, arg)

/* Defines precision-dependent macros */
#ifndef LACAML_DOUBLE           /* Single precision */

#define REAL real
#define COMPLEX complex
#define FABS fabsf

#ifndef LACAML_COMPLEX          /* Real number */
#define SDMATHH(name) name##f
#define FUN(name) s##name##_
#define FUN2(prefix,name) prefix##s##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_S##name
#else                           /* Complex number */
#define FUN(name) c##name##_
#define FUN2(prefix,name) prefix##c##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_C##name
#endif  /* LACAML_COMPLEX */

#else                           /* Double precision */

#define REAL doublereal
#define COMPLEX doublecomplex
#define FABS fabs

#ifndef LACAML_COMPLEX          /* Real number */
#define SDMATHH(name) name
#define FUN(name) d##name##_
#define FUN2(prefix,name) prefix##d##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_D##name
#else                           /* Complex number */
#define FUN(name) z##name##_
#define FUN2(prefix,name) prefix##z##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_Z##name
#endif  /* LACAML_COMPLEX */

#endif  /* LACAML_DOUBLE */

/* Real vs. imaginery */
#ifndef LACAML_COMPLEX          /* Real number */
#define NUMBER REAL
#define CAMLreturnNUMBER(X) CAMLreturnT(double, X)
#define COPY_NUMBER(X) caml_copy_double(X)
#define NUMBER_ZERO 0
#define NUMBER_ONE 1
#define NUMBER_MINUS_ONE (-1)
#define NUMBER_EQUAL(X, Y) (X) == (Y)
#define INIT_NUMBER(name) name = v##name
#define vNUMBER double
#define NUMBER_val(X) Double_val(X)
#define DOTU FUN(dot)
#define ADD_NUMBER(X, Y) (X + Y)
#define SUB_NUMBER(X, Y) (X - Y)
#define MUL_NUMBER(X, Y) (X * Y)
#define MUL_NUMBERP(X, Y) (*X * *Y)
#define NEG_NUMBER(X) (-X)
#else                           /* Complex number */
#define NUMBER COMPLEX
#define CAMLreturnNUMBER(X) CAMLreturn(copy_two_doubles(X.r, X.i))
#define COPY_NUMBER(X) X
#define NUMBER_ZERO { 0, 0 }
#define NUMBER_ONE { 1, 0 }
#define NUMBER_MINUS_ONE { -1, 0 }
#define NUMBER_EQUAL(X, Y) (X).r == (Y).r && (X).i == (Y).i
#define INIT_NUMBER(name) \
  name.r = Double_field(v##name, 0); \
  name.i = Double_field(v##name, 1)
#define vNUMBER value
#define NUMBER_val(X) X
#define DOTU FUN(lacaml_dotu_wrap)
#define DOTC FUN(lacaml_dotc_wrap)
#define ADD_NUMBER(X, Y) ((NUMBER) { X.r + Y.r, X.i + Y.i })
#define SUB_NUMBER(X, Y) ((NUMBER) { X.r - Y.r, X.i - Y.i })
#define MUL_NUMBER(X, Y) \
  ((NUMBER) { X.r * Y.r - X.i * Y.i, X.r * Y.i + X.i * Y.r })
#define MUL_NUMBERP(X, Y) \
  ((NUMBER) { X->r * Y->r - X->i * Y->i, X->r * Y->i + X->i * Y->r })
#define NEG_NUMBER(X) ((NUMBER) { -X.r, -X.i })
#define COMLEX_CONJ(X) ((NUMBER) { X.r, -X.i })
#endif

/* Fetch boolean parameters */
#define GET_BOOL(V) V = Bool_val(v##V)

/* Fetch char parameters */
#define GET_CHAR(V) V = Int_val(v##V)

/* Fetch integer parameters */
#define GET_INT(V) V = v##V

/* Fetch double parameters */
#define GET_DOUBLE(V) V = v##V

/* Fetch matrix parameters from bigarray */
#define MAT_PARAMS(M) \
  struct caml_ba_array *big_##M = Caml_ba_array_val(v##M); \
  intnat *dims_##M = big_##M->dim; \
  integer M##R = v##M##R; \
  integer M##C = v##M##C; \
  integer rows_##M = *dims_##M++; \
  CAMLunused integer cols_##M = *dims_##M; \
  NUMBER *M##_data = ((NUMBER *) big_##M->data) + M##R + rows_##M*(M##C - 1) - 1

/* Fetch vector parameters from bigarray */
#define VEC_PARAMS(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  CAMLunused integer dim_##V = *big_##V->dim; \
  NUMBER *V##_data = ((NUMBER *) big_##V->data) + (vOFS##V - 1)

/* Fetch vector parameters from real bigarray */
#define RVEC_PARAMS(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  CAMLunused integer dim_##V = *big_##V->dim; \
  REAL *V##_data = ((REAL *) big_##V->data) + (vOFS##V - 1)

/* Fetch vector parameters from bigarray with offset 1 */
#define VEC_PARAMS1(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  CAMLunused integer dim_##V = *big_##V->dim; \
  NUMBER *V##_data = big_##V->data

/* Fetch vector parameters from bigarray with offset 1 */
#define RVEC_PARAMS1(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  CAMLunused integer dim_##V = *big_##V->dim; \
  REAL *V##_data = big_##V->data

/* Fetch vector parameters from integer bigarray */
#define INT_VEC_PARAMS(V) \
  struct caml_ba_array *big_##V = Caml_ba_array_val(v##V); \
  CAMLunused integer dim_##V = *big_##V->dim; \
  integer *V##_data = big_##V->data

/* Split an integer couple (int * int) into two ints */
#define INT_COUPLE(V) \
  integer V##1 = Int_val(Field(v##V, 0)); \
  integer V##2 = Int_val(Field(v##V, 1))

#endif  /* LACAML_MACROS */
