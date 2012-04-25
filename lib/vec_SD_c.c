/* File: vec_SD_c.c

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
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <math.h>
#include "lacaml_macros.h"
#include "f2c.h"

static REAL LACAML_INF = 1. / 0.;

CAMLprim value LFUN(linspace_stub)(value vY, value va, value vb, value vN)
{
  CAMLparam1(vY);
  int i, GET_INT(N);
  double a = Double_val(va),
         h = (Double_val(vb) - a)/(N - 1.),
         x = a;
  VEC_PARAMS1(Y);

  caml_enter_blocking_section();  /* Allow other threads */

  for (i = 1; i <= N; i++) {
    *Y_data = x;
    Y_data++;
    x = a + i * h;
  }

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

extern double exp(double);
extern double exp2(double);
extern double exp10(double);

CAMLprim value LFUN(logspace_stub)(value vY, value va, value vb,
                                   value vbase, value vN)
{
  CAMLparam1(vY);
  int i, GET_INT(N);
  double a = Double_val(va),
         h = (Double_val(vb) - a)/(N - 1.),
         base = Double_val(vbase),
         x = a;
  VEC_PARAMS1(Y);

  caml_enter_blocking_section();  /* Allow other threads */

  if (base == 2.0)
    for (i = 1; i <= N; i++) {
      *Y_data = exp2(x);
      Y_data++;
      x = a + i * h;
    }
  else if (base == 10.0)
    for (i = 1; i <= N; i++) {
      *Y_data = exp10(x);
      Y_data++;
      x = a + i * h;
    }
  else if (base == 2.7182818284590452353602874713526625L)
    for (i = 1; i <= N; i++) {
      *Y_data = exp(x);
      Y_data++;
      x = a + i * h;
    }
  else {
    double log_base = log(base);
    for (i = 1; i <= N; i++) {
      *Y_data = exp(x * log_base);
      Y_data++;
      x = a + i * h;
    }
  }

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

extern REAL FUN(nrm2)(integer *N, REAL *X, integer *INCX);

extern REAL FUN(dot)(
  integer *N,
  REAL *X, integer *INCX,
  REAL *Y, integer *INCY);

CAMLprim value LFUN(sqr_nrm2_stub)(
  value vSTABLE, value vN, value vOFSX, value vINCX, value vX)
{
  CAMLparam1(vX);

  int GET_INT(N), GET_INT(INCX);
  doublereal res;

  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  if (Bool_val(vSTABLE)) {
    res = FUN(nrm2)(&N, X_data, &INCX);
    res *= res;
  } else res = FUN(dot)(&N, X_data, &INCX, X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(caml_copy_double(res));
}

#define NAME LFUN(max_stub)
#define INIT -LACAML_INF
#define FUNC(acc, x) if (x > acc) acc = x
#include "fold_col.c"

#define NAME LFUN(min_stub)
#define INIT LACAML_INF
#define FUNC(acc, x) if (x < acc) acc = x
#include "fold_col.c"

#define NAME LFUN(sum_stub)
#define INIT 0.0
#define FUNC(acc, x) acc += x
#include "fold_col.c"

#define NAME LFUN(prod_stub)
#define INIT 1.0
#define FUNC(acc, x) acc *= x
#include "fold_col.c"

extern value LFUN(dot_stub)(
  value vN,
  value vOFSY, value vINCY, value vY,
  value vOFSX, value vINCX, value vX);

CAMLprim value LFUN(ssqr_zero_stub)(
  value vN, value vOFSX, value vINCX, value vX)
{
  return LFUN(dot_stub(vN, vOFSX, vINCX, vX, vOFSX, vINCX, vX));
}

CAMLprim value LFUN(ssqr_stub)(
  value vN,
  value vC,
  value vOFSX, value vINCX, value vX)
{
  CAMLparam1(vX);

  int GET_INT(N),
      GET_INT(INCX);

  VEC_PARAMS(X);

  REAL *start, *last;
  REAL acc = 0;
  REAL c = Double_val(vC);
  REAL diff;

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX > 0) {
    start = X_data;
    last = start + N*INCX;
  }
  else {
    start = X_data - (N - 1)*INCX;
    last = X_data + INCX;
  };

  while (start != last) {
    diff = *start - c;
    acc += diff * diff;
    start += INCX;
  };

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(caml_copy_double(acc));
}

#define NAME LFUN(sqr_stub)
#define BC_NAME LFUN(sqr_stub_bc)
#define FUNC(dst, x) *dst = x * x
#include "vec_map.c"

#define NAME LFUN(sqrt_stub)
#define BC_NAME LFUN(sqrt_stub_bc)
#define FUNC(dst, x) *dst = sqrt(x)
#include "vec_map.c"

#define NAME LFUN(neg_stub)
#define BC_NAME LFUN(neg_stub_bc)
#define FUNC(dst, x) *dst = - x
#include "vec_map.c"

#define NAME LFUN(add_stub)
#define BC_NAME LFUN(add_stub_bc)
#define FUNC(dst, x, y) *dst = x + y
#include "vec_combine.c"

#define NAME LFUN(sub_stub)
#define BC_NAME LFUN(sub_stub_bc)
#define FUNC(dst, x, y) *dst = x - y
#include "vec_combine.c"

#define NAME LFUN(mul_stub)
#define BC_NAME LFUN(mul_stub_bc)
#define FUNC(dst, x, y) *dst = x*y
#include "vec_combine.c"

#define NAME LFUN(div_stub)
#define BC_NAME LFUN(div_stub_bc)
#define FUNC(dst, x, y) *dst = x/y
#include "vec_combine.c"

#define NAME LFUN(ssqr_diff_stub)
#define BC_NAME LFUN(ssqr_diff_stub_bc)
#define INIT 0.0
#define FUNC(acc, x, y) x -= y; x *= x; acc += x
#include "fold2_col.c"

/* Since executing the (small) callback may dominate the running time,
 * specialize the function when the order is the usual one on floats.
 * In this case the callback is not used. */

/* NaN are put last (greater than anything) to ensure the algo termination.
   If both a and b are NaN, return false (consider NaN equal for this). */
#define NAN_LAST(a, b, SORT)                            \
  (isnan(b) ? (!isnan(a)) : (!isnan(a) && (SORT)))

#define NAME LFUN(sort_incr)
#define NAME_PERM LFUN(sort_incr_perm)
#define BC_NAME_PERM LFUN(sort_incr_perm_bc)
#define OCAML_SORT_LT(a, b) NAN_LAST(a, b, a < b)
#include "vec_sort.c"

#define NAME LFUN(sort_decr)
#define NAME_PERM LFUN(sort_decr_perm)
#define BC_NAME_PERM LFUN(sort_decr_perm_bc)
#define OCAML_SORT_LT(a, b) NAN_LAST(a, b, a > b)
#include "vec_sort.c"

#define NAME LFUN(sort)
#define NAME_PERM LFUN(sort_perm)
#define BC_NAME_PERM LFUN(sort_perm_bc)
#define OCAML_SORT_LT(a, b)                                     \
  NAN_LAST(a, b, (va = caml_copy_double(a),                     \
                  vb = caml_copy_double(b),                     \
                  Int_val(caml_callback2(vCMP, va, vb)) < 0))
#define OCAML_SORT_CALLBACK
#include "vec_sort.c"
#undef OCAML_SORT_CALLBACK
