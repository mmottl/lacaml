/* File: vec_SD.h

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

#include <math.h>
#include "lacaml_macros.h"

CAMLprim value LFUN(linspace_stub)(value vY, double va, double vb, intnat vN)
{
  CAMLparam1(vY);
  integer i, GET_INT(N);
  REAL GET_DOUBLE(a);
  REAL GET_DOUBLE(b);
  REAL h = (b - a)/(N - 1), x = a;
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

CAMLprim value LFUN(linspace_stub_bc)(value vY, value va, value vb, value vN)
{
  return
    LFUN(linspace_stub)(
        vY,
        Double_val(va),
        Double_val(vb),
        Int_val(vN));
}


#ifndef LACAML_DOUBLE
extern float exp10f(float);
#define myexp10 exp10f
#else
extern double exp10(double);
#define myexp10 exp10
#endif

CAMLprim value LFUN(logspace_stub)(
    value vY, double va, double vb, double vbase, intnat vN)
{
  CAMLparam1(vY);
  integer i, GET_INT(N);
  REAL GET_DOUBLE(a);
  REAL GET_DOUBLE(b);
  double GET_DOUBLE(base);
  REAL h = (b - a)/(N - 1), x = a;
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
      *Y_data = myexp10(x);
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

CAMLprim value LFUN(logspace_stub_bc)(
    value vY, value va, value vb, value vbase, value vN)
{
  return
    LFUN(logspace_stub)(
        vY,
        Double_val(va),
        Double_val(vb),
        Double_val(vbase),
        Int_val(vN));
}


extern REAL FUN(nrm2)(integer *N, REAL *X, integer *INCX);

extern REAL FUN(dot)(
  integer *N,
  REAL *X, integer *INCX,
  REAL *Y, integer *INCY);

CAMLprim double LFUN(sqr_nrm2_stub)(
  value vSTABLE, intnat vN, intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N), GET_INT(INCX);
  doublereal res;

  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  if (Bool_val(vSTABLE)) {
    res = FUN(nrm2)(&N, X_data, &INCX);
    res *= res;
  } else res = FUN(dot)(&N, X_data, &INCX, X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturnT(double, res);
}

CAMLprim value LFUN(sqr_nrm2_stub_bc)(
    value vSTABLE, value vN, value vOFSX, value vINCX, value vX)
{
  return
    caml_copy_double(
        LFUN(sqr_nrm2_stub)(
          vSTABLE,
          Int_val(vN),
          Int_val(vOFSX),
          Int_val(vINCX),
          vX));
}

#define NAME LFUN(max_stub)
#define BC_NAME LFUN(max_stub_bc)
#define INIT -INFINITY
#define FUNC(acc, x) acc = SDMATHH(fmax)(acc, x)
#include "fold_col.h"

#define NAME LFUN(min_stub)
#define BC_NAME LFUN(min_stub_bc)
#define INIT INFINITY
#define FUNC(acc, x) acc = SDMATHH(fmin)(acc, x)
#include "fold_col.h"

#define NAME LFUN(sum_vec_stub)
#define BC_NAME LFUN(sum_vec_stub_bc)
#define INIT 0.0
#define FUNC(acc, x) acc += x
#include "fold_col.h"

#define NAME LFUN(prod_stub)
#define BC_NAME LFUN(prod_stub_bc)
#define INIT 1.0
#define FUNC(acc, x) acc *= x
#include "fold_col.h"

extern double LFUN(dot_stub)(
  intnat vN,
  intnat vOFSY, intnat vINCY, value vY,
  intnat vOFSX, intnat vINCX, value vX);

CAMLprim double LFUN(ssqr_zero_stub)(
  intnat vN, intnat vOFSX, intnat vINCX, value vX)
{
  return LFUN(dot_stub(vN, vOFSX, vINCX, vX, vOFSX, vINCX, vX));
}

CAMLprim value LFUN(ssqr_zero_stub_bc)(
    value vN, value vOFSX, value vINCX, value vX)
{
  return
    caml_copy_double(
        LFUN(ssqr_zero_stub)(
          Int_val(vN),
          Int_val(vOFSX),
          Int_val(vINCX),
          vX));
}

CAMLprim double LFUN(ssqr_stub)(
  intnat vN,
  double vC,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N),
          GET_INT(INCX);

  VEC_PARAMS(X);

  REAL *start, *last;
  REAL acc = 0.0;
  REAL GET_DOUBLE(C);

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) {
      REAL diff = X_data[i] - C;
      acc += diff * diff;
    }
  else {
    if (INCX > 0) {
      start = X_data;
      last = start + N*INCX;
    }
    else {
      start = X_data - (N - 1)*INCX;
      last = X_data + INCX;
    }

    while (start != last) {
      REAL diff = *start - C;
      acc += diff * diff;
      start += INCX;
    }
  }

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturnT(double, acc);
}

CAMLprim value LFUN(ssqr_stub_bc)(
    value vN, value vC, value vOFSX, value vINCX, value vX)
{
  return
    caml_copy_double(
        LFUN(ssqr_stub)(
          Int_val(vN),
          Double_val(vC),
          Int_val(vOFSX),
          Int_val(vINCX),
          vX));
}

/* Unary vector operations */

#define NAME LFUN(neg_stub)
#define BC_NAME LFUN(neg_stub_bc)
#define FUNC(dst, x) *dst = - x
#include "vec_map.h"

#define NAME LFUN(reci_stub)
#define BC_NAME LFUN(reci_stub_bc)
#define FUNC(dst, x) *dst = 1.0 / x
#include "vec_map.h"

#define NAME LFUN(abs_stub)
#define BC_NAME LFUN(abs_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(fabs)(x)
#include "vec_map.h"

#define NAME LFUN(signum_stub)
#define BC_NAME LFUN(signum_stub_bc)
#define FUNC(dst, x) *dst = (x > 0.0) ? 1.0 : (x < 0.0) ? -1.0 : x;
#include "vec_map.h"

#define NAME LFUN(sqr_stub)
#define BC_NAME LFUN(sqr_stub_bc)
#define FUNC(dst, x) *dst = x * x
#include "vec_map.h"

#define NAME LFUN(sqrt_stub)
#define BC_NAME LFUN(sqrt_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sqrt)(x)
#include "vec_map.h"

#define NAME LFUN(cbrt_stub)
#define BC_NAME LFUN(cbrt_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cbrt)(x)
#include "vec_map.h"

#define NAME LFUN(exp_stub)
#define BC_NAME LFUN(exp_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(exp)(x)
#include "vec_map.h"

#define NAME LFUN(exp2_stub)
#define BC_NAME LFUN(exp2_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(exp2)(x)
#include "vec_map.h"

#define NAME LFUN(expm1_stub)
#define BC_NAME LFUN(expm1_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(expm1)(x)
#include "vec_map.h"

#define NAME LFUN(log_stub)
#define BC_NAME LFUN(log_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log)(x)
#include "vec_map.h"

#define NAME LFUN(log10_stub)
#define BC_NAME LFUN(log10_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log10)(x)
#include "vec_map.h"

#define NAME LFUN(log2_stub)
#define BC_NAME LFUN(log2_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log2)(x)
#include "vec_map.h"

#define NAME LFUN(log1p_stub)
#define BC_NAME LFUN(log1p_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log1p)(x)
#include "vec_map.h"

#define NAME LFUN(sin_stub)
#define BC_NAME LFUN(sin_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sin)(x)
#include "vec_map.h"

#define NAME LFUN(cos_stub)
#define BC_NAME LFUN(cos_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cos)(x)
#include "vec_map.h"

#define NAME LFUN(tan_stub)
#define BC_NAME LFUN(tan_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(tan)(x)
#include "vec_map.h"

#define NAME LFUN(asin_stub)
#define BC_NAME LFUN(asin_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(asin)(x)
#include "vec_map.h"

#define NAME LFUN(acos_stub)
#define BC_NAME LFUN(acos_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(acos)(x)
#include "vec_map.h"

#define NAME LFUN(atan_stub)
#define BC_NAME LFUN(atan_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(atan)(x)
#include "vec_map.h"

#define NAME LFUN(sinh_stub)
#define BC_NAME LFUN(sinh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sinh)(x)
#include "vec_map.h"

#define NAME LFUN(cosh_stub)
#define BC_NAME LFUN(cosh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cosh)(x)
#include "vec_map.h"

#define NAME LFUN(tanh_stub)
#define BC_NAME LFUN(tanh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(tanh)(x)
#include "vec_map.h"

#define NAME LFUN(asinh_stub)
#define BC_NAME LFUN(asinh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(asinh)(x)
#include "vec_map.h"

#define NAME LFUN(acosh_stub)
#define BC_NAME LFUN(acosh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(acosh)(x)
#include "vec_map.h"

#define NAME LFUN(atanh_stub)
#define BC_NAME LFUN(atanh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(atanh)(x)
#include "vec_map.h"

#define NAME LFUN(floor_stub)
#define BC_NAME LFUN(floor_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(floor)(x)
#include "vec_map.h"

#define NAME LFUN(ceil_stub)
#define BC_NAME LFUN(ceil_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(ceil)(x)
#include "vec_map.h"

#define NAME LFUN(round_stub)
#define BC_NAME LFUN(round_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(round)(x)
#include "vec_map.h"

#define NAME LFUN(trunc_stub)
#define BC_NAME LFUN(trunc_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(trunc)(x)
#include "vec_map.h"

#define NAME LFUN(erf_stub)
#define BC_NAME LFUN(erf_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(erf)(x)
#include "vec_map.h"

#define NAME LFUN(erfc_stub)
#define BC_NAME LFUN(erfc_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(erfc)(x)
#include "vec_map.h"

#define NAME LFUN(logistic_stub)
#define BC_NAME LFUN(logistic_stub_bc)
#define FUNC(dst, x) *dst = 1.0 / (1.0 + SDMATHH(exp)(-x))
#include "vec_map.h"

#define NAME LFUN(relu_stub)
#define BC_NAME LFUN(relu_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(fmax)(x, 0)
#include "vec_map.h"

#define NAME LFUN(softplus_stub)
#define BC_NAME LFUN(softplus_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log1p)(SDMATHH(exp)(x))
#include "vec_map.h"

#define NAME LFUN(softsign_stub)
#define BC_NAME LFUN(softsign_stub_bc)
#define FUNC(dst, x) *dst = x / (1 + SDMATHH(fabs)(x))
#include "vec_map.h"


/* Binary vector operations */

#define NAME LFUN(add_stub)
#define BC_NAME LFUN(add_stub_bc)
#define FUNC(dst, x, y) *dst = x + y
#include "vec_combine.h"

#define NAME LFUN(sub_stub)
#define BC_NAME LFUN(sub_stub_bc)
#define FUNC(dst, x, y) *dst = x - y
#include "vec_combine.h"

#define NAME LFUN(mul_stub)
#define BC_NAME LFUN(mul_stub_bc)
#define FUNC(dst, x, y) *dst = x*y
#include "vec_combine.h"

#define NAME LFUN(div_stub)
#define BC_NAME LFUN(div_stub_bc)
#define FUNC(dst, x, y) *dst = x/y
#include "vec_combine.h"

#define NAME LFUN(pow_stub)
#define BC_NAME LFUN(pow_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(pow)(x, y)
#include "vec_combine.h"

#define NAME LFUN(atan2_stub)
#define BC_NAME LFUN(atan2_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(atan2)(x, y)
#include "vec_combine.h"

#define NAME LFUN(hypot_stub)
#define BC_NAME LFUN(hypot_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(hypot)(x, y)
#include "vec_combine.h"

#define NAME LFUN(min2_stub)
#define BC_NAME LFUN(min2_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(fmin)(x, y)
#include "vec_combine.h"

#define NAME LFUN(max2_stub)
#define BC_NAME LFUN(max2_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(fmax)(x, y)
#include "vec_combine.h"


/* Ternary matrix operations */

#define NAME LFUN(zpxy_stub)
#define BC_NAME LFUN(zpxy_stub_bc)
# ifdef FP_FAST_FMA
#  define FUNC(dst, x, y) *dst = SDMATHH(fma)(x, y, *dst)
# else
#  define FUNC(dst, x, y) *dst += x*y
# endif
#include "vec_combine.h"

#define NAME LFUN(zmxy_stub)
#define BC_NAME LFUN(zmxy_stub_bc)
#define FUNC(dst, x, y) *dst -= x*y
#include "vec_combine.h"


/* Unary vector operations yielding floats */

#define NAME LFUN(log_sum_exp_vec_stub)
#define BC_NAME LFUN(log_sum_exp_vec_stub_bc)
#define DECLARE_EXTRA NUMBER x_max = -INFINITY
#define INIT_HAVE_LOCK \
  x_max = LFUN(max_stub_blocking)(N, X_data, INCX, x_max)
#define INIT 0.0
#define FUNC(acc, x) acc += SDMATHH(exp)(x - x_max)
#define FINISH_HAVE_LOCK acc = SDMATHH(log)(acc) + x_max
#include "fold_col.h"


/* Binary vector operations yielding floats */

#define NAME LFUN(ssqr_diff_stub)
#define BC_NAME LFUN(ssqr_diff_stub_bc)
#define INIT 0.0
# ifdef FP_FAST_FMA
#  define FUNC(acc, x, y) x -= y; acc = SDMATHH(fma)(x, x, acc)
# else
#  define FUNC(acc, x, y) x -= y; x *= x; acc += x
# endif
#include "fold2_col.h"


/* Misc operations */

/* Since executing the (small) callback may dominate the running time,
 * specialize the function when the order is the usual one on floats.
 * In this case the callback is not used. */

/* NaN are put last (greater than anything) to ensure the algo termination.
   If both a and b are NaN, return false (consider NaN equal for this). */
#define NAN_LAST(a, b, SORT)                            \
  (isnan(b) ? (!isnan(a)) : (!isnan(a) && (SORT)))

#define NAME LFUN(sort_incr)
#define BC_NAME LFUN(sort_incr_bc)
#define NAME_PERM LFUN(sort_incr_perm)
#define BC_NAME_PERM LFUN(sort_incr_perm_bc)
#define OCAML_SORT_LT(a, b) NAN_LAST(a, b, a < b)
#include "vec_sort.h"

#define NAME LFUN(sort_decr)
#define BC_NAME LFUN(sort_decr_bc)
#define NAME_PERM LFUN(sort_decr_perm)
#define BC_NAME_PERM LFUN(sort_decr_perm_bc)
#define OCAML_SORT_LT(a, b) NAN_LAST(a, b, a > b)
#include "vec_sort.h"

#define NAME LFUN(sort)
#define BC_NAME LFUN(sort_bc)
#define NAME_PERM LFUN(sort_perm)
#define BC_NAME_PERM LFUN(sort_perm_bc)
#define OCAML_SORT_LT(a, b)                                     \
  NAN_LAST(a, b, (va = caml_copy_double(a),                     \
                  vb = caml_copy_double(b),                     \
                  Int_val(caml_callback2(vCMP, va, vb)) < 0))
#define OCAML_SORT_CALLBACK
#include "vec_sort.h"
#undef OCAML_SORT_CALLBACK
