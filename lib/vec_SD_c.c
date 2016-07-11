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

CAMLprim value LFUN(linspace_stub)(value vY, value va, value vb, value vN)
{
  CAMLparam1(vY);
  integer i, GET_INT(N);
  double a = Double_val(va),
         h = (Double_val(vb) - a)/(N - 1),
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

#ifdef __FreeBSD__
static double exp10(double x){
	  return exp(log(10)*x);
}
#else
extern double exp10(double x);
#endif

CAMLprim value LFUN(logspace_stub)(value vY, value va, value vb,
                                   value vbase, value vN)
{
  CAMLparam1(vY);
  integer i, GET_INT(N);
  double a = Double_val(va),
         h = (Double_val(vb) - a)/(N - 1),
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

  integer GET_INT(N), GET_INT(INCX);
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
#define INIT -INFINITY
#define FUNC(acc, x) acc = SDMATHH(fmax)(acc, x)
#include "fold_col.c"

#define NAME LFUN(min_stub)
#define INIT INFINITY
#define FUNC(acc, x) acc = SDMATHH(fmin)(acc, x)
#include "fold_col.c"

#define NAME LFUN(sum_vec_stub)
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

  integer GET_INT(N),
          GET_INT(INCX);

  VEC_PARAMS(X);

  REAL *start, *last;
  REAL acc = 0.0;
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

/* Unary vector operations */

#define NAME LFUN(neg_stub)
#define BC_NAME LFUN(neg_stub_bc)
#define FUNC(dst, x) *dst = - x
#include "vec_map.c"

#define NAME LFUN(reci_stub)
#define BC_NAME LFUN(reci_stub_bc)
#define FUNC(dst, x) *dst = 1.0 / x
#include "vec_map.c"

#define NAME LFUN(abs_stub)
#define BC_NAME LFUN(abs_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(fabs)(x)
#include "vec_map.c"

#define NAME LFUN(signum_stub)
#define BC_NAME LFUN(signum_stub_bc)
#define FUNC(dst, x) *dst = (x > 0.0) ? 1.0 : (x < 0.0) ? -1.0 : x;
#include "vec_map.c"

#define NAME LFUN(sqr_stub)
#define BC_NAME LFUN(sqr_stub_bc)
#define FUNC(dst, x) *dst = x * x
#include "vec_map.c"

#define NAME LFUN(sqrt_stub)
#define BC_NAME LFUN(sqrt_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sqrt)(x)
#include "vec_map.c"

#define NAME LFUN(cbrt_stub)
#define BC_NAME LFUN(cbrt_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cbrt)(x)
#include "vec_map.c"

#define NAME LFUN(exp_stub)
#define BC_NAME LFUN(exp_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(exp)(x)
#include "vec_map.c"

#define NAME LFUN(exp2_stub)
#define BC_NAME LFUN(exp2_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(exp2)(x)
#include "vec_map.c"

#define NAME LFUN(expm1_stub)
#define BC_NAME LFUN(expm1_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(expm1)(x)
#include "vec_map.c"

#define NAME LFUN(log_stub)
#define BC_NAME LFUN(log_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log)(x)
#include "vec_map.c"

#define NAME LFUN(log10_stub)
#define BC_NAME LFUN(log10_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log10)(x)
#include "vec_map.c"

#define NAME LFUN(log2_stub)
#define BC_NAME LFUN(log2_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log2)(x)
#include "vec_map.c"

#define NAME LFUN(log1p_stub)
#define BC_NAME LFUN(log1p_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log1p)(x)
#include "vec_map.c"

#define NAME LFUN(sin_stub)
#define BC_NAME LFUN(sin_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sin)(x)
#include "vec_map.c"

#define NAME LFUN(cos_stub)
#define BC_NAME LFUN(cos_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cos)(x)
#include "vec_map.c"

#define NAME LFUN(tan_stub)
#define BC_NAME LFUN(tan_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(tan)(x)
#include "vec_map.c"

#define NAME LFUN(asin_stub)
#define BC_NAME LFUN(asin_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(asin)(x)
#include "vec_map.c"

#define NAME LFUN(acos_stub)
#define BC_NAME LFUN(acos_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(acos)(x)
#include "vec_map.c"

#define NAME LFUN(atan_stub)
#define BC_NAME LFUN(atan_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(atan)(x)
#include "vec_map.c"

#define NAME LFUN(sinh_stub)
#define BC_NAME LFUN(sinh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sinh)(x)
#include "vec_map.c"

#define NAME LFUN(cosh_stub)
#define BC_NAME LFUN(cosh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cosh)(x)
#include "vec_map.c"

#define NAME LFUN(tanh_stub)
#define BC_NAME LFUN(tanh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(tanh)(x)
#include "vec_map.c"

#define NAME LFUN(asinh_stub)
#define BC_NAME LFUN(asinh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(asinh)(x)
#include "vec_map.c"

#define NAME LFUN(acosh_stub)
#define BC_NAME LFUN(acosh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(acosh)(x)
#include "vec_map.c"

#define NAME LFUN(atanh_stub)
#define BC_NAME LFUN(atanh_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(atanh)(x)
#include "vec_map.c"

#define NAME LFUN(floor_stub)
#define BC_NAME LFUN(floor_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(floor)(x)
#include "vec_map.c"

#define NAME LFUN(ceil_stub)
#define BC_NAME LFUN(ceil_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(ceil)(x)
#include "vec_map.c"

#define NAME LFUN(round_stub)
#define BC_NAME LFUN(round_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(round)(x)
#include "vec_map.c"

#define NAME LFUN(trunc_stub)
#define BC_NAME LFUN(trunc_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(trunc)(x)
#include "vec_map.c"

#define NAME LFUN(erf_stub)
#define BC_NAME LFUN(erf_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(erf)(x)
#include "vec_map.c"

#define NAME LFUN(erfc_stub)
#define BC_NAME LFUN(erfc_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(erfc)(x)
#include "vec_map.c"

#define NAME LFUN(logistic_stub)
#define BC_NAME LFUN(logistic_stub_bc)
#define FUNC(dst, x) *dst = 1.0 / (1.0 + SDMATHH(exp)(-x))
#include "vec_map.c"

#define NAME LFUN(relu_stub)
#define BC_NAME LFUN(relu_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(fmax)(x, 0)
#include "vec_map.c"

#define NAME LFUN(softplus_stub)
#define BC_NAME LFUN(softplus_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log1p)(SDMATHH(exp)(x))
#include "vec_map.c"

#define NAME LFUN(softsign_stub)
#define BC_NAME LFUN(softsign_stub_bc)
#define FUNC(dst, x) *dst = x / (1 + SDMATHH(fabs)(x))
#include "vec_map.c"


/* Binary vector operations */

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

#define NAME LFUN(pow_stub)
#define BC_NAME LFUN(pow_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(pow)(x, y)
#include "vec_combine.c"

#define NAME LFUN(atan2_stub)
#define BC_NAME LFUN(atan2_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(atan2)(x, y)
#include "vec_combine.c"

#define NAME LFUN(hypot_stub)
#define BC_NAME LFUN(hypot_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(hypot)(x, y)
#include "vec_combine.c"

#define NAME LFUN(min2_stub)
#define BC_NAME LFUN(min2_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(fmin)(x, y)
#include "vec_combine.c"

#define NAME LFUN(max2_stub)
#define BC_NAME LFUN(max2_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(fmax)(x, y)
#include "vec_combine.c"


/* Ternary matrix operations */

#define NAME LFUN(zpxy_stub)
#define BC_NAME LFUN(zpxy_stub_bc)
# ifdef FP_FAST_FMA
#  define FUNC(dst, x, y) *dst = SDMATHH(fma)(x, y, *dst)
# else
#  define FUNC(dst, x, y) *dst += x*y
# endif
#include "vec_combine.c"

#define NAME LFUN(zmxy_stub)
#define BC_NAME LFUN(zmxy_stub_bc)
#define FUNC(dst, x, y) *dst -= x*y
#include "vec_combine.c"


/* Unary vector operations yielding floats */

#define NAME LFUN(log_sum_exp_vec_stub)
#define DECLARE_EXTRA NUMBER x_max = -INFINITY, *max_start
#define INIT_HAVE_LOCK \
  for (max_start = start; max_start != last; max_start += INCX) \
    x_max = SDMATHH(fmax)(x_max, *max_start);
#define INIT 0.0
#define FUNC(acc, x) acc += SDMATHH(exp)(x - x_max)
#define FINISH_HAVE_LOCK acc = SDMATHH(log)(acc) + x_max
#include "fold_col.c"


/* Binary vector operations yielding floats */

#define NAME LFUN(ssqr_diff_stub)
#define BC_NAME LFUN(ssqr_diff_stub_bc)
#define INIT 0.0
# ifdef FP_FAST_FMA
#  define FUNC(acc, x, y) x -= y; acc = SDMATHH(fma)(x, x, acc)
# else
#  define FUNC(acc, x, y) x -= y; x *= x; acc += x
# endif
#include "fold2_col.c"


/* Misc operations */

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
