/* File: vec_CZ.h

   Copyright © 2003-

   Markus Mottl <markus.mottl@gmail.com>

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
*/

#include <math.h>

#define LACAML_COMPLEX
#include "lacaml_macros.h"

static COMPLEX LACAML_COMPLEX_INF = {INFINITY, INFINITY};
static COMPLEX LACAML_COMPLEX_NEG_INF = {-INFINITY, -INFINITY};

CAMLprim value LFUN(linspace_stub)(value vY, value va, value vb, intnat vN) {
  CAMLparam1(vY);
  integer i, GET_INT(N);
  REAL ar = Double_field(va, 0), ai = Double_field(va, 1), N1 = N - 1.,
       hr = (Double_field(vb, 0) - ar) / N1,
       hi = (Double_field(vb, 1) - ai) / N1, xr = ar, xi = ai;
  VEC_PARAMS1(Y);

  caml_enter_blocking_section(); /* Allow other threads */

  for (i = 1; i <= N; i++) {
    Y_data->r = xr;
    Y_data->i = xi;
    Y_data++;
    xr = ar + i * hr;
    xi = ai + i * hi;
  }

  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(linspace_stub_bc)(value vY, value va, value vb, value vN) {
  return LFUN(linspace_stub)(vY, va, vb, Int_val(vN));
}

#ifndef LACAML_DOUBLE
extern float expf(float);
extern float exp10f(float);
extern float exp2f(float);
#define myexp expf
#define myexp10 exp10f
#define myexp2 exp2f
#else
extern double exp(double);
extern double exp10(double);
extern double exp2(double);
#define myexp exp
#define myexp10 exp10
#define myexp2 exp2
#endif

CAMLprim value LFUN(logspace_stub)(value vY, value va, value vb, double vbase,
                                   intnat vN) {
  CAMLparam1(vY);
  integer i, GET_INT(N);
  REAL ar = Double_field(va, 0), ai = Double_field(va, 1), N1 = N - 1.,
       hr = (Double_field(vb, 0) - ar) / N1,
       hi = (Double_field(vb, 1) - ai) / N1, xr = ar, xi = ai;
  double GET_DOUBLE(base);
  VEC_PARAMS1(Y);

  caml_enter_blocking_section(); /* Allow other threads */

  if (base == 2.0)
    for (i = 1; i <= N; i++) {
      Y_data->r = myexp2(xr);
      Y_data->i = myexp2(xi);
      Y_data++;
      xr = ar + i * hr;
      xi = ai + i * hi;
    }
  else if (base == 10.0)
    for (i = 1; i <= N; i++) {
      Y_data->r = myexp10(xr);
      Y_data->i = myexp10(xi);
      Y_data++;
      xr = ar + i * hr;
      xi = ai + i * hi;
    }
  else if (base == 2.7182818284590452353602874713526625L)
    for (i = 1; i <= N; i++) {
      Y_data->r = myexp(xr);
      Y_data->i = myexp(xi);
      Y_data++;
      xr = ar + i * hr;
      xi = ai + i * hi;
    }
  else {
    double log_base = log(base);
    for (i = 1; i <= N; i++) {
      Y_data->r = myexp(xr * log_base);
      Y_data->i = myexp(xi * log_base);
      Y_data++;
      xr = ar + i * hr;
      xi = ai + i * hi;
    }
  }

  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(logspace_stub_bc)(value vY, value va, value vb, value vbase,
                                      value vN) {
  return LFUN(logspace_stub)(vY, va, vb, Double_val(vbase), Int_val(vN));
}

extern real scnrm2_(integer *N, complex *X, integer *INCX);
extern doublereal dznrm2_(integer *N, doublecomplex *X, integer *INCX);

extern COMPLEX DOTC(integer *N, COMPLEX *X, integer *INCX, COMPLEX *Y,
                    integer *INCY);

CAMLprim value LFUN(sqr_nrm2_stub)(value vSTABLE, intnat vN, intnat vOFSX,
                                   intnat vINCX, value vX) {
  CAMLparam1(vX);

  integer GET_INT(N), GET_INT(INCX);
  REAL res;

  VEC_PARAMS(X);

  int stable = Bool_val(vSTABLE);
  //
  caml_enter_blocking_section(); /* Allow other threads */
  if (stable) {
#ifndef LACAML_DOUBLE
    res = scnrm2_(&N, X_data, &INCX);
#else
    res = dznrm2_(&N, X_data, &INCX);
#endif
    res *= res;
  } else {
    COMPLEX cres = DOTC(&N, X_data, &INCX, X_data, &INCX);
    res = cres.r;
  }
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(caml_copy_double(res));
}

CAMLprim value LFUN(sqr_nrm2_stub_bc)(value vSTABLE, value vN, value vOFSX,
                                      value vINCX, value vX) {
  return caml_copy_double(LFUN(sqr_nrm2_stub)(
      vSTABLE, Int_val(vN), Int_val(vOFSX), Int_val(vINCX), vX));
}

#define NAME LFUN(max_stub)
#define BC_NAME LFUN(max_stub_bc)
#define INIT LACAML_COMPLEX_NEG_INF
#define DECLARE_EXTRA REAL acc_big = 0., acc_nrm = 1., x_big, x_nrm, q, r, i
#define FUNC(acc, x)                                                           \
  r = x.r;                                                                     \
  i = x.i;                                                                     \
  if (r < 0.)                                                                  \
    r = -r;                                                                    \
  if (i < 0.)                                                                  \
    i = -i;                                                                    \
  if (r >= i) {                                                                \
    if (r == 0.)                                                               \
      continue;                                                                \
    x_big = r;                                                                 \
    q = i / r;                                                                 \
    x_nrm = 1. + q * q;                                                        \
  } else /* r < i or NaN */ {                                                  \
    if (i == 0.)                                                               \
      continue;                                                                \
    x_big = i;                                                                 \
    q = r / i;                                                                 \
    x_nrm = 1. + q * q;                                                        \
  }                                                                            \
  q = x_big / acc_big;                                                         \
  if (q * q * x_nrm > acc_nrm) {                                               \
    acc = x;                                                                   \
    acc_big = x_big;                                                           \
    acc_nrm = x_nrm;                                                           \
  }
#include "fold_col.h"

#define NAME LFUN(min_stub)
#define BC_NAME LFUN(min_stub_bc)
#define INIT LACAML_COMPLEX_INF
#define DECLARE_EXTRA                                                          \
  REAL acc_big = INFINITY, acc_nrm = 1., x_big, x_nrm, q, r, i
#define FUNC(acc, x)                                                           \
  r = x.r;                                                                     \
  i = x.i;                                                                     \
  if (r < 0.)                                                                  \
    r = -r;                                                                    \
  if (i < 0.)                                                                  \
    i = -i;                                                                    \
  if (r >= i) {                                                                \
    if (r == 0.)                                                               \
      continue;                                                                \
    x_big = r;                                                                 \
    q = i / r;                                                                 \
    x_nrm = 1. + q * q;                                                        \
  } else /* r < i or NaN */ {                                                  \
    if (i == 0.)                                                               \
      continue;                                                                \
    x_big = i;                                                                 \
    q = r / i;                                                                 \
    x_nrm = 1. + q * q;                                                        \
  }                                                                            \
  q = x_big / acc_big;                                                         \
  if (q * q * x_nrm < acc_nrm) {                                               \
    acc = x;                                                                   \
    acc_big = x_big;                                                           \
    acc_nrm = x_nrm;                                                           \
  }
#include "fold_col.h"

#define NAME LFUN(sum_vec_stub)
#define BC_NAME LFUN(sum_vec_stub_bc)
#define INIT {0.0, 0.0}
#define FUNC(acc, x)                                                           \
  acc.r += x.r;                                                                \
  acc.i += x.i
#include "fold_col.h"

#define NAME LFUN(prod_stub)
#define BC_NAME LFUN(prod_stub_bc)
#define INIT {1.0, 1.0}
#define FUNC(acc, x)                                                           \
  acc.r = acc.r * x.r - acc.i * x.i;                                           \
  acc.i = acc.r * x.i + acc.i * x.r
#include "fold_col.h"

extern value LFUN(dotu_stub)(value vN, value vOFSY, value vINCY, value vY,
                             value vOFSX, value vINCX, value vX);

CAMLprim value LFUN(ssqr_zero_stub)(intnat vN, intnat vOFSX, intnat vINCX,
                                    value vX) {
  return LFUN(dotu_stub(vN, vOFSX, vINCX, vX, vOFSX, vINCX, vX));
}

CAMLprim value LFUN(ssqr_zero_stub_bc)(value vN, value vOFSX, value vINCX,
                                       value vX) {
  return LFUN(ssqr_zero_stub)(Int_val(vN), Int_val(vOFSX), Int_val(vINCX), vX);
}

CAMLprim value LFUN(ssqr_stub)(intnat vN, value vC, intnat vOFSX, intnat vINCX,
                               value vX) {
  CAMLparam1(vX);

  integer GET_INT(N), GET_INT(INCX);

  VEC_PARAMS(X);

  COMPLEX *start, *last;
  COMPLEX acc = {0.0, 0.0};
  REAL cr = Double_field(vC, 0);
  REAL ci = Double_field(vC, 1);
  REAL diffr;
  REAL diffi;

  caml_enter_blocking_section(); /* Allow other threads */

  if (INCX == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) {
      COMPLEX *xp = X_data + i;
      diffr = xp->r - cr;
      diffi = xp->i - ci;
      acc.r += diffr * diffr - diffi * diffi;
      acc.i += 2 * diffr * diffi;
    }
  else {
    if (INCX > 0) {
      start = X_data;
      last = start + N * INCX;
    } else {
      start = X_data - (N - 1) * INCX;
      last = X_data + INCX;
    };

    while (start != last) {
      diffr = start->r - cr;
      diffi = start->i - ci;
      acc.r += diffr * diffr - diffi * diffi;
      acc.i += 2 * diffr * diffi;
      start += INCX;
    }
  }

  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(copy_two_doubles(acc.r, acc.i));
}

CAMLprim value LFUN(ssqr_stub_bc)(value vN, value vC, value vOFSX, value vINCX,
                                  value vX) {
  return LFUN(ssqr_stub)(Int_val(vN), vC, Int_val(vOFSX), Int_val(vINCX), vX);
}

#define NAME LFUN(neg_stub)
#define BC_NAME LFUN(neg_stub_bc)
#define FUNC(dst, x)                                                           \
  dst->r = -x.r;                                                               \
  dst->i = -x.i
#include "vec_map.h"

#define NAME LFUN(reci_stub)
#define BC_NAME LFUN(reci_stub_bc)
#define FUNC(dst, x)                                                           \
  if (abs(x.r) >= abs(x.i)) {                                                  \
    REAL r = x.i / x.r;                                                        \
    REAL d = x.r + r * x.i;                                                    \
    dst->r = 1 / d;                                                            \
    dst->i = -r / d;                                                           \
  } else {                                                                     \
    REAL r = x.r / x.i;                                                        \
    REAL d = x.i + r * x.r;                                                    \
    dst->r = r / d;                                                            \
    dst->i = -1 / d;                                                           \
  }
#include "vec_map.h"

#define NAME LFUN(add_stub)
#define BC_NAME LFUN(add_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r = x.r + y.r;                                                          \
  dst->i = x.i + y.i
#include "vec_combine.h"

#define NAME LFUN(sub_stub)
#define BC_NAME LFUN(sub_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r = x.r - y.r;                                                          \
  dst->i = x.i - y.i
#include "vec_combine.h"

#define NAME LFUN(mul_stub)
#define BC_NAME LFUN(mul_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r = x.r * y.r - x.i * y.i;                                              \
  dst->i = x.r * y.i + x.i * y.r
#include "vec_combine.h"

#define NAME LFUN(div_stub)
#define BC_NAME LFUN(div_stub_bc)
#define FUNC(dst, x, y)                                                        \
  REAL xr = x.r, xi = x.i, yr = y.r, yi = y.i;                                 \
  if (FABS(yr) >= FABS(yi)) {                                                  \
    REAL r = yi / yr, d = yr + r * yi;                                         \
    dst->r = (xr + r * xi) / d;                                                \
    dst->i = (xi - r * xr) / d;                                                \
  } else {                                                                     \
    REAL r = yr / yi, d = yi + r * yr;                                         \
    dst->r = (r * xr + xi) / d;                                                \
    dst->i = (r * xi - xr) / d;                                                \
  }
#include "vec_combine.h"

#define NAME LFUN(zpxy_stub)
#define BC_NAME LFUN(zpxy_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r += x.r * y.r - x.i * y.i;                                             \
  dst->i += x.r * y.i + x.i * y.r
#include "vec_combine.h"

#define NAME LFUN(zmxy_stub)
#define BC_NAME LFUN(zmxy_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r -= x.r * y.r - x.i * y.i;                                             \
  dst->i -= x.r * y.i + x.i * y.r
#include "vec_combine.h"

#define NAME LFUN(ssqr_diff_stub)
#define BC_NAME LFUN(ssqr_diff_stub_bc)
#define INIT {0.0, 0.0}
#define FUNC(acc, x, y)                                                        \
  x.r -= y.r;                                                                  \
  x.i -= y.i;                                                                  \
  acc.r += (x.r - x.i) * (x.r + x.i);                                          \
  acc.i += 2 * x.r * x.i
#include "fold2_col.h"

/* Since executing the (small) callback may dominate the running time,
 * specialize the function when the order is the lexicographical one
 * on complex numbers.  In this case the callback is not used. */

/* NaN are put last (greater than anything) to ensure the algo termination.
   If both a and b are NaN, return false (consider NaN equal for this). */
#define ANY_NAN(x) (isnan(x.r) || isnan(x.i))
#define NAN_LAST(a, b, SORT)                                                   \
  (ANY_NAN(b) ? (!ANY_NAN(a)) : (!ANY_NAN(a) && (SORT)))

#define NAME LFUN(sort_incr)
#define BC_NAME LFUN(sort_incr_bc)
#define NAME_PERM LFUN(sort_incr_perm)
#define BC_NAME_PERM LFUN(sort_incr_perm_bc)
#define OCAML_SORT_LT(a, b)                                                    \
  NAN_LAST(a, b, a.r < b.r || (a.r == b.r && a.i < b.i))
#include "vec_sort.h"

#define NAME LFUN(sort_decr)
#define BC_NAME LFUN(sort_decr_bc)
#define NAME_PERM LFUN(sort_decr_perm)
#define BC_NAME_PERM LFUN(sort_decr_perm_bc)
#define OCAML_SORT_LT(a, b)                                                    \
  NAN_LAST(a, b, a.r > b.r || (a.r == b.r && a.i > b.i))
#include "vec_sort.h"

#define NAME LFUN(sort)
#define BC_NAME LFUN(sort_bc)
#define NAME_PERM LFUN(sort_perm)
#define BC_NAME_PERM LFUN(sort_perm_bc)
#define OCAML_SORT_LT(a, b)                                                    \
  NAN_LAST(a, b,                                                               \
           (va = copy_two_doubles(a.r, a.i), vb = copy_two_doubles(b.r, b.i),  \
            Int_val(caml_callback2(vCMP, va, vb)) < 0))
#define OCAML_SORT_CALLBACK
#include "vec_sort.h"
#undef OCAML_SORT_CALLBACK
