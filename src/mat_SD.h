/* File: mat_SD.h

   Copyright (C) 2015-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

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

/* Unary matrix operations */

#define NAME LFUN(neg_mat_stub)
#define BC_NAME LFUN(neg_mat_stub_bc)
#define FUNC(dst, x) *dst = - x
#include "mat_map.h"

#define NAME LFUN(reci_mat_stub)
#define BC_NAME LFUN(reci_mat_stub_bc)
#define FUNC(dst, x) *dst = 1.0 / x
#include "mat_map.h"

#define NAME LFUN(abs_mat_stub)
#define BC_NAME LFUN(abs_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(fabs)(x)
#include "mat_map.h"

#define NAME LFUN(signum_mat_stub)
#define BC_NAME LFUN(signum_mat_stub_bc)
#define FUNC(dst, x) *dst = (x > 0.0) ? 1.0 : (x < 0.0) ? -1.0 : x;
#include "mat_map.h"

#define NAME LFUN(sqr_mat_stub)
#define BC_NAME LFUN(sqr_mat_stub_bc)
#define FUNC(dst, x) *dst = x * x
#include "mat_map.h"

#define NAME LFUN(sqrt_mat_stub)
#define BC_NAME LFUN(sqrt_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sqrt)(x)
#include "mat_map.h"

#define NAME LFUN(cbrt_mat_stub)
#define BC_NAME LFUN(cbrt_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cbrt)(x)
#include "mat_map.h"

#define NAME LFUN(exp_mat_stub)
#define BC_NAME LFUN(exp_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(exp)(x)
#include "mat_map.h"

#define NAME LFUN(exp2_mat_stub)
#define BC_NAME LFUN(exp2_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(exp2)(x)
#include "mat_map.h"

#define NAME LFUN(expm1_mat_stub)
#define BC_NAME LFUN(expm1_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(expm1)(x)
#include "mat_map.h"

#define NAME LFUN(log_mat_stub)
#define BC_NAME LFUN(log_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log)(x)
#include "mat_map.h"

#define NAME LFUN(log10_mat_stub)
#define BC_NAME LFUN(log10_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log10)(x)
#include "mat_map.h"

#define NAME LFUN(log2_mat_stub)
#define BC_NAME LFUN(log2_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log2)(x)
#include "mat_map.h"

#define NAME LFUN(log1p_mat_stub)
#define BC_NAME LFUN(log1p_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log1p)(x)
#include "mat_map.h"

#define NAME LFUN(sin_mat_stub)
#define BC_NAME LFUN(sin_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sin)(x)
#include "mat_map.h"

#define NAME LFUN(cos_mat_stub)
#define BC_NAME LFUN(cos_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cos)(x)
#include "mat_map.h"

#define NAME LFUN(tan_mat_stub)
#define BC_NAME LFUN(tan_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(tan)(x)
#include "mat_map.h"

#define NAME LFUN(asin_mat_stub)
#define BC_NAME LFUN(asin_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(asin)(x)
#include "mat_map.h"

#define NAME LFUN(acos_mat_stub)
#define BC_NAME LFUN(acos_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(acos)(x)
#include "mat_map.h"

#define NAME LFUN(atan_mat_stub)
#define BC_NAME LFUN(atan_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(atan)(x)
#include "mat_map.h"

#define NAME LFUN(sinh_mat_stub)
#define BC_NAME LFUN(sinh_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(sinh)(x)
#include "mat_map.h"

#define NAME LFUN(cosh_mat_stub)
#define BC_NAME LFUN(cosh_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(cosh)(x)
#include "mat_map.h"

#define NAME LFUN(tanh_mat_stub)
#define BC_NAME LFUN(tanh_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(tanh)(x)
#include "mat_map.h"

#define NAME LFUN(asinh_mat_stub)
#define BC_NAME LFUN(asinh_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(asinh)(x)
#include "mat_map.h"

#define NAME LFUN(acosh_mat_stub)
#define BC_NAME LFUN(acosh_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(acosh)(x)
#include "mat_map.h"

#define NAME LFUN(atanh_mat_stub)
#define BC_NAME LFUN(atanh_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(atanh)(x)
#include "mat_map.h"

#define NAME LFUN(floor_mat_stub)
#define BC_NAME LFUN(floor_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(floor)(x)
#include "mat_map.h"

#define NAME LFUN(ceil_mat_stub)
#define BC_NAME LFUN(ceil_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(ceil)(x)
#include "mat_map.h"

#define NAME LFUN(round_mat_stub)
#define BC_NAME LFUN(round_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(round)(x)
#include "mat_map.h"

#define NAME LFUN(trunc_mat_stub)
#define BC_NAME LFUN(trunc_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(trunc)(x)
#include "mat_map.h"

#define NAME LFUN(erf_mat_stub)
#define BC_NAME LFUN(erf_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(erf)(x)
#include "mat_map.h"

#define NAME LFUN(erfc_mat_stub)
#define BC_NAME LFUN(erfc_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(erfc)(x)
#include "mat_map.h"

#define NAME LFUN(logistic_mat_stub)
#define BC_NAME LFUN(logistic_mat_stub_bc)
#define FUNC(dst, x) *dst = 1.0 / (1.0 + SDMATHH(exp)(-x))
#include "mat_map.h"

#define NAME LFUN(relu_mat_stub)
#define BC_NAME LFUN(relu_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(fmax)(x, 0)
#include "mat_map.h"

#define NAME LFUN(softplus_mat_stub)
#define BC_NAME LFUN(softplus_mat_stub_bc)
#define FUNC(dst, x) *dst = SDMATHH(log1p)(SDMATHH(exp)(x))
#include "mat_map.h"

#define NAME LFUN(softsign_mat_stub)
#define BC_NAME LFUN(softsign_mat_stub_bc)
#define FUNC(dst, x) *dst = x / (1 + SDMATHH(fabs)(x))
#include "mat_map.h"


/* Binary matrix operations */

#define NAME LFUN(add_mat_stub)
#define BC_NAME LFUN(add_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x + y
#include "mat_combine.h"

#define NAME LFUN(sub_mat_stub)
#define BC_NAME LFUN(sub_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x - y
#include "mat_combine.h"

#define NAME LFUN(mul_mat_stub)
#define BC_NAME LFUN(mul_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x*y
#include "mat_combine.h"

#define NAME LFUN(div_mat_stub)
#define BC_NAME LFUN(div_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x/y
#include "mat_combine.h"

#define NAME LFUN(pow_mat_stub)
#define BC_NAME LFUN(pow_mat_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(pow)(x, y)
#include "mat_combine.h"

#define NAME LFUN(atan2_mat_stub)
#define BC_NAME LFUN(atan2_mat_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(atan2)(x, y)
#include "mat_combine.h"

#define NAME LFUN(hypot_mat_stub)
#define BC_NAME LFUN(hypot_mat_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(hypot)(x, y)
#include "mat_combine.h"

#define NAME LFUN(min2_mat_stub)
#define BC_NAME LFUN(min2_mat_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(fmin)(x, y)
#include "mat_combine.h"

#define NAME LFUN(max2_mat_stub)
#define BC_NAME LFUN(max2_mat_stub_bc)
#define FUNC(dst, x, y) *dst = SDMATHH(fmax)(x, y)
#include "mat_combine.h"


/* Ternary matrix operations */

#define NAME LFUN(cpab_stub)
#define BC_NAME LFUN(cpab_stub_bc)
#ifdef FP_FAST_FMA
# define FUNC(dst, x, y) *dst = SDMATHH(fma)(x, y, *dst)
#else
# define FUNC(dst, x, y) *dst += x*y
#endif
#include "mat_combine.h"

#define NAME LFUN(cmab_stub)
#define BC_NAME LFUN(cmab_stub_bc)
#define FUNC(dst, x, y) *dst -= x*y
#include "mat_combine.h"


/* Unary matrix operations yielding floats */

#define NAME LFUN(max_el_mat_stub)
#define BC_NAME LFUN(max_el_mat_stub_bc)
#define INIT 0.0
#define FUNC(acc, x) acc = SDMATHH(fmax)(x, acc)
#include "mat_fold.h"

#define NAME LFUN(log_sum_exp_mat_stub)
#define BC_NAME LFUN(log_sum_exp_mat_stub_bc)
#define DECLARE_EXTRA NUMBER x_max = -INFINITY
#define INIT_HAVE_LOCK  \
  x_max = \
    LFUN(max_el_mat_stub_blocking)(PKIND, PINIT, M, N, A_data, rows_A, x_max)
#define INIT 0.0
#define FUNC(acc, x) acc += SDMATHH(exp)(x - x_max)
#define FINISH_HAVE_LOCK acc = SDMATHH(log)(acc) + x_max
#include "mat_fold.h"


/* Binary matrix operations yielding floats */

#define NAME LFUN(ssqr_diff_mat_stub)
#define BC_NAME LFUN(ssqr_diff_mat_stub_bc)
#define INIT 0.0
# ifdef FP_FAST_FMA
#  define FUNC(acc, x, y) x -= y; acc = SDMATHH(fma)(x, x, acc)
# else
#  define FUNC(acc, x, y) x -= y; x *= x; acc += x
# endif
#include "mat_fold2.h"

#define NAME LFUN(sum_prod_mat_stub)
#define BC_NAME LFUN(sum_prod_mat_stub_bc)
#define INIT 0.0
# ifdef FP_FAST_FMA
#  define FUNC(acc, x, y) acc = SDMATHH(fma)(x, y, acc)
# else
#  define FUNC(acc, x, y) acc += x*y
# endif
#include "mat_fold2.h"
