/* File: mat_CZ.h

   Copyright © 2015-

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

#define NAME LFUN(neg_mat_stub)
#define BC_NAME LFUN(neg_mat_stub_bc)
#define FUNC(dst, x)                                                           \
  dst->r = -x.r;                                                               \
  dst->i = -x.i
#include "mat_map.h"

#define NAME LFUN(reci_mat_stub)
#define BC_NAME LFUN(reci_mat_stub_bc)
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
#include "mat_map.h"

#define NAME LFUN(add_mat_stub)
#define BC_NAME LFUN(add_mat_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r = x.r + y.r;                                                          \
  dst->i = x.i + y.i
#include "mat_combine.h"

#define NAME LFUN(sub_mat_stub)
#define BC_NAME LFUN(sub_mat_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r = x.r - y.r;                                                          \
  dst->i = x.i - y.i
#include "mat_combine.h"

#define NAME LFUN(mul_mat_stub)
#define BC_NAME LFUN(mul_mat_stub_bc)
#define FUNC(dst, x, y)                                                        \
  dst->r = x.r * y.r - x.i * y.i;                                              \
  dst->i = x.r * y.i + x.i * y.r
#include "mat_combine.h"

#define NAME LFUN(div_mat_stub)
#define BC_NAME LFUN(div_mat_stub_bc)
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
#include "mat_combine.h"

#define NAME LFUN(ssqr_diff_mat_stub)
#define BC_NAME LFUN(ssqr_diff_mat_stub_bc)
#define INIT {0.0, 0.0}
#define FUNC(acc, x, y)                                                        \
  x.r -= y.r;                                                                  \
  x.i -= y.i;                                                                  \
  acc.r += (x.r - x.i) * (x.r + x.i);                                          \
  acc.i += 2 * x.r * x.i
#include "mat_fold2.h"
