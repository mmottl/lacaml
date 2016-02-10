/* File: mat_SD_c.c

   Copyright (C) 2015-

     Markus Mottl
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
*/

#include <math.h>
#include "lacaml_macros.h"

#define NAME LFUN(neg_mat_stub)
#define BC_NAME LFUN(neg_mat_stub_bc)
#define FUNC(dst, x) *dst = - x
#include "mat_map.c"

#define NAME LFUN(reci_mat_stub)
#define BC_NAME LFUN(reci_mat_stub_bc)
#define FUNC(dst, x) *dst = 1 / x
#include "mat_map.c"

#define NAME LFUN(sqr_mat_stub)
#define BC_NAME LFUN(sqr_mat_stub_bc)
#define FUNC(dst, x) *dst = x * x
#include "mat_map.c"

#define NAME LFUN(sqrt_mat_stub)
#define BC_NAME LFUN(sqrt_mat_stub_bc)
#define FUNC(dst, x) *dst = sqrt(x)
#include "mat_map.c"

#define NAME LFUN(exp_mat_stub)
#define BC_NAME LFUN(exp_mat_stub_bc)
#define FUNC(dst, x) *dst = exp(x)
#include "mat_map.c"

#define NAME LFUN(log_mat_stub)
#define BC_NAME LFUN(log_mat_stub_bc)
#define FUNC(dst, x) *dst = log(x)
#include "mat_map.c"

#define NAME LFUN(sin_mat_stub)
#define BC_NAME LFUN(sin_mat_stub_bc)
#define FUNC(dst, x) *dst = sin(x)
#include "mat_map.c"

#define NAME LFUN(cos_mat_stub)
#define BC_NAME LFUN(cos_mat_stub_bc)
#define FUNC(dst, x) *dst = cos(x)
#include "mat_map.c"

#define NAME LFUN(tan_mat_stub)
#define BC_NAME LFUN(tan_mat_stub_bc)
#define FUNC(dst, x) *dst = tan(x)
#include "mat_map.c"

#define NAME LFUN(tanh_mat_stub)
#define BC_NAME LFUN(tanh_mat_stub_bc)
#define FUNC(dst, x) *dst = tanh(x)
#include "mat_map.c"

#define NAME LFUN(add_mat_stub)
#define BC_NAME LFUN(add_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x + y
#include "mat_combine.c"

#define NAME LFUN(sub_mat_stub)
#define BC_NAME LFUN(sub_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x - y
#include "mat_combine.c"

#define NAME LFUN(mul_mat_stub)
#define BC_NAME LFUN(mul_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x*y
#include "mat_combine.c"

#define NAME LFUN(div_mat_stub)
#define BC_NAME LFUN(div_mat_stub_bc)
#define FUNC(dst, x, y) *dst = x/y
#include "mat_combine.c"

#define NAME LFUN(cpab_stub)
#define BC_NAME LFUN(cpab_stub_bc)
#define FUNC(dst, x, y) *dst += x*y
#include "mat_combine.c"

#define NAME LFUN(cmab_stub)
#define BC_NAME LFUN(cmab_stub_bc)
#define FUNC(dst, x, y) *dst -= x*y
#include "mat_combine.c"

#define NAME LFUN(ssqr_diff_mat_stub)
#define BC_NAME LFUN(ssqr_diff_mat_stub_bc)
#define INIT 0.0
#define FUNC(acc, x, y) x -= y; x *= x; acc += x
#include "mat_fold2.c"
