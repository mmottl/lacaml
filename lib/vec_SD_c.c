/* File: vec_SD_c.c

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
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

/* $Id: vec_SD_c.c,v 1.24 2006/01/18 15:03:40 mottl Exp $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/signals.h>

#include <math.h>

#include "f2c.h"

#include "lacaml_macros.h"

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

#define COPY_NUMBER(acc) caml_copy_double(acc)

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

#define NAME LFUN(ssqr_zero_stub)
#define INIT 0.0
#define FUNC(acc, x) acc += x*x
#include "fold_col.c"

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

CAMLprim value LFUN(fold_stub)(
  value vINIT,
  value vN,
  value vOFSX, value vINCX, value vX,
  value vClosure)
{
  CAMLparam2(vX, vClosure);
  CAMLlocal1(acc);

  int GET_INT(N),
      GET_INT(INCX);

  VEC_PARAMS(X);

  REAL *start, *last;
  acc = vINIT;

  if (INCX > 0) {
    start = X_data;
    last = start + N*INCX;
  }
  else {
    start = X_data - (N - 1)*INCX;
    last = X_data + INCX;
  };

  while (start != last) {
    value v_start = caml_copy_double(*start);
    acc = caml_callback2(vClosure, acc, v_start);
    start += INCX;
  };

  CAMLreturn(acc);
}

CAMLprim value LFUN(fold_stub_bc)(value *argv, int argn)
{
  return LFUN(fold_stub)(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

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
