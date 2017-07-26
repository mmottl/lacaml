/* File: vec_SD_c.c

   Copyright (C) 2013-

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

/* fill_vec */

CAMLprim value LFUN(fill_vec_stub)(
  value vN, value vOFSX, value vINCX, value vX, value vA)
{
  CAMLparam1(vX);

  integer GET_INT(N), GET_INT(INCX);

  NUMBER A;
  VEC_PARAMS(X);

  NUMBER *start, *last;

  INIT_NUMBER(A);

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) X_data[i] = A;
  else {
    if (INCX > 0) {
      start = X_data;
      last = start + N*INCX;
    } else {
      start = X_data - (N - 1)*INCX;
      last = X_data + INCX;
    };

    while (start != last) {
      *start = A;
      start += INCX;
    }
  }

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}


/* add_const_vec */

CAMLprim value LFUN(add_const_vec_stub)(
  value vC, value vN,
  value vOFSY, value vINCY, value vY,
  value vOFSX, value vINCX, value vX)
{
  CAMLparam2(vX, vY);

  NUMBER C;
  integer GET_INT(N), GET_INT(INCX), GET_INT(INCY);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  NUMBER *start_src, *last_src, *dst;
  INIT_NUMBER(C);

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX == 1 && INCY == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) {
      NUMBER src = X_data[i];
      Y_data[i] = ADD_NUMBER(src, C);
    }
  else {
    if (INCX > 0) {
      start_src = X_data;
      last_src = start_src + N*INCX;
    }
    else {
      start_src = X_data - (N - 1)*INCX;
      last_src = X_data + INCX;
    };

    if (INCY > 0) dst = Y_data;
    else dst = Y_data - (N - 1)*INCY;

    while (start_src != last_src) {
      NUMBER src = *start_src;
      *dst = ADD_NUMBER(src, C);
      start_src += INCX;
      dst += INCY;
    }
  }

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(add_const_vec_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(add_const_vec_stub)(
        argv[0], argv[1], argv[2],argv[3],
        argv[4], argv[5], argv[6], argv[7]);
}
