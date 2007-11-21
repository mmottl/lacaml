/* File: vec_combine.c

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

/* $Id: vec_combine.c,v 1.13 2006/01/18 15:03:40 mottl Exp $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include "f2c.h"

#include "lacaml_macros.h"

CAMLprim value NAME(
  value vN,
  value vOFSZ, value vINCZ, value vZ,
  value vOFSX, value vINCX, value vX,
  value vOFSY, value vINCY, value vY)
{
  CAMLparam3(vX, vY, vZ);

  int GET_INT(N),
      GET_INT(INCX),
      GET_INT(INCY),
      GET_INT(INCZ);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);
  VEC_PARAMS(Z);

  NUMBER *start_src1, *last_src1, *start_src2, *dst;

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX > 0) {
    start_src1 = X_data;
    last_src1 = start_src1 + N*INCX;
  }
  else {
    start_src1 = X_data - (N - 1)*INCX;
    last_src1 = X_data + INCX;
  };

  if (INCY > 0) start_src2 = Y_data;
  else start_src2 = Y_data - (N - 1)*INCY;

  if (INCZ > 0) dst = Z_data;
  else dst = Z_data - (N - 1)*INCZ;

  while (start_src1 != last_src1) {
    NUMBER x = *start_src1;
    NUMBER y = *start_src2;
    FUNC(dst, x, y);
    start_src1 += INCX;
    start_src2 += INCY;
    dst += INCZ;
  };

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int argn)
{
  return
    NAME(argv[0], argv[1], argv[2],argv[3], argv[4], argv[5],
         argv[6], argv[7], argv[8], argv[9]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
