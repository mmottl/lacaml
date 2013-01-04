/* File: fold2_col.c

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

#include "lacaml_macros.h"

CAMLprim value NAME(
  value vN,
  value vOFSX, value vINCX, value vX,
  value vOFSY, value vINCY, value vY)
{
  CAMLparam2(vX, vY);

  int GET_INT(N),
      GET_INT(INCX),
      GET_INT(INCY);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  NUMBER *start1, *last1, *start2, acc = INIT;

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX > 0) {
    start1 = X_data;
    last1 = start1 + N*INCX;
  } else {
    start1 = X_data - (N - 1)*INCX;
    last1 = X_data + INCX;
  };

  if (INCY > 0) start2 = Y_data;
  else start2 = Y_data - (N - 1)*INCY;

  while (start1 != last1) {
    NUMBER x = *start1,
           y = *start2;

    FUNC(acc, x, y);

    start1 += INCX;
    start2 += INCY;
  };

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(COPY_NUMBER(acc));
}

CAMLprim value BC_NAME(value *argv, int __unused argn)
{
  return NAME(argv[0], argv[1], argv[2],
              argv[3], argv[4], argv[5], argv[6]);
}


#undef NAME
#undef INIT
#undef FUNC
