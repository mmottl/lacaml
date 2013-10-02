/* File: vec_SD_c.c

   Copyright (C) 2013-

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

CAMLprim value LFUN(fill_stub)(
  value vN, value vOFSX, value vINCX, value vX, value vA)
{
  CAMLparam1(vX);

  int GET_INT(N), GET_INT(INCX);

  CREATE_NUMBER(A);
  VEC_PARAMS(X);

  NUMBER *start, *last;

  INIT_NUMBER(A);

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX > 0) {
    start = X_data;
    last = start + N*INCX;
  }
  else {
    start = X_data - (N - 1)*INCX;
    last = X_data + INCX;
  };

  while (start != last) { *start = A; start += INCX; };

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}
