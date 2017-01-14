/* File: fold_col.c

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
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "lacaml_macros.h"

CAMLprim value NAME(value vN, value vOFSX, value vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N),
          GET_INT(INCX);

  VEC_PARAMS(X);

  NUMBER *start, *last, acc = INIT;

#ifdef DECLARE_EXTRA
  DECLARE_EXTRA;
#undef DECLARE_EXTRA
#endif

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX > 0) {
    start = X_data;
    last = start + N*INCX;
  }
  else {
    start = X_data - (N - 1)*INCX;
    last = X_data + INCX;
  };

#ifdef INIT_HAVE_LOCK
  INIT_HAVE_LOCK;
#undef INIT_HAVE_LOCK
#endif

  while (start != last) {
    NUMBER x = *start;
    FUNC(acc, x);
    start += INCX;
  };

#ifdef FINISH_HAVE_LOCK
  FINISH_HAVE_LOCK;
#undef FINISH_HAVE_LOCK
#endif

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(COPY_NUMBER(acc));
}

#undef NAME
#undef INIT
#undef FUNC
