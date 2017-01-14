/* File: mat_fold.c

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

#include "lacaml_macros.h"

CAMLprim value NAME(value vM, value vN, value vAR, value vAC, value vA)
{
  CAMLparam1(vA);

  integer GET_INT(M), GET_INT(N);
  NUMBER acc = INIT;

  MAT_PARAMS(A);

  if (M > 0) {
    NUMBER *last = A_data + N*rows_A;
    integer diff_A = rows_A - M;

#ifdef DECLARE_EXTRA
    DECLARE_EXTRA;
#undef DECLARE_EXTRA
#endif

    caml_enter_blocking_section();  /* Allow other threads */

#ifdef INIT_HAVE_LOCK
    INIT_HAVE_LOCK;
#undef INIT_HAVE_LOCK
#endif

    while (A_data != last) {
      NUMBER *A_col_last = A_data + M;

      while (A_data != A_col_last) {
        NUMBER x = *A_data;
        FUNC(acc, x);
        A_data++;
      }

      A_data += diff_A;
    }

#ifdef FINISH_HAVE_LOCK
    FINISH_HAVE_LOCK;
#undef FINISH_HAVE_LOCK
#endif

    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(COPY_NUMBER(acc));
}

#undef NAME
#undef INIT
#undef FUNC
