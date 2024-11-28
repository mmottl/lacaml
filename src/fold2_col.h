/* File: fold2_col.h

   Copyright © 2001-

   Markus Mottl <markus.mottl@gmail.com>

   Christophe Troestler <Christophe.Troestler@umons.ac.be>

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

#include "lacaml_macros.h"

CAMLprim vNUMBER NAME(intnat vN, intnat vOFSX, intnat vINCX, value vX,
                      intnat vOFSY, intnat vINCY, value vY) {
  CAMLparam2(vX, vY);

  integer GET_INT(N), GET_INT(INCX), GET_INT(INCY);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  NUMBER *start1, *last1, *start2, acc = INIT;

  caml_enter_blocking_section(); /* Allow other threads */

  if (INCX > 0) {
    start1 = X_data;
    last1 = start1 + N * INCX;
  } else {
    start1 = X_data - (N - 1) * INCX;
    last1 = X_data + INCX;
  };

  if (INCY > 0)
    start2 = Y_data;
  else
    start2 = Y_data - (N - 1) * INCY;

  if (INCX == 1 && INCY == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) {
      NUMBER x = X_data[i], y = Y_data[i];
      FUNC(acc, x, y);
    }
  else
    while (start1 != last1) {
      NUMBER x = *start1, y = *start2;
      FUNC(acc, x, y);
      start1 += INCX;
      start2 += INCY;
    }

  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturnNUMBER(acc);
}

CAMLprim value BC_NAME(value *argv, int __unused argn) {
  return COPY_NUMBER(NAME(Int_val(argv[0]), Int_val(argv[1]), Int_val(argv[2]),
                          argv[3], Int_val(argv[4]), Int_val(argv[5]),
                          argv[6]));
}

#undef NAME
#undef BC_NAME
#undef INIT
#undef FUNC
