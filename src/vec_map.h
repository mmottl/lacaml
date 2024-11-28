/* File: vec_map.h

   Copyright © 2009-

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

CAMLprim value NAME(intnat vN, intnat vOFSY, intnat vINCY, value vY,
                    intnat vOFSX, intnat vINCX, value vX) {
  CAMLparam2(vX, vY);

  integer GET_INT(N), GET_INT(INCX), GET_INT(INCY);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  NUMBER *start1, *last1, *dst;

  caml_enter_blocking_section(); /* Allow other threads */

  if (INCX == 1 && INCY == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) {
      NUMBER x = X_data[i];
      NUMBER *dst = Y_data + i;
      FUNC(dst, x);
    }
  else {
    if (INCX > 0) {
      start1 = X_data;
      last1 = start1 + N * INCX;
    } else {
      start1 = X_data - (N - 1) * INCX;
      last1 = X_data + INCX;
    };

    if (INCY > 0)
      dst = Y_data;
    else
      dst = Y_data - (N - 1) * INCY;

    while (start1 != last1) {
      NUMBER x = *start1;
      FUNC(dst, x);
      start1 += INCX;
      dst += INCY;
    };
  }

  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int __unused argn) {
  return NAME(Int_val(argv[0]), Int_val(argv[1]), Int_val(argv[2]), argv[3],
              Int_val(argv[4]), Int_val(argv[5]), argv[6]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
