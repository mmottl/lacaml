/* File: vec_combine.h

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

CAMLprim value NAME(intnat vN, intnat vOFSZ, intnat vINCZ, value vZ,
                    intnat vOFSX, intnat vINCX, value vX, intnat vOFSY,
                    intnat vINCY, value vY) {
  CAMLparam3(vX, vY, vZ);

  integer GET_INT(N), GET_INT(INCX), GET_INT(INCY), GET_INT(INCZ);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);
  VEC_PARAMS(Z);

  NUMBER *start_src1, *last_src1, *start_src2, *dst;

  caml_enter_blocking_section(); /* Allow other threads */

  if (INCX == 1 && INCY == 1 && INCZ == 1)
    /* NOTE: may improve SIMD optimization */
    for (int i = 0; i < N; i++) {
      NUMBER x = X_data[i];
      NUMBER y = Y_data[i];
      NUMBER *dst = Z_data + i;
      FUNC(dst, x, y);
    }
  else {
    if (INCX > 0) {
      start_src1 = X_data;
      last_src1 = start_src1 + N * INCX;
    } else {
      start_src1 = X_data - (N - 1) * INCX;
      last_src1 = X_data + INCX;
    };

    if (INCY > 0)
      start_src2 = Y_data;
    else
      start_src2 = Y_data - (N - 1) * INCY;

    if (INCZ > 0)
      dst = Z_data;
    else
      dst = Z_data - (N - 1) * INCZ;

    while (start_src1 != last_src1) {
      NUMBER x = *start_src1;
      NUMBER y = *start_src2;
      FUNC(dst, x, y);
      start_src1 += INCX;
      start_src2 += INCY;
      dst += INCZ;
    }
  }

  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int __unused argn) {
  return NAME(Int_val(argv[0]), Int_val(argv[1]), Int_val(argv[2]), argv[3],
              Int_val(argv[4]), Int_val(argv[5]), argv[6], Int_val(argv[7]),
              Int_val(argv[8]), argv[9]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
