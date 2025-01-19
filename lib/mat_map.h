/* File: mat_map.h

   Copyright © 2015-

   Markus Mottl <markus.mottl@gmail.com>

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

static inline void STR(NAME, _range)(integer N, NUMBER *A_data,
                                     NUMBER *B_data) {
  for (int i = 0; i < N; i++) {
    NUMBER a = A_data[i];
    NUMBER *dst = B_data + i;
    FUNC(dst, a);
  }
}

CAMLprim value NAME(value vPKIND, intnat vPINIT, intnat vM, intnat vN,
                    intnat vAR, intnat vAC, value vA, intnat vBR, intnat vBC,
                    value vB) {
  CAMLparam2(vA, vB);

  integer GET_INT(M), GET_INT(N);

  if (M > 0) {
    MAT_PARAMS(A);
    MAT_PARAMS(B);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    caml_enter_blocking_section(); /* Allow other threads */
    switch (PKIND) {
    case UPPER: {
      NUMBER *A_stop = A_data + rows_A * N;
      if (PINIT + N - 1 <= M) {
        while (A_data < A_stop) {
          STR(NAME, _range)(PINIT, A_data, B_data);
          PINIT++;
          A_data += rows_A;
          B_data += rows_B;
        }
      } else {
        while (PINIT < M) {
          STR(NAME, _range)(PINIT, A_data, B_data);
          PINIT++;
          A_data += rows_A;
          B_data += rows_B;
        }
        if (M == rows_A)
          STR(NAME, _range)(A_stop - A_data, A_data, B_data);
        else
          while (A_data < A_stop) {
            STR(NAME, _range)(M, A_data, B_data);
            A_data += rows_A;
            B_data += rows_B;
          }
      }
      break;
    }
    case LOWER: {
      NUMBER *A_stop;
      integer stop_col = M + PINIT;
      if (stop_col > N)
        stop_col = N;
      A_stop = A_data + stop_col * rows_A;
      if (PINIT > 1) {
        if (M == rows_A && M == rows_B) {
          integer MP = M * PINIT;
          STR(NAME, _range)(MP, A_data, B_data);
          A_data += MP;
          B_data += MP;
        } else {
          NUMBER *A_block_stop = A_data + PINIT * rows_A;
          while (A_data < A_block_stop) {
            STR(NAME, _range)(M, A_data, B_data);
            A_data += rows_A;
            B_data += rows_B;
          }
        }
        A_data++;
        B_data++;
        M--;
      }
      rows_A++;
      rows_B++;
      while (A_data < A_stop) {
        STR(NAME, _range)(M, A_data, B_data);
        M--;
        A_data += rows_A;
        B_data += rows_B;
      }
      break;
    }
    }
    caml_leave_blocking_section(); /* Disallow other threads */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int __unused argn) {
  return NAME(argv[0], Int_val(argv[1]), Int_val(argv[2]), Int_val(argv[3]),
              Int_val(argv[4]), Int_val(argv[5]), argv[6], Int_val(argv[7]),
              Int_val(argv[8]), argv[9]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
