/* File: mat_combine.h

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

static inline void STR(NAME, _range)(integer N,
    NUMBER *A_data, NUMBER *B_data, NUMBER *C_data)
{
  for (int i = 0; i < N; i++) {
    NUMBER a = A_data[i];
    NUMBER b = B_data[i];
    NUMBER *dst = C_data + i;
    FUNC(dst, a, b);
  }
}

CAMLprim value NAME(
  value vPKIND, value vPINIT,
  value vM, value vN,
  value vAR, value vAC, value vA,
  value vBR, value vBC, value vB,
  value vCR, value vCC, value vC)
{
  CAMLparam3(vA, vB, vC);

  integer GET_INT(M), GET_INT(N);

  if (M > 0) {
    MAT_PARAMS(A);
    MAT_PARAMS(B);
    MAT_PARAMS(C);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer PINIT = Long_val(vPINIT);
    caml_enter_blocking_section();  /* Allow other threads */
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                STR(NAME, _range)(PINIT, A_data, B_data, C_data);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
                C_data += rows_C;
              }
            } else {
              while (PINIT < M) {
                STR(NAME, _range)(PINIT, A_data, B_data, C_data);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
                C_data += rows_C;
              }
              if (M == rows_A && M == rows_B && M == rows_C)
                STR(NAME, _range)(A_stop - A_data, A_data, B_data, C_data);
              else
                while (A_data < A_stop) {
                  STR(NAME, _range)(M, A_data, B_data, C_data);
                  A_data += rows_A;
                  B_data += rows_B;
                  C_data += rows_C;
                }
            }
            break;
          }
        case LOWER :
          {
            NUMBER *A_stop;
            integer stop_col = M + PINIT;
            if (stop_col > N) stop_col = N;
            A_stop = A_data + stop_col*rows_A;
            if (PINIT > 1) {
              if (M == rows_A && M == rows_B && M == rows_C) {
                integer MP = M*PINIT;
                STR(NAME, _range)(MP, A_data, B_data, C_data);
                A_data += MP;
                B_data += MP;
                C_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  STR(NAME, _range)(M, A_data, B_data, C_data);
                  A_data += rows_A;
                  B_data += rows_B;
                  C_data += rows_C;
                }
              }
              M--;
            }
            rows_A++; rows_B++; rows_C++;
            while (A_data < A_stop) {
              STR(NAME, _range)(M, A_data, B_data, C_data);
              M--;
              A_data += rows_A;
              B_data += rows_B;
              C_data += rows_C;
            }
            break;
          }
      }
    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int __unused argn)
{
  return
    NAME(argv[0], argv[1], argv[2],argv[3], argv[4], argv[5],
         argv[6], argv[7], argv[8], argv[9], argv[10], argv[11], argv[12]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
