/* File: mat_fold2.c

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

static inline NUMBER STR(NAME, _range)(integer N,
    NUMBER *A_data, NUMBER *B_data, NUMBER acc)
{
  for (int i = 0; i < N; i++) {
    NUMBER a = A_data[i];
    NUMBER b = B_data[i];
    FUNC(acc, a, b);
  }
  return acc;
}

CAMLprim value NAME(
    value vPKIND, value vPINIT,
    value vM, value vN,
    value vAR, value vAC, value vA,
    value vBR, value vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(M), GET_INT(N);
  NUMBER acc = INIT;

  if (M > 0) {
    MAT_PARAMS(A);
    MAT_PARAMS(B);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer PINIT = Long_val(vPINIT);
    caml_enter_blocking_section();  /* Allow other threads */
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                acc = STR(NAME, _range)(PINIT, A_data, B_data, acc);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
              }
            } else {
              while (PINIT < M) {
                acc = STR(NAME, _range)(PINIT, A_data, B_data, acc);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
              }
              if (M == rows_A)
                acc = STR(NAME, _range)(A_stop - A_data, A_data, B_data, acc);
              else
                while (A_data < A_stop) {
                  acc = STR(NAME, _range)(M, A_data, B_data, acc);
                  A_data += rows_A;
                  B_data += rows_B;
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
              if (M == rows_A && M == rows_B) {
                integer MP = M*PINIT;
                acc = STR(NAME, _range)(MP, A_data, B_data, acc);
                A_data += MP;
                B_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  acc = STR(NAME, _range)(M, A_data, B_data, acc);
                  A_data += rows_A;
                  B_data += rows_B;
                }
              }
              M--;
            }
            rows_A++;
            while (A_data < A_stop) {
              acc = STR(NAME, _range)(M, A_data, B_data, acc);
              M--;
              A_data += rows_A;
              B_data += rows_B;
            }
            break;
          }
      }
    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(COPY_NUMBER(acc));
}

CAMLprim value BC_NAME(value *argv, int __unused argn)
{
  return NAME(
      argv[0], argv[1], argv[2], argv[3], argv[4],
      argv[5], argv[6], argv[7], argv[8], argv[9]);
}

#undef NAME
#undef INIT
#undef FUNC
