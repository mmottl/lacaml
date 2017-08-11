/* File: mat_fold.h

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

#define FOLD_REGION(N_ARG) \
  integer N = N_ARG; \
  for (int i = 0; i < N; i++) { \
    NUMBER a = A_data[i]; \
    FUNC(acc, a); \
  }

static inline NUMBER STR(NAME, _blocking)(
    integer PKIND, integer PINIT, integer M, integer N,
    NUMBER *A_data, integer rows_A, NUMBER acc) {
#ifdef DECLARE_EXTRA
    DECLARE_EXTRA;
#undef DECLARE_EXTRA
#endif

#ifdef INIT_HAVE_LOCK
      INIT_HAVE_LOCK;
#undef INIT_HAVE_LOCK
#endif

  switch (PKIND) {
    case UPPER :
      {
        NUMBER *A_stop = A_data + rows_A * N;
        if (PINIT + N - 1 <= M) {
          while (A_data < A_stop) {
            FOLD_REGION(PINIT);
            PINIT++;
            A_data += rows_A;
          }
        } else {
          while (PINIT < M) {
            FOLD_REGION(PINIT);
            PINIT++;
            A_data += rows_A;
          }
          if (M == rows_A) {
            FOLD_REGION(A_stop - A_data);
          }
          else
            while (A_data < A_stop) {
              FOLD_REGION(M);
              A_data += rows_A;
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
          if (M == rows_A) {
            integer MP = M*PINIT;
            FOLD_REGION(MP);
            A_data += MP;
          } else {
            NUMBER *A_block_stop = A_data + PINIT*rows_A;
            while (A_data < A_block_stop) {
              FOLD_REGION(M);
              A_data += rows_A;
            }
          }
          M--;
        }
        rows_A++;
        while (A_data < A_stop) {
          FOLD_REGION(M);
          M--;
          A_data += rows_A;
        }
        break;
      }
  }

#ifdef FINISH_HAVE_LOCK
      FINISH_HAVE_LOCK;
#undef FINISH_HAVE_LOCK
#endif

  return acc;
}

CAMLprim value NAME(
    value vPKIND, value vPINIT,
    value vM, value vN,
    value vAR, value vAC, value vA)
{
  CAMLparam1(vA);

  integer GET_INT(M), GET_INT(N);
  NUMBER acc = INIT;

  if (M > 0) {
    MAT_PARAMS(A);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer PINIT = Long_val(vPINIT);

    caml_enter_blocking_section();  /* Allow other threads */

      acc = STR(NAME, _blocking)(PKIND, PINIT, M, N, A_data, rows_A, acc);

    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(COPY_NUMBER(acc));
}

CAMLprim value BC_NAME(value *argv, int __unused argn)
{
  return NAME(argv[0], argv[1], argv[2],argv[3], argv[4], argv[5], argv[6]);
}

#undef FOLD_REGION
#undef NAME
#undef BC_NAME
#undef INIT
#undef FUNC
