/* File: mat_combine.c

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

CAMLprim value NAME(
  value vM, value vN,
  value vAR, value vAC, value vA,
  value vBR, value vBC, value vB,
  value vCR, value vCC, value vC)
{
  CAMLparam3(vA, vB, vC);

  integer GET_INT(M), GET_INT(N);

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  MAT_PARAMS(C);

  if (M > 0) {
    NUMBER *last = A_data + N*rows_A;
    integer diff_A = rows_A - M;
    integer diff_B = rows_B - M;
    integer diff_C = rows_C - M;

    caml_enter_blocking_section();  /* Allow other threads */

    while (A_data != last) {
      NUMBER *A_col_last = A_data + M;

      while (A_data != A_col_last) {
        NUMBER x = *A_data;
        NUMBER y = *B_data;
        FUNC(C_data, x, y);
        A_data++;
        B_data++;
        C_data++;
      }

      A_data += diff_A;
      B_data += diff_B;
      C_data += diff_C;
    }

    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int __unused argn)
{
  return
    NAME(argv[0], argv[1], argv[2],argv[3], argv[4], argv[5],
         argv[6], argv[7], argv[8], argv[9], argv[10]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
