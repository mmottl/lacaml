/* File: mat_map.c

   Copyright (C) 2015-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "lacaml_macros.h"

CAMLprim value NAME(
  value vM, value vN,
  value vAR, value vAC, value vA,
  value vBR, value vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(M), GET_INT(N);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  if (M > 0) {
    NUMBER *last = A_data + N*rows_A;
    integer diff_A = rows_A - M;
    integer diff_B = rows_B - M;

    caml_enter_blocking_section();  /* Allow other threads */

    while (A_data != last) {
      NUMBER *A_col_last = A_data + M;

      while (A_data != A_col_last) {
        NUMBER x = *A_data;
        FUNC(B_data, x);
        A_data++;
        B_data++;
      }

      A_data += diff_A;
      B_data += diff_B;
    }

    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value BC_NAME(value *argv, int __unused argn)
{
  return
    NAME(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

#undef NAME
#undef BC_NAME
#undef FUNC
