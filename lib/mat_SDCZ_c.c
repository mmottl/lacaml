/* File: mat_SDCZ_c.c

   Copyright (C) 2007-

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

#include <string.h>
#include "lacaml_macros.h"
#include "f2c.h"

CAMLprim value LFUN(copy_mat_stub)(
  value vM, value vN,
  value vYR, value vYC,
  value vY,
  value vXR, value vXC,
  value vX)
{
  CAMLparam2(vX, vY);

  integer GET_INT(M), GET_INT(N);

  MAT_PARAMS(X);
  MAT_PARAMS(Y);

  caml_enter_blocking_section();
    if (rows_X == M && rows_Y == M)
      memcpy(Y_data, X_data, M * N * sizeof(NUMBER));
    else {
      integer col_size = M * sizeof(NUMBER);
      NUMBER *X_src = X_data + rows_X * (N - 1);
      NUMBER *Y_dst = Y_data + rows_Y * (N - 1);
      while (X_src >= X_data) {
        memcpy(Y_dst, X_src, col_size);
        X_src -= rows_X;
        Y_dst -= rows_Y;
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(copy_mat_stub_bc)(value *argv, int argn)
{
  return LFUN(copy_mat_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4],
    argv[5], argv[6], argv[7]);
}

static integer ONE = 1;

CAMLprim value LFUN(scal_mat_stub)(
  value vM, value vN,
  value vALPHA,
  value vAR, value vAC, value vA)
{
  CAMLparam1(vA);

  integer GET_INT(M), GET_INT(N);
  CREATE_NUMBERP(ALPHA);

  MAT_PARAMS(A);

  INIT_NUMBER(ALPHA);

  caml_enter_blocking_section();
    if (rows_A == M) {
      integer MN = M * N;
      FUN(scal)(&MN, pALPHA, A_data, &ONE);
    } else {
      NUMBER *A_src = A_data + rows_A * (N - 1);
      while (A_src >= A_data) {
        FUN(scal)(&M, pALPHA, A_src, &ONE);
        A_src -= rows_A;
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(scal_mat_stub_bc)(value *argv, int argn)
{
  return LFUN(scal_mat_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value LFUN(scal_cols_stub)(
  value vM, value vN,
  value vOFSALPHAs,
  value vALPHAs,
  value vAR, value vAC, value vA)
{
  CAMLparam2(vALPHAs, vA);

  integer GET_INT(M), GET_INT(N);

  VEC_PARAMS(ALPHAs);
  MAT_PARAMS(A);

  caml_enter_blocking_section();
    NUMBER *A_src = A_data + rows_A * (N - 1);
    NUMBER *ALPHAs_src = ALPHAs_data + (N - 1);
    while (A_src >= A_data) {
      FUN(scal)(&M, ALPHAs_src, A_src, &ONE);
      A_src -= rows_A;
      ALPHAs_src--;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(scal_cols_stub_bc)(value *argv, int argn)
{
  return LFUN(scal_cols_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value LFUN(axpy_mat_stub)(
  value vM, value vN,
  value vALPHA,
  value vXR, value vXC, value vX,
  value vYR, value vYC, value vY)
{
  CAMLparam2(vX, vY);

  integer GET_INT(M), GET_INT(N);
  CREATE_NUMBERP(ALPHA);

  MAT_PARAMS(X);
  MAT_PARAMS(Y);

  INIT_NUMBER(ALPHA);

  caml_enter_blocking_section();
    if (rows_X == M && rows_Y == M) {
      integer MN = M * N;
      FUN(axpy)(&MN, pALPHA, X_data, &ONE, Y_data, &ONE);
    } else {
      NUMBER *X_src = X_data + rows_X * (N - 1);
      NUMBER *Y_dst = Y_data + rows_Y * (N - 1);
      while (X_src >= X_data) {
        FUN(axpy)(&M, pALPHA, X_src, &ONE, Y_dst, &ONE);
        X_src -= rows_X;
        Y_dst -= rows_Y;
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(axpy_mat_stub_bc)(value *argv, int argn)
{
  return LFUN(axpy_mat_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4],
    argv[5], argv[6], argv[7], argv[8]);
}
