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
#include "utils_c.h"
#include "f2c.h"

static integer integer_one = 1;
static NUMBER number_one = NUMBER_ONE;
static char uplo_U = 'U';


/* scal */

extern void FUN(scal)(
  integer *N,
  NUMBER *ALPHA,
  NUMBER *X, integer *INCX);

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
      FUN(scal)(&MN, pALPHA, A_data, &integer_one);
    } else {
      NUMBER *A_src = A_data + rows_A * (N - 1);
      while (A_src >= A_data) {
        FUN(scal)(&M, pALPHA, A_src, &integer_one);
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


/* scal_cols */

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

  NUMBER *A_src = A_data + rows_A * (N - 1);
  NUMBER *ALPHAs_src = ALPHAs_data + (N - 1);

  caml_enter_blocking_section();
    while (A_src >= A_data) {
      FUN(scal)(&M, ALPHAs_src, A_src, &integer_one);
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


/* mat_axpy */

extern void FUN(axpy)(
  integer *N,
  NUMBER *ALPHA,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(mat_axpy_stub)(
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
      FUN(axpy)(&MN, pALPHA, X_data, &integer_one, Y_data, &integer_one);
    } else {
      NUMBER *X_src = X_data + rows_X * (N - 1);
      NUMBER *Y_dst = Y_data + rows_Y * (N - 1);
      while (X_src >= X_data) {
        FUN(axpy)(&M, pALPHA, X_src, &integer_one, Y_dst, &integer_one);
        X_src -= rows_X;
        Y_dst -= rows_Y;
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(mat_axpy_stub_bc)(value *argv, int argn)
{
  return LFUN(mat_axpy_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4],
    argv[5], argv[6], argv[7], argv[8]);
}


/* gemm_diag */

extern void FUN(gemm)(
  char *TRANSA, char *TRANSB,
  integer *M, integer *N, integer *K,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  NUMBER *BETA,
  NUMBER *C, integer *LDC);

CAMLprim value LFUN(gemm_diag_stub)(
  value vTRANSA,
  value vTRANSB,
  value vN, value vK,
  value vAR, value vAC, value vA,
  value vBR, value vBC, value vB,
  value vOFSY,
  value vY,
  value vALPHA,
  value vBETA
  )
{
  CAMLparam3(vA, vB, vY);

  integer GET_INT(N), GET_INT(K);
  char GET_INT(TRANSA), GET_INT(TRANSB);

  CREATE_NUMBERP(ALPHA);
  CREATE_NUMBERP(BETA);

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  VEC_PARAMS(Y);

  int incr_A = (TRANSA == 'N') ? 1 : rows_A;
  int incr_B = (TRANSB == 'N') ? rows_B : 1;

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  while (N--) {
    /* TODO: quite inefficient for small K (> factor 2 for ten elements).
       Optimize by essentially reimplementing gemm, possibly using "dot"
       at each step, but hoisting all initializations and checks out of
       the loop. */
    FUN(gemm)(
      &TRANSA, &TRANSB,
      &integer_one, &integer_one, &K,
      pALPHA,
      A_data, &rows_A,
      B_data, &rows_B,
      pBETA,
      Y_data, &integer_one);
    A_data += incr_A;
    B_data += incr_B;
    Y_data++;
  }
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(gemm_diag_stub_bc)(value *argv, int argn)
{
  return LFUN(gemm_diag_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
    argv[7], argv[8], argv[9], argv[10], argv[11], argv[12], argv[13]);
}


/* syrk_diag */

extern void FUN(syrk)(
  char *UPLO, char *TRANS,
  integer *N, integer *K,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *BETA,
  NUMBER *C, integer *LDC);

CAMLprim value LFUN(syrk_diag_stub)(
  value vTRANS,
  value vN, value vK,
  value vAR, value vAC, value vA,
  value vOFSY,
  value vY,
  value vALPHA,
  value vBETA
  )
{
  CAMLparam2(vA, vY);

  integer GET_INT(N), GET_INT(K);
  char GET_INT(TRANS);

  CREATE_NUMBERP(ALPHA);
  CREATE_NUMBERP(BETA);

  MAT_PARAMS(A);
  VEC_PARAMS(Y);

  int incr_A = (TRANS == 'N') ? 1 : rows_A;

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  while (N--) {
    /* TODO: quite inefficient for small K (> factor 2 for ten elements).
       Optimize by essentially reimplementing syrk, possibly using "dot"
       at each step, but hoisting all initializations and checks out of
       the loop. */
    FUN(syrk)(
      &uplo_U, &TRANS,
      &integer_one, &K,
      pALPHA,
      A_data, &rows_A,
      pBETA,
      Y_data, &integer_one);
    A_data += incr_A;
    Y_data++;
  }
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(syrk_diag_stub_bc)(value *argv, int argn)
{
  return LFUN(syrk_diag_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
    argv[7], argv[8], argv[9]);
}


/* gemm_trace */

CAMLprim value LFUN(gemm_trace_stub)(
  value vTRANSA,
  value vTRANSB,
  value vN, value vK,
  value vAR, value vAC, value vA,
  value vBR, value vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(N), GET_INT(K);
  char GET_INT(TRANSA), GET_INT(TRANSB);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  int incr_A = (TRANSA == 'N') ? 1 : rows_A;
  int incr_B = (TRANSB == 'N') ? rows_B : 1;

  NUMBER res = NUMBER_ZERO;

  caml_enter_blocking_section();  /* Allow other threads */
  while (N--) {
    /* TODO: quite inefficient for small K (> factor 2 for ten elements).
       Optimize by essentially reimplementing gemm, possibly using "dot"
       at each step, but hoisting all initializations and checks out of
       the loop. */
    FUN(gemm)(
      &TRANSA, &TRANSB,
      &integer_one, &integer_one, &K,
      &number_one,
      A_data, &rows_A,
      B_data, &rows_B,
      &number_one,
      &res, &integer_one);
    A_data += incr_A;
    B_data += incr_B;
  }
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(COPY_NUMBER(res));
}

CAMLprim value LFUN(gemm_trace_stub_bc)(value *argv, int argn)
{
  return LFUN(gemm_trace_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4],
    argv[5], argv[6], argv[7], argv[8], argv[9]);
}
