/* File: impl_SDCZ.h

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

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

static integer integer_one = 1;

/*** BLAS-1 */

/** SWAP */

extern void FUN(swap)(
  integer *N,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(swap_stub)(
  intnat vN,
  intnat vOFSX, intnat vINCX, value vX,
  intnat vOFSY, intnat vINCY, value vY)
{
  CAMLparam2(vX, vY);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(swap)(
    &N,
    X_data, &INCX,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(swap_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(swap_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6]);
}


/** SCAL */

extern void FUN(scal)(
  integer *N,
  NUMBER *ALPHA,
  NUMBER *X, integer *INCX);

CAMLprim value LFUN(scal_stub)(
  intnat vN,
  vNUMBER vALPHA,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N),
          GET_INT(INCX);

  NUMBER ALPHA;

  VEC_PARAMS(X);

  INIT_NUMBER(ALPHA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(scal)(
    &N,
    &ALPHA,
    X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(scal_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(scal_stub)(
        Int_val(argv[0]),
        NUMBER_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4]);
}


/** COPY */

extern void FUN(copy)(
  integer *N,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(copy_stub)(
  intnat vN,
  intnat vOFSY, intnat vINCY, value vY,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam2(vX, vY);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(copy)(
    &N,
    X_data, &INCX,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(copy_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(copy_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6]);
}


/** NRM2 */

#ifndef LACAML_COMPLEX          /* Real number */
extern REAL FUN(nrm2)(integer *N, REAL *X, integer *INCX);
#else
#ifndef LACAML_DOUBLE
extern real scnrm2_(integer *N, complex *X, integer *INCX);
#else
extern doublereal dznrm2_(integer *N, doublecomplex *X, integer *INCX);
#endif
#endif

CAMLprim double LFUN(nrm2_stub)(intnat vN, intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N),
          GET_INT(INCX);

  REAL res;

  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
#ifndef LACAML_COMPLEX          /* Real number */
  res = FUN(nrm2)(&N, X_data, &INCX);
#else
#ifndef LACAML_DOUBLE
  res = scnrm2_(&N, X_data, &INCX);
#else
  res = dznrm2_(&N, X_data, &INCX);
#endif
#endif
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(res);
}

CAMLprim value LFUN(nrm2_stub_bc)(value *argv, int __unused argn)
{
  return
    caml_copy_double(
        LFUN(nrm2_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          Int_val(argv[2]),
          argv[3]));
}


/** AXPY */

extern void FUN(axpy)(
  integer *N,
  NUMBER *ALPHA,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(axpy_stub)(
  vNUMBER vALPHA,
  intnat vN,
  intnat vOFSX, intnat vINCX, value vX,
  intnat vOFSY, intnat vINCY, value vY)
{
  CAMLparam2(vX, vY);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  NUMBER ALPHA;

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  INIT_NUMBER(ALPHA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(axpy)(
    &N,
    &ALPHA,
    X_data, &INCX,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(axpy_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(axpy_stub)(
        NUMBER_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4],
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7]);
}


/** AMAX */

extern integer FUN2(i, amax)(integer *N, NUMBER *X, integer *INCX);

CAMLprim intnat LFUN(iamax_stub)(
    intnat vN, intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N),
          GET_INT(INCX),
          index;

  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  index = FUN2(i,amax)(&N, X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(index);
}

CAMLprim value LFUN(iamax_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(iamax_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          Int_val(argv[3]),
          argv[4]));
}


/*** BLAS-2 */

/** GEMV */

extern void FUN(gemv)(
  char *TRANS,
  integer *M, integer *N,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *X, integer *INCX,
  NUMBER *BETA,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(gemv_stub)(
  intnat vOFSY, intnat vINCY, value vY,
  intnat vAR, intnat vAC, value vA,
  intnat vM, intnat vN,
  value vTRANS,
  vNUMBER vALPHA, vNUMBER vBETA,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam3(vA, vX, vY);

  char GET_CHAR(TRANS);

  integer GET_INT(M),
          GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gemv)(
    &TRANS,
    &M, &N,
    &ALPHA,
    A_data, &rows_A,
    X_data, &INCX,
    &BETA,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(gemv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gemv_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        argv[2],
        Int_val(argv[3]),
        Int_val(argv[4]),
        argv[5],
        Int_val(argv[6]),
        Int_val(argv[7]),
        argv[8],
        NUMBER_val(argv[9]),
        NUMBER_val(argv[10]),
        Int_val(argv[11]),
        Int_val(argv[12]),
        argv[13]);
}


/** GBMV */

extern void FUN(gbmv)(
  char *TRANS,
  integer *M, integer *N, integer *KL, integer *KU,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *X, integer *INCX,
  NUMBER *BETA,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(gbmv_stub)(
  intnat vOFSY, intnat vINCY, value vY,
  intnat vAR, intnat vAC, value vA,
  intnat vM, intnat vN, intnat vKL, intnat vKU,
  value vTRANS,
  vNUMBER vALPHA, vNUMBER vBETA,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam3(vA, vX, vY);

  char GET_CHAR(TRANS);

  integer GET_INT(M),
          GET_INT(N),
          GET_INT(KL),
          GET_INT(KU),
          GET_INT(INCX),
          GET_INT(INCY);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gbmv)(
    &TRANS,
    &M, &N, &KL, &KU,
    &ALPHA,
    A_data, &rows_A,
    X_data, &INCX,
    &BETA,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(gbmv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gbmv_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        argv[2],
        Int_val(argv[3]),
        Int_val(argv[4]),
        argv[5],
        Int_val(argv[6]),
        Int_val(argv[7]),
        Int_val(argv[8]),
        Int_val(argv[9]),
        argv[10],
        NUMBER_val(argv[11]),
        NUMBER_val(argv[12]),
        Int_val(argv[13]),
        Int_val(argv[14]),
        argv[15]);
}


/** SYMV */

extern void FUN(symv)(
  char *UPLO,
  integer *N,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *X, integer *INCX,
  NUMBER *BETA,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(symv_stub)(
  intnat vOFSY, intnat vINCY, value vY,
  intnat vAR,
  intnat vAC,
  value vA,
  intnat vN,
  value vUPLO,
  vNUMBER vALPHA,
  vNUMBER vBETA,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam3(vA, vX, vY);

  char GET_CHAR(UPLO);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(symv)(
    &UPLO,
    &N,
    &ALPHA,
    A_data, &rows_A,
    X_data, &INCX,
    &BETA,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(symv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(symv_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        argv[2],
        Int_val(argv[3]),
        Int_val(argv[4]),
        argv[5],
        Int_val(argv[6]),
        argv[7],
        NUMBER_val(argv[8]),
        NUMBER_val(argv[9]),
        Int_val(argv[10]),
        Int_val(argv[11]),
        argv[12]);
}


/** TRMV */

extern void FUN(trmv)(
  char *UPLO,
  char *TRANS,
  char *DIAG,
  integer *N,
  NUMBER *A, integer *LDA,
  NUMBER *X, integer *INCX);

CAMLprim value LFUN(trmv_stub)(
  intnat vAR,
  intnat vAC,
  value vA,
  intnat vN,
  value vUPLO,
  value vTRANS,
  value vDIAG,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam2(vA, vX);

  char GET_CHAR(UPLO),
       GET_CHAR(TRANS),
       GET_CHAR(DIAG);

  integer GET_INT(N),
          GET_INT(INCX);

  MAT_PARAMS(A);
  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(trmv)(
    &UPLO,
    &TRANS,
    &DIAG,
    &N,
    A_data, &rows_A,
    X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(trmv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(trmv_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        argv[2],
        Int_val(argv[3]),
        argv[4],
        argv[5],
        argv[6],
        Int_val(argv[7]),
        Int_val(argv[8]),
        argv[9]);
}


/** TRSV */

extern void FUN(trsv)(
  char *UPLO,
  char *TRANS,
  char *DIAG,
  integer *N,
  NUMBER *A, integer *LDA,
  NUMBER *X, integer *INCX);

CAMLprim value LFUN(trsv_stub)(
  intnat vAR,
  intnat vAC,
  value vA,
  intnat vN,
  value vUPLO,
  value vTRANS,
  value vDIAG,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam2(vA, vX);

  char GET_CHAR(UPLO),
       GET_CHAR(TRANS),
       GET_CHAR(DIAG);

  integer GET_INT(N),
          GET_INT(INCX);

  MAT_PARAMS(A);
  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(trsv)(
    &UPLO,
    &TRANS,
    &DIAG,
    &N,
    A_data, &rows_A,
    X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(trsv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(trsv_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        argv[2],
        Int_val(argv[3]),
        argv[4],
        argv[5],
        argv[6],
        Int_val(argv[7]),
        Int_val(argv[8]),
        argv[9]);
}


/** TPMV */

extern void FUN(tpmv)(
  char *UPLO,
  char *TRANS,
  char *DIAG,
  integer *N,
  NUMBER *AP,
  NUMBER *X, integer *INCX);

CAMLprim value LFUN(tpmv_stub)(
  intnat vOFSAP,
  value vAP,
  intnat vN,
  value vUPLO,
  value vTRANS,
  value vDIAG,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam2(vAP, vX);

  char GET_CHAR(UPLO),
       GET_CHAR(TRANS),
       GET_CHAR(DIAG);

  integer GET_INT(N),
          GET_INT(INCX);

  VEC_PARAMS(AP);
  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(tpmv)(
    &UPLO,
    &TRANS,
    &DIAG,
    &N,
    AP_data,
    X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(tpmv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(tpmv_stub)(
        Int_val(argv[0]),
        argv[1],
        Int_val(argv[2]),
        argv[3],
        argv[4],
        argv[5],
        Int_val(argv[6]),
        Int_val(argv[7]),
        argv[8]);
}


/** TPSV */

extern void FUN(tpsv)(
  char *UPLO,
  char *TRANS,
  char *DIAG,
  integer *N,
  NUMBER *AP,
  NUMBER *X, integer *INCX);

CAMLprim value LFUN(tpsv_stub)(
  intnat vOFSAP,
  value vAP,
  intnat vN,
  value vUPLO,
  value vTRANS,
  value vDIAG,
  intnat vOFSX, intnat vINCX, value vX)
{
  CAMLparam2(vAP, vX);

  char GET_CHAR(UPLO),
       GET_CHAR(TRANS),
       GET_CHAR(DIAG);

  integer GET_INT(N),
          GET_INT(INCX);

  VEC_PARAMS(AP);
  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(tpsv)(
    &UPLO,
    &TRANS,
    &DIAG,
    &N,
    AP_data,
    X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(tpsv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(tpsv_stub)(
        Int_val(argv[0]),
        argv[1],
        Int_val(argv[2]),
        argv[3],
        argv[4],
        argv[5],
        Int_val(argv[6]),
        Int_val(argv[7]),
        argv[8]);
}


/** TODO: SPMV */

/** TODO: TBMV */

/** TODO: TBSV */


/*** BLAS-3 */

/** GEMM */

extern void FUN(gemm)(
  char *TRANSA, char *TRANSB,
  integer *M, integer *N, integer *K,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  NUMBER *BETA,
  NUMBER *C, integer *LDC);

CAMLprim value LFUN(gemm_stub)(
  value vTRANSA, value vTRANSB,
  intnat vM, intnat vN, intnat vK,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB,
  intnat vCR, intnat vCC, value vC,
  vNUMBER vALPHA, vNUMBER vBETA)
{
  CAMLparam3(vA, vB, vC);

  char GET_CHAR(TRANSA), GET_CHAR(TRANSB);
  integer GET_INT(M), GET_INT(N), GET_INT(K);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  MAT_PARAMS(C);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gemm)(
    &TRANSA, &TRANSB,
    &M, &N, &K,
    &ALPHA,
    A_data, &rows_A,
    B_data, &rows_B,
    &BETA,
    C_data, &rows_C);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(gemm_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gemm_stub)(
        argv[0],
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7],
        Int_val(argv[8]),
        Int_val(argv[9]),
        argv[10],
        Int_val(argv[11]),
        Int_val(argv[12]),
        argv[13],
        NUMBER_val(argv[14]),
        NUMBER_val(argv[15]));
}

/** SYMM */

extern void FUN(symm)(
  char *SIDE, char *UPLO,
  integer *M, integer *N,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  NUMBER *BETA,
  NUMBER *C, integer *LDC);

CAMLprim value LFUN(symm_stub)(
  value vSIDE, value vUPLO,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB,
  intnat vCR, intnat vCC, value vC,
  vNUMBER vALPHA, vNUMBER vBETA)
{
  CAMLparam3(vA, vB, vC);

  char GET_CHAR(SIDE), GET_CHAR(UPLO);
  integer GET_INT(M), GET_INT(N);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  MAT_PARAMS(C);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(symm)(
    &SIDE, &UPLO,
    &M, &N,
    &ALPHA,
    A_data, &rows_A,
    B_data, &rows_B,
    &BETA,
    C_data, &rows_C);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(symm_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(symm_stub)(
        argv[0],
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        Int_val(argv[7]),
        Int_val(argv[8]),
        argv[9],
        Int_val(argv[10]),
        Int_val(argv[11]),
        argv[12],
        NUMBER_val(argv[13]),
        NUMBER_val(argv[14]));
}

/** TRMM */

extern void FUN(trmm)(
  char *SIDE, char *UPLO, char *TRANS, char *DIAG,
  integer *M, integer *N,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB);

CAMLprim value LFUN(trmm_stub)(
  value vSIDE, value vUPLO, value vTRANS, value vDIAG,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB,
  vNUMBER vALPHA)
{
  CAMLparam2(vA, vB);

  char GET_CHAR(SIDE), GET_CHAR(UPLO), GET_CHAR(TRANS), GET_CHAR(DIAG);
  integer GET_INT(M), GET_INT(N);

  NUMBER ALPHA;

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  INIT_NUMBER(ALPHA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(trmm)(
    &SIDE, &UPLO, &TRANS, &DIAG,
    &M, &N,
    &ALPHA,
    A_data, &rows_A,
    B_data, &rows_B);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(trmm_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(trmm_stub)(
        argv[0],
        argv[1],
        argv[2],
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        Int_val(argv[7]),
        argv[8],
        Int_val(argv[9]),
        Int_val(argv[10]),
        argv[11],
        NUMBER_val(argv[12]));
}

/** TRSM */

extern void FUN(trsm)(
  char *SIDE, char *UPLO, char *TRANS, char *DIAG,
  integer *M, integer *N,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB);

CAMLprim value LFUN(trsm_stub)(
  value vSIDE, value vUPLO, value vTRANS, value vDIAG,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB,
  vNUMBER vALPHA)
{
  CAMLparam2(vA, vB);

  char GET_CHAR(SIDE), GET_CHAR(UPLO), GET_CHAR(TRANS), GET_CHAR(DIAG);
  integer GET_INT(M), GET_INT(N);

  NUMBER ALPHA;

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  INIT_NUMBER(ALPHA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(trsm)(
    &SIDE, &UPLO, &TRANS, &DIAG,
    &M, &N,
    &ALPHA,
    A_data, &rows_A,
    B_data, &rows_B);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(trsm_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(trsm_stub)(
        argv[0],
        argv[1],
        argv[2],
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        Int_val(argv[7]),
        argv[8],
        Int_val(argv[9]),
        Int_val(argv[10]),
        argv[11],
        NUMBER_val(argv[12]));
}

/** SYRK */

extern void FUN(syrk)(
  char *UPLO, char *TRANS,
  integer *N, integer *K,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *BETA,
  NUMBER *C, integer *LDC);

CAMLprim value LFUN(syrk_stub)(
  value vUPLO, value vTRANS,
  intnat vN, intnat vK,
  intnat vAR, intnat vAC, value vA,
  intnat vCR, intnat vCC, value vC,
  vNUMBER vALPHA, vNUMBER vBETA)
{
  CAMLparam2(vA, vC);

  char GET_CHAR(UPLO), GET_CHAR(TRANS);
  integer GET_INT(N), GET_INT(K);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  MAT_PARAMS(C);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(syrk)(
    &UPLO, &TRANS,
    &N, &K,
    &ALPHA,
    A_data, &rows_A,
    &BETA,
    C_data, &rows_C);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(syrk_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syrk_stub)(
        argv[0],
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        Int_val(argv[7]),
        Int_val(argv[8]),
        argv[9],
        NUMBER_val(argv[10]),
        NUMBER_val(argv[11]));
}

/** SYR2K */

extern void FUN(syr2k)(
  char *UPLO, char *TRANS,
  integer *N, integer *K,
  NUMBER *ALPHA,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  NUMBER *BETA,
  NUMBER *C, integer *LDC);

CAMLprim value LFUN(syr2k_stub)(
  value vUPLO, value vTRANS,
  intnat vN, intnat vK,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB,
  intnat vCR, intnat vCC, value vC,
  vNUMBER vALPHA, vNUMBER vBETA)
{
  CAMLparam2(vA, vC);

  char GET_CHAR(UPLO), GET_CHAR(TRANS);
  integer GET_INT(N), GET_INT(K);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  MAT_PARAMS(C);

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(syr2k)(
    &UPLO, &TRANS,
    &N, &K,
    &ALPHA,
    A_data, &rows_A,
    B_data, &rows_B,
    &BETA,
    C_data, &rows_C);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(syr2k_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syr2k_stub)(
        argv[0],
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        Int_val(argv[7]),
        Int_val(argv[8]),
        argv[9],
        Int_val(argv[10]),
        Int_val(argv[11]),
        argv[12],
        NUMBER_val(argv[13]),
        NUMBER_val(argv[14]));
}


/*** LAPACK */

/* Auxiliary Routines
************************************************************************/

/** LARNV */

extern void FUN(larnv)(
  integer *IDIST,
  integer *ISEED,
  integer *N,
  NUMBER *X);

CAMLprim value LFUN(larnv_stub)(
  intnat vIDIST,
  value vISEED,
  intnat vN,
  intnat vOFSX,
  value vX)
{
  CAMLparam2(vISEED, vX);

  integer GET_INT(IDIST), GET_INT(N);

  INT_VEC_PARAMS(ISEED);
  VEC_PARAMS(X);

  caml_enter_blocking_section();
  FUN(larnv)(&IDIST, ISEED_data, &N, X_data);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(larnv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(larnv_stub)(
        Int_val(argv[0]),
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4]);
}


/** LACPY */

extern void FUN(lacpy)(
  char *UPLO,
  integer *M,
  integer *N,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB);

CAMLprim value LFUN(lacpy_stub)(
  value vPKIND, intnat vPINIT,
  value vUPLO,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(M), GET_INT(N), GET_INT(PINIT);
  char GET_CHAR(UPLO);
  MAT_PARAMS(A);
  MAT_PARAMS(B);
  pentagon_kind PKIND = get_pentagon_kind(vPKIND);

  caml_enter_blocking_section();  /* Allow other threads */

    switch (UPLO) {
      case 'A':
      case 'U':
      case 'L':
        FUN(lacpy)(
          &UPLO, &M, &N,
          A_data, &rows_A,
          B_data, &rows_B);
        break;
      default:
        switch (PKIND) {
          case UPPER :
            {
              NUMBER *A_stop = A_data + rows_A * N;
              if (PINIT + N - 1 <= M) {
                while (A_data < A_stop) {
                  FUN(copy)(&PINIT, A_data, &integer_one, B_data, &integer_one);
                  PINIT++;
                  A_data += rows_A;
                  B_data += rows_B;
                }
              } else {
                while (PINIT < M) {
                  FUN(copy)(&PINIT, A_data, &integer_one, B_data, &integer_one);
                  PINIT++;
                  A_data += rows_A;
                  B_data += rows_B;
                }
                if (M == rows_A && M == rows_B) {
                  integer MN = A_stop - A_data;
                  FUN(copy)(&MN, A_data, &integer_one, B_data, &integer_one);
                } else
                  while (A_data < A_stop) {
                    FUN(copy)(&M, A_data, &integer_one, B_data, &integer_one);
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
                  FUN(copy)(&MP, A_data, &integer_one, B_data, &integer_one);
                  A_data += MP;
                  B_data += MP;
                } else {
                  NUMBER *A_block_stop = A_data + PINIT*rows_A;
                  while (A_data < A_block_stop) {
                    FUN(copy)(&M, A_data, &integer_one, B_data, &integer_one);
                    A_data += rows_A;
                    B_data += rows_B;
                  }
                }
                M--;
              }
              rows_A++; rows_B++;
              while (A_data < A_stop) {
                FUN(copy)(&M, A_data, &integer_one, B_data, &integer_one);
                M--;
                A_data += rows_A;
                B_data += rows_B;
              }
              break;
            }
        }
        break;
    }

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(lacpy_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(lacpy_stub)(
        argv[0],
        Int_val(argv[1]),
        argv[2],
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7],
        Int_val(argv[8]),
        Int_val(argv[9]),
        argv[10]);
}


/** LASWP */

extern void FUN(laswp)(
  integer *N,
  NUMBER *A,
  integer *LDA,
  integer *K1,
  integer *K2,
  integer *IPIV,
  integer *INCX);

CAMLprim value LFUN(laswp_stub)(
  intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vK1, value vK2,
  value vIPIV,
  intnat vINCX)
{
  CAMLparam2(vA, vIPIV);

  integer GET_INT(N),
          GET_INT(K1),
          GET_INT(K2),
          GET_INT(INCX);

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);

  caml_release_runtime_system(); /* Allow other threads */
  FUN(laswp)(
    &N,
    A_data, &rows_A,
    &K1, &K2,
    IPIV_data, &INCX);
  caml_acquire_runtime_system(); /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(laswp_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(laswp_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        Int_val(argv[7]));
}


/** LAPMT */

extern void FUN(lapmt)(
  logical *FORWRD,
  integer *M,
  integer *N,
  NUMBER *X,
  integer *LDX,
  integer *K);

CAMLprim value LFUN(lapmt_stub)(
  value vFORWRD,
  intnat vM,
  intnat vN,
  value vK,
  intnat vAR,
  intnat vAC,
  value vA)
{
  CAMLparam2(vA, vK);

  logical GET_BOOL(FORWRD);

  integer GET_INT(M),
          GET_INT(N);

  MAT_PARAMS(A);
  INT_VEC_PARAMS(K);

  caml_release_runtime_system(); /* Allow other threads */
  FUN(lapmt)(
    &FORWRD,
    &M, &N,
    A_data,
    &rows_A,
    K_data);
  caml_acquire_runtime_system(); /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(lapmt_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(lapmt_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6]);
}


/** LASSQ */

extern void FUN(lassq)(
  integer *N,
  NUMBER *X, integer *INCX,
  REAL *SCALE, REAL *SUMSQ);

CAMLprim value LFUN(lassq_stub)(
  intnat vN,
  intnat vOFSX, intnat vINCX, value vX,
  double vSCALE, double vSUMSQ)
{
  CAMLparam1(vX);
  CAMLlocal2(v_scl, v_smsq);
  value v_res;

  integer GET_INT(N), GET_INT(INCX);

  VEC_PARAMS(X);

  REAL GET_DOUBLE(SCALE), GET_DOUBLE(SUMSQ);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(lassq)(
    &N,
    X_data, &INCX,
    &SCALE, &SUMSQ);
  caml_leave_blocking_section();  /* Disallow other threads */

  v_scl = caml_copy_double(SCALE);
  v_smsq = caml_copy_double(SUMSQ);

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = v_scl;
  Field(v_res, 1) = v_smsq;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(lassq_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(lassq_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Double_val(argv[4]),
        Double_val(argv[5]));
}


/** LANGE */

extern REAL FUN(lange)(
  char *NORM,
  integer *M, integer *N,
  NUMBER *A, integer *LDA,
  REAL *WORK);

CAMLprim double LFUN(lange_stub)(
  value vNORM,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vWORK)
{
  CAMLparam2(vA, vWORK);

  char GET_CHAR(NORM);
  integer GET_INT(M), GET_INT(N);

  REAL res;

  MAT_PARAMS(A);
  RVEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  res = FUN(lange)(
          &NORM, &M, &N,
          A_data, &rows_A,
          WORK_data);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(res);
}

CAMLprim value LFUN(lange_stub_bc)(value *argv, int __unused argn)
{
  return
    caml_copy_double(
        LFUN(lange_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          argv[6]));
}


/** LAUUM */

extern void FUN(lauum)(
  char *UPLO,
  integer *N,
  NUMBER *A, integer *LDA,
  integer *INFO);

CAMLprim value LFUN(lauum_stub)(
  value vUPLO, intnat vN,
  intnat vAR, intnat vAC, value vA)
{
  CAMLparam1(vA);

  char GET_CHAR(UPLO);
  integer GET_INT(N), INFO;

  MAT_PARAMS(A);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(lauum)(&UPLO, &N, A_data, &rows_A, &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(lauum_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(lauum_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4]);
}


/* Linear Equations (computational routines)
************************************************************************/

/** GETRF */

extern void FUN(getrf)(
  integer *M, integer *N,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  integer *INFO);

CAMLprim intnat LFUN(getrf_stub)(
  intnat vM,
  intnat vN,
  intnat vAR,
  intnat vAC,
  value vA,
  value vIPIV)
{
  CAMLparam2(vA, vIPIV);

  integer GET_INT(M), GET_INT(N), INFO;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(getrf)(
    &M, &N,
    A_data, &rows_A,
    IPIV_data,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(getrf_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(getrf_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          argv[4],
          argv[5]));
}

/** GETRS */

extern void FUN(getrs)(
  char *TRANS,
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(getrs_stub)(
  value vTRANS,
  intnat vN,
  intnat vNRHS,
  intnat vAR,
  intnat vAC,
  value vA,
  intnat vBR,
  intnat vBC,
  value vB,
  value vIPIV)
{
  CAMLparam3(vA, vB, vIPIV);

  char GET_CHAR(TRANS);
  integer GET_INT(N), GET_INT(NRHS), INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  INT_VEC_PARAMS(IPIV);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(getrs)(
    &TRANS,
    &N, &NRHS,
    A_data, &rows_A,
    IPIV_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(getrs_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(getrs_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8],
          argv[9]));
}

/** GETRI */

extern void FUN(getri)(
  integer *N,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *WORK, integer *LWORK,
  integer *INFO);

CAMLprim intnat LFUN(getri_stub)(
  intnat vN,
  intnat vAR,
  intnat vAC,
  value vA,
  value vIPIV,
  value vWORK,
  intnat vLWORK)
{
  CAMLparam3(vA, vIPIV, vWORK);

  integer GET_INT(N), GET_INT(LWORK), INFO;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(getri)(
    &N, A_data, &rows_A,
    IPIV_data,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(getri_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(getri_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          Int_val(argv[2]),
          argv[3],
          argv[4],
          argv[5],
          Int_val(argv[6])));
}

/** SYTRF */

extern void FUN(sytrf)(
  char *UPLO, integer *N,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *WORK, integer *LWORK,
  integer *INFO);

CAMLprim intnat LFUN(sytrf_stub)(
  value vUPLO,
  intnat vN,
  intnat vAR,
  intnat vAC,
  value vA,
  value vIPIV,
  value vWORK,
  intnat vLWORK)
{
  CAMLparam3(vA, vIPIV, vWORK);

  char GET_CHAR(UPLO);
  integer GET_INT(N), GET_INT(LWORK), INFO;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sytrf)(
    &UPLO, &N,
    A_data, &rows_A,
    IPIV_data,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(sytrf_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
    LFUN(sytrf_stub)(
      argv[0],
      Int_val(argv[1]),
      Int_val(argv[2]),
      Int_val(argv[3]),
      argv[4],
      argv[5],
      argv[6],
      Int_val(argv[7])));
}

/** SYTRS */

extern void FUN(sytrs)(
  char *UPLO,
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(sytrs_stub)(
  value vUPLO,
  intnat vN,
  intnat vNRHS,
  intnat vAR,
  intnat vAC,
  value vA,
  intnat vBR,
  intnat vBC,
  value vB,
  value vIPIV)
{
  CAMLparam3(vA, vB, vIPIV);

  char GET_CHAR(UPLO);
  integer GET_INT(N), GET_INT(NRHS), INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  INT_VEC_PARAMS(IPIV);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sytrs)(
    &UPLO,
    &N, &NRHS,
    A_data, &rows_A,
    IPIV_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(sytrs_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(sytrs_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8],
          argv[9]));
}

/** SYTRI */

extern void FUN(sytri)(
  char *UPLO,
  integer *N,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *WORK,
  integer *INFO);

CAMLprim intnat LFUN(sytri_stub)(
  value vUPLO,
  intnat vN,
  intnat vAR,
  intnat vAC,
  value vA,
  value vIPIV,
  value vWORK)
{
  CAMLparam3(vA, vIPIV, vWORK);

  char GET_CHAR(UPLO);
  integer GET_INT(N), INFO;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sytri)(
    &UPLO,
    &N, A_data, &rows_A,
    IPIV_data,
    WORK_data,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(sytri_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(sytri_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          argv[4],
          argv[5],
          argv[6]));
}

/** POTRF */

extern void FUN(potrf)(
  char *UPLO, integer *N,
  NUMBER *A, integer *LDA,
  integer *INFO);

CAMLprim intnat LFUN(potrf_stub)(
  value vUPLO,
  intnat vN,
  intnat vAR,
  intnat vAC,
  value vA)
{
  CAMLparam1(vA);

  char GET_CHAR(UPLO);
  integer GET_INT(N), INFO;

  MAT_PARAMS(A);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(potrf)(&UPLO, &N, A_data, &rows_A, &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(potrf_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(potrf_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          argv[4]));
}

/** POTRS */

extern void FUN(potrs)(
  char *UPLO,
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(potrs_stub)(
  value vUPLO,
  intnat vN, intnat vNRHS,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  char GET_CHAR(UPLO);
  integer GET_INT(N), GET_INT(NRHS), INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(potrs)(
    &UPLO,
    &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(potrs_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(potrs_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8]));
}

/** POTRI */

extern void FUN(potri)(
  char *UPLO, integer *N,
  NUMBER *A, integer *LDA,
  integer *INFO);

CAMLprim intnat LFUN(potri_stub)(
  value vUPLO,
  intnat vN,
  intnat vAR, intnat vAC, value vA)
{
  CAMLparam1(vA);

  char GET_CHAR(UPLO);
  integer GET_INT(N), INFO;

  MAT_PARAMS(A);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(potri)(&UPLO, &N, A_data, &rows_A, &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(potri_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(potri_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          argv[4]));
}

/** TRTRS */

extern void FUN(trtrs)(
  char *UPLO, char *TRANS, char *DIAG,
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(trtrs_stub)(
  value vUPLO, value vTRANS, value vDIAG,
  intnat vN, intnat vNRHS,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  char GET_CHAR(UPLO), GET_CHAR(TRANS), GET_CHAR(DIAG);
  integer GET_INT(N), GET_INT(NRHS), INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(trtrs)(
    &UPLO, &TRANS, &DIAG,
    &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(trtrs_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(trtrs_stub)(
          argv[0],
          argv[1],
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          Int_val(argv[6]),
          argv[7],
          Int_val(argv[8]),
          Int_val(argv[9]),
          argv[10]));
}

/** TRTRI */

extern void FUN(trtri)(
  char *UPLO, char *DIAG,
  integer *N,
  NUMBER *A, integer *LDA,
  integer *INFO);

CAMLprim intnat LFUN(trtri_stub)(
  value vUPLO, value vDIAG,
  intnat vN,
  intnat vAR, intnat vAC, value vA)
{
  CAMLparam1(vA);

  char GET_CHAR(UPLO), GET_CHAR(DIAG);
  integer GET_INT(N), INFO;

  MAT_PARAMS(A);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(trtri)(&UPLO, &DIAG, &N, A_data, &rows_A, &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(trtri_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(trtri_stub)(
          argv[0],
          argv[1],
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5]));
}

/** TBTRS */

extern void FUN(tbtrs)(
  char *UPLO, char *TRANS, char *DIAG,
  integer *N, integer *KD, integer *NRHS,
  NUMBER *AB, integer *LDAB,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(tbtrs_stub)(
  value vUPLO, value vTRANS, value vDIAG,
  intnat vN, intnat vKD, intnat vNRHS,
  intnat vABR, intnat vABC, value vAB,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vAB, vB);

  char GET_CHAR(UPLO), GET_CHAR(TRANS), GET_CHAR(DIAG);
  integer GET_INT(N), GET_INT(KD), GET_INT(NRHS), INFO;

  MAT_PARAMS(AB);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(tbtrs)(
    &UPLO, &TRANS, &DIAG,
    &N, &KD, &NRHS,
    AB_data, &rows_AB,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(tbtrs_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(tbtrs_stub)(
          argv[0],
          argv[1],
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8],
          Int_val(argv[9]),
          Int_val(argv[10]),
          argv[11]));
}

/** GEQRF */

extern void FUN(geqrf)(
  integer *M,
  integer *N,
  NUMBER *A, integer *LDA,
  NUMBER *TAU,
  NUMBER *WORK, integer *LWORK,
  integer *INFO);

CAMLprim intnat LFUN(geqrf_stub)(
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vTAU,
  value vWORK, intnat vLWORK)
{
  CAMLparam3(vA, vTAU, vWORK);

  integer GET_INT(M), GET_INT(N), GET_INT(LWORK), INFO;

  MAT_PARAMS(A);
  VEC_PARAMS1(TAU);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(geqrf)(
    &M, &N,
    A_data, &rows_A,
    TAU_data,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(geqrf_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(geqrf_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          argv[4],
          argv[5],
          argv[6],
          Int_val(argv[7])));
}


/* Linear Equations (simple drivers)
************************************************************************/

/** GESV */

extern void FUN(gesv)(
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(gesv_stub)(
  intnat vAR, intnat vAC, value vA,
  intnat vN,
  value vIPIV,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam3(vA, vB, vIPIV);

  integer GET_INT(N),
          GET_INT(NRHS),
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  INT_VEC_PARAMS(IPIV);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gesv)(
    &N, &NRHS,
    A_data, &rows_A,
    IPIV_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(gesv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(gesv_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          argv[4],
          Int_val(argv[5]),
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8]));
}


/** GBSV */

extern void FUN(gbsv)(
  integer *N, integer *KL, integer *KU, integer *NRHS,
  NUMBER *AB, integer *LDAB,
  integer *IPIV,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(gbsv_stub)(
  intnat vABR, intnat vABC, value vAB,
  intnat vN, intnat vKL, intnat vKU,
  value vIPIV,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam3(vAB, vB, vIPIV);

  integer GET_INT(N),
          GET_INT(KL),
          GET_INT(KU),
          GET_INT(NRHS),
          INFO;

  MAT_PARAMS(AB);
  MAT_PARAMS(B);
  INT_VEC_PARAMS(IPIV);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gbsv)(
    &N, &KL, &KU, &NRHS,
    AB_data, &rows_AB,
    IPIV_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(gbsv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(gbsv_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          argv[6],
          Int_val(argv[7]),
          Int_val(argv[8]),
          Int_val(argv[9]),
          argv[10]));
}


/** GTSV */

extern void FUN(gtsv)(
  integer *N, integer *NRHS,
  NUMBER *DL, NUMBER *D, NUMBER *DU,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(gtsv_stub)(
  intnat vOFSDL, value vDL,
  intnat vOFSD, value vD,
  intnat vOFSDU, value vDU,
  intnat vN,
  intnat vNRHS,
  intnat vBR,
  intnat vBC,
  value vB)
{
  CAMLparam4(vDL, vD, vDU, vB);

  integer GET_INT(N),
          GET_INT(NRHS),
          INFO;

  VEC_PARAMS(DL);
  VEC_PARAMS(D);
  VEC_PARAMS(DU);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gtsv)(
    &N, &NRHS,
    DL_data, D_data, DU_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(gtsv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(gtsv_stub)(
          Int_val(argv[0]),
          argv[1],
          Int_val(argv[2]),
          argv[3],
          Int_val(argv[4]),
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          Int_val(argv[8]),
          Int_val(argv[9]),
          argv[10]));
}


/** POSV */

extern void FUN(posv)(
  char *UPLO,
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(posv_stub)(
  intnat vAR, intnat vAC, value vA,
  intnat vN,
  value vUPLO,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  char GET_CHAR(UPLO);

  integer GET_INT(N),
          GET_INT(NRHS),
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(posv)(
    &UPLO,
    &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(posv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(posv_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          argv[4],
          Int_val(argv[5]),
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8]));
}


/** PPSV */

extern void FUN(ppsv)(
  char *UPLO,
  integer *N, integer *NRHS,
  NUMBER *AP,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(ppsv_stub)(
  intnat vOFSAP, value vAP,
  intnat vN,
  value vUPLO,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vAP, vB);

  char GET_CHAR(UPLO);

  integer GET_INT(N),
          GET_INT(NRHS),
          INFO;

  VEC_PARAMS(AP);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(ppsv)(
    &UPLO,
    &N, &NRHS,
    AP_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(ppsv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(ppsv_stub)(
          Int_val(argv[0]),
          argv[1],
          Int_val(argv[2]),
          argv[3],
          Int_val(argv[4]),
          Int_val(argv[5]),
          Int_val(argv[6]),
          argv[7]));
}


/** PBSV */

extern void FUN(pbsv)(
  char *UPLO,
  integer *N, integer *KD, integer *NRHS,
  NUMBER *AB, integer *LDAB,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(pbsv_stub)(
  intnat vABR, intnat vABC, value vAB,
  intnat vN,
  intnat vKD,
  value vUPLO,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vAB, vB);

  char GET_CHAR(UPLO);

  integer GET_INT(N),
          GET_INT(KD),
          GET_INT(NRHS),
          INFO;

  MAT_PARAMS(AB);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(pbsv)(
    &UPLO,
    &N, &KD, &NRHS,
    AB_data, &rows_AB,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(pbsv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(pbsv_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          Int_val(argv[8]),
          argv[9]));
}


/** PTSV */

extern void FUN(ptsv)(
  integer *N, integer *NRHS,
  REAL *D, NUMBER *E,
  NUMBER *B, integer *LDB,
  integer *INFO);

CAMLprim intnat LFUN(ptsv_stub)(
  intnat vOFSD, value vD,
  intnat vOFSE, value vE,
  intnat vN,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam3(vD, vE, vB);

  integer GET_INT(N),
          GET_INT(NRHS),
          INFO;

  RVEC_PARAMS(D);
  VEC_PARAMS(E);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(ptsv)(
    &N, &NRHS,
    D_data, E_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(ptsv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(ptsv_stub)(
          Int_val(argv[0]),
          argv[1],
          Int_val(argv[2]),
          argv[3],
          Int_val(argv[4]),
          Int_val(argv[5]),
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8]));
}


/** SYSV */

extern void FUN(sysv)(
  char *UPLO,
  integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  integer *IPIV,
  NUMBER *B, integer *LDB,
  NUMBER *WORK, integer *LWORK,
  integer *INFO);

CAMLprim intnat LFUN(sysv_stub)(
  intnat vAR, intnat vAC, value vA,
  intnat vN,
  value vUPLO,
  value vIPIV,
  value vWORK,
  intnat vLWORK,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam4(vA, vIPIV, vWORK, vB);

  char GET_CHAR(UPLO);

  integer GET_INT(N),
          GET_INT(LWORK),
          GET_INT(NRHS),
          INFO;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);
  VEC_PARAMS1(WORK);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sysv)(
    &UPLO,
    &N, &NRHS,
    A_data, &rows_A,
    IPIV_data,
    B_data, &rows_B,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(sysv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(sysv_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          argv[4],
          argv[5],
          argv[6],
          Int_val(argv[7]),
          Int_val(argv[8]),
          Int_val(argv[9]),
          Int_val(argv[10]),
          argv[11]));
}


/** SPSV */

extern void FUN(spsv)(
  char *UPLO,
  integer *N, integer *NRHS,
  NUMBER *AP,
  integer *IPIV,
  NUMBER *B, integer *LDB,
  integer *INFO);


CAMLprim intnat LFUN(spsv_stub)(
  intnat vOFSAP, value vAP,
  intnat vN,
  value vUPLO,
  value vIPIV,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam3(vAP, vIPIV, vB);

  char GET_CHAR(UPLO);

  integer GET_INT(N),
          GET_INT(NRHS),
          INFO;

  VEC_PARAMS(AP);
  INT_VEC_PARAMS(IPIV);
  MAT_PARAMS(B);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(spsv)(
    &UPLO,
    &N, &NRHS,
    AP_data,
    IPIV_data,
    B_data, &rows_B,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(spsv_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(spsv_stub)(
          Int_val(argv[0]),
          argv[1],
          Int_val(argv[2]),
          argv[3],
          argv[4],
          Int_val(argv[5]),
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8]));
}


/* Linear Equations (expert drivers)
************************************************************************/

/** TODO: GESVX */

/** TODO: GBSVX */

/** TODO: GTSVX */

/** TODO: POSVX */

/** TODO: PPSVX */

/** TODO: PBSVX */

/** TODO: PTSVX */

/** TODO: SYSVX */

/** TODO: SPSVX */


/* Least squares (simple drivers)
************************************************************************/

/** GELS */

extern void FUN(gels)(
  char *TRANS,
  integer *M, integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  NUMBER *WORK, integer *LWORK,
  integer *INFO);

CAMLprim intnat LFUN(gels_stub)(
  intnat vAR, intnat vAC, value vA,
  intnat vM, intnat vN,
  value vTRANS,
  value vWORK,
  intnat vLWORK,
  intnat vNRHS,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam3(vA, vB, vWORK);

  char GET_CHAR(TRANS);

  integer GET_INT(M),
          GET_INT(N),
          GET_INT(LWORK),
          GET_INT(NRHS),
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gels)(
    &TRANS,
    &M, &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(INFO);
}

CAMLprim value LFUN(gels_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(gels_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          argv[6],
          Int_val(argv[7]),
          Int_val(argv[8]),
          Int_val(argv[9]),
          Int_val(argv[10]),
          argv[11]));
}


/** GGLSE */


/* Standard eigenvalue and singular value problems (simple drivers)
************************************************************************/

/** TODO: GEES */


/* Generalized eigenvalue and singular value problems (simple drivers)
************************************************************************/

/** TODO: GGES */

/** TODO: GGEV */

/** TODO: GGSVD */


/* Generalized eigenvalue and singular value problems (expert drivers)
************************************************************************/

/** TODO: GEESX */

/** TODO: GGESX */

/** TODO: GEEVX */

/** TODO: GGEVX */
