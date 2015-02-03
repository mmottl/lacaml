/* File: impl_SD_c.c

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

     Florent Hoareau
     email: h.florent@gmail.com
     WWW: none

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

/*** BLAS-1 */

/** DOT */

extern REAL FUN(dot)(
  integer *N,
  REAL *X, integer *INCX,
  REAL *Y, integer *INCY);

CAMLprim value LFUN(dot_stub)(
  value vN,
  value vOFSY, value vINCY, value vY,
  value vOFSX, value vINCX, value vX)
{
  CAMLparam2(vX, vY);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  REAL res;

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  res =
    FUN(dot)(
      &N,
      X_data, &INCX,
      Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(caml_copy_double(res));
}

CAMLprim value LFUN(dot_stub_bc)(value *argv, int __unused argn)
{
  return LFUN(dot_stub)(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}


/** ASUM */

extern REAL FUN(asum)(integer *N, REAL *X, integer *INCX);

CAMLprim value LFUN(asum_stub)(value vN, value vOFSX, value vINCX, value vX)
{
  CAMLparam1(vX);

  integer GET_INT(N),
          GET_INT(INCX);

  REAL res;

  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  res = FUN(asum)(&N, X_data, &INCX);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(caml_copy_double(res));
}



/*** BLAS-2 */

/** SBMV */

extern void FUN(sbmv)(
  char *UPLO,
  integer *N, integer *K,
  REAL *ALPHA,
  REAL *A, integer *LDA,
  REAL *X, integer *INCX,
  REAL *BETA,
  REAL *Y, integer *INCY);

CAMLprim value LFUN(sbmv_stub)(
  value vOFSY, value vINCY, value vY,
  value vAR,
  value vAC,
  value vA,
  value vN, value vK,
  value vUPLO,
  value vALPHA,
  value vBETA,
  value vOFSX, value vINCX, value vX)
{
  CAMLparam3(vA, vX, vY);

  char GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(K),
          GET_INT(INCX),
          GET_INT(INCY);

  REAL GET_DOUBLE(ALPHA),
       GET_DOUBLE(BETA);

  MAT_PARAMS(A);
  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sbmv)(
    &UPLO,
    &N, &K,
    &ALPHA,
    A_data, &rows_A,
    X_data, &INCX,
    &BETA,
    Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(sbmv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(sbmv_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7], argv[8], argv[9], argv[10], argv[11], argv[12], argv[13]);
}


/** GER */

extern void FUN(ger)(
  integer *M,
  integer *N,
  REAL *ALPHA,
  REAL *X, integer *INCX,
  REAL *Y, integer *INCY,
  REAL *A, integer *LDA);

CAMLprim value LFUN(ger_stub)(
  value vM, value vN,
  value vALPHA,
  value vOFSX, value vINCX, value vX,
  value vOFSY, value vINCY, value vY,
  value vAR, value vAC, value vA)
{
  CAMLparam3(vA, vX, vY);

  integer GET_INT(M), GET_INT(N), GET_INT(INCX), GET_INT(INCY);

  REAL GET_DOUBLE(ALPHA);

  MAT_PARAMS(A);
  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(ger)(
    &M,
    &N,
    &ALPHA,
    X_data, &INCX,
    Y_data, &INCY,
    A_data, &rows_A);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(ger_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(ger_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7], argv[8], argv[9], argv[10], argv[11]);
}


/** SYR */

extern void FUN(syr)(
  char *UPLO,
  integer *N,
  REAL *ALPHA,
  REAL *X, integer *INCX,
  REAL *A, integer *LDA);

CAMLprim value LFUN(syr_stub)(
  value vUPLO,
  value vN,
  value vALPHA,
  value vOFSX, value vINCX, value vX,
  value vAR, value vAC, value vA)
{
  CAMLparam2(vA, vX);

  char GET_INT(UPLO);
  integer GET_INT(N),
          GET_INT(INCX);

  REAL GET_DOUBLE(ALPHA);

  MAT_PARAMS(A);
  VEC_PARAMS(X);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(syr)(
    &UPLO,
    &N,
    &ALPHA,
    X_data, &INCX,
    A_data, &rows_A);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(syr_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syr_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7], argv[8]);
}


/*** LAPACK */

/** LANSY */

extern REAL FUN(lansy)(
  char *NORM, char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  REAL *WORK);

CAMLprim value LFUN(lansy_stub)(
  value vNORM,
  value vUPLO,
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vWORK)
{
  CAMLparam2(vA, vWORK);

  char GET_INT(NORM), GET_INT(UPLO);
  integer GET_INT(N);

  REAL res;

  MAT_PARAMS(A);
  RVEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  res = FUN(lansy)(
    &NORM, &UPLO, &N,
    A_data, &rows_A,
    WORK_data);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(caml_copy_double(res));
}

CAMLprim value LFUN(lansy_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(lansy_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}


/** LAMCH */

extern REAL FUN(lamch)(char *CMACH);

CAMLprim value LFUN(lamch_stub)(value vCMACH)
{
  char GET_INT(CMACH);
  REAL res = FUN(lamch)(&CMACH);
  return caml_copy_double(res);
}


/** ORGQR */

extern void FUN(orgqr)(
  integer *M,
  integer *N,
  integer *K,
  REAL *A, integer *LDA,
  REAL *TAU,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(orgqr_stub)(
  value vM,
  value vN,
  value vK,
  value vWORK,
  value vLWORK,
  value vTAU,
  value vAR,
  value vAC,
  value vA)
{
  CAMLparam2(vTAU, vA);

  integer GET_INT(M), GET_INT(N), GET_INT(K), GET_INT(LWORK), INFO;

  VEC_PARAMS1(WORK);
  VEC_PARAMS1(TAU);
  MAT_PARAMS(A);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(orgqr)(
    &M, &N, &K,
    A_data, &rows_A,
    TAU_data,
    WORK_data, &LWORK, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_int(INFO));
}

CAMLprim value LFUN(orgqr_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(orgqr_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4],
      argv[5], argv[6], argv[7], argv[8]);
}


/** ORMQR */

extern void FUN(ormqr)(
  char *SIDE,
  char *TRANS,
  integer *M,
  integer *N,
  integer *K,
  REAL *A, integer *LDA,
  REAL *TAU,
  REAL *C, integer *LDC,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(ormqr_stub)(
  value vSIDE,
  value vTRANS,
  value vM,
  value vN,
  value vK,
  value vWORK,
  value vLWORK,
  value vTAU,
  value vAR,
  value vAC,
  value vA,
  value vCR,
  value vCC,
  value vC)
{
  CAMLparam3(vTAU, vA, vC);

  char SIDE = GET_INT(SIDE), GET_INT(TRANS);
  integer GET_INT(M), GET_INT(N), GET_INT(K), GET_INT(LWORK), INFO;

  VEC_PARAMS1(WORK);
  VEC_PARAMS1(TAU);
  MAT_PARAMS(A);
  MAT_PARAMS(C);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(ormqr)(
    &SIDE, &TRANS, &M, &N, &K,
    A_data, &rows_A,
    TAU_data,
    C_data, &rows_C,
    WORK_data, &LWORK, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_int(INFO));
}

CAMLprim value LFUN(ormqr_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(ormqr_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4],
      argv[5], argv[6], argv[7], argv[8], argv[9],
      argv[10], argv[11], argv[12], argv[13]);
}


/** GECON */

extern void FUN(gecon)(
  char *NORM,
  integer *N,
  REAL *A, integer *LDA,
  REAL *ANORM, REAL *RCOND,
  REAL *WORK, integer *IWORK,
  integer *INFO);

CAMLprim value LFUN(gecon_stub)(
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vWORK,
  value vIWORK,
  value vNORM,
  value vANORM)
{
  CAMLparam3(vA, vWORK, vIWORK);
  CAMLlocal1(v_rcond);

  char GET_INT(NORM);
  integer GET_INT(N), INFO;
  REAL GET_DOUBLE(ANORM), RCOND;

  value v_res;

  MAT_PARAMS(A);
  VEC_PARAMS1(WORK);
  INT_VEC_PARAMS(IWORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gecon)(
    &NORM, &N,
    A_data, &rows_A,
    &ANORM, &RCOND,
    WORK_data, IWORK_data, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  v_rcond = caml_copy_double(RCOND);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = v_rcond;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(gecon_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gecon_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7]);
}

/** SYCON */

extern void FUN(sycon)(
  char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  integer *IPIV,
  REAL *ANORM, REAL *RCOND,
  REAL *WORK, integer *IWORK,
  integer *INFO);

CAMLprim value LFUN(sycon_stub)(
  value vUPLO,
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vIPIV,
  value vWORK,
  value vIWORK,
  value vANORM)
{
  CAMLparam4(vA, vIPIV, vWORK, vIWORK);
  CAMLlocal1(v_rcond);

  char GET_INT(UPLO);
  integer GET_INT(N), INFO;
  REAL GET_DOUBLE(ANORM), RCOND;

  value v_res;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);
  VEC_PARAMS1(WORK);
  INT_VEC_PARAMS(IWORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sycon)(
    &UPLO, &N,
    A_data, &rows_A,
    IPIV_data,
    &ANORM, &RCOND,
    WORK_data, IWORK_data, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  v_rcond = caml_copy_double(RCOND);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = v_rcond;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(sycon_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(sycon_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7], argv[8]);
}

/** POCON */

extern void FUN(pocon)(
  char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  REAL *ANORM, REAL *RCOND,
  REAL *WORK, integer *IWORK,
  integer *INFO);

CAMLprim value LFUN(pocon_stub)(
  value vUPLO,
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vWORK,
  value vIWORK,
  value vANORM)
{
  CAMLparam3(vA, vWORK, vIWORK);
  CAMLlocal1(v_rcond);

  char GET_INT(UPLO);
  integer GET_INT(N), INFO;
  REAL GET_DOUBLE(ANORM), RCOND;

  value v_res;

  MAT_PARAMS(A);
  VEC_PARAMS1(WORK);
  INT_VEC_PARAMS(IWORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(pocon)(
    &UPLO, &N,
    A_data, &rows_A,
    &ANORM, &RCOND,
    WORK_data, IWORK_data, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  v_rcond = caml_copy_double(RCOND);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = v_rcond;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(pocon_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(pocon_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7]);
}

/* Least squares (expert drivers)
************************************************************************/

/** GELSY */

extern void FUN(gelsy)(
  integer *M, integer *N, integer *NRHS,
  REAL *A, integer *LDA,
  REAL *B, integer *LDB,
  integer *JPVT, REAL *RCOND, integer *RANK,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(gelsy_stub)(
  value vAR,
  value vAC,
  value vA,
  value vM,
  value vN,
  value vJPVT,
  value vRCOND,
  value vWORK,
  value vLWORK,
  value vNRHS,
  value vBR,
  value vBC,
  value vB)
{
  CAMLparam4(vA, vB, vJPVT, vWORK);

  integer GET_INT(M),
          GET_INT(N),
          GET_INT(LWORK),
          GET_INT(NRHS),
          RANK,
          INFO;

  REAL GET_DOUBLE(RCOND);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  INT_VEC_PARAMS(JPVT);
  VEC_PARAMS1(WORK);

  value v_res;

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gelsy)(
    &M, &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    JPVT_data, &RCOND, &RANK,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = Val_long(RANK);

  CAMLreturn(v_res);
}

CAMLprim value LFUN(gelsy_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gelsy_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11], argv[12]);
}


/** GELSD */

extern void FUN(gelsd)(
  integer *M, integer *N, integer *NRHS,
  REAL *A, integer *LDA,
  REAL *B, integer *LDB,
  REAL *S, REAL *RCOND, integer *RANK,
  REAL *WORK, integer *LWORK, REAL *IWORK,
  integer *INFO);

CAMLprim value LFUN(gelsd_stub)(
  value vAR,
  value vAC,
  value vA,
  value vM,
  value vN,
  value vOFSS, value vS,
  value vRCOND,
  value vWORK,
  value vLWORK,
  value vIWORK,
  value vNRHS,
  value vBR,
  value vBC,
  value vB)
{
  CAMLparam5(vA, vB, vS, vWORK, vIWORK);

  integer GET_INT(M),
          GET_INT(N),
          GET_INT(LWORK),
          GET_INT(NRHS),
          RANK,
          INFO;

  REAL GET_DOUBLE(RCOND);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  VEC_PARAMS(S);
  VEC_PARAMS1(WORK);
  VEC_PARAMS1(IWORK);

  value v_res;

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gelsd)(
    &M, &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    S_data, &RCOND, &RANK,
    WORK_data, &LWORK,
    IWORK_data,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = Val_long(RANK);

  CAMLreturn(v_res);
}

CAMLprim value LFUN(gelsd_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gelsd_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11], argv[12], argv[13], argv[14]);
}


/** GELSS */

extern void FUN(gelss)(
  integer *M, integer *N, integer *NRHS,
  NUMBER *A, integer *LDA,
  NUMBER *B, integer *LDB,
  REAL *S, REAL *RCOND, integer *RANK,
  NUMBER *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(gelss_stub)(
  value vAR,
  value vAC,
  value vA,
  value vM,
  value vN,
  value vOFSS, value vS,
  value vRCOND,
  value vWORK,
  value vLWORK,
  value vNRHS,
  value vBR,
  value vBC,
  value vB)
{
  CAMLparam4(vA, vB, vS, vWORK);

  integer GET_INT(M),
          GET_INT(N),
          GET_INT(LWORK),
          GET_INT(NRHS),
          RANK,
          INFO;

  REAL RCOND = Double_val(vRCOND);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  RVEC_PARAMS(S);
  VEC_PARAMS1(WORK);

  value v_res;

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gelss)(
    &M, &N, &NRHS,
    A_data, &rows_A,
    B_data, &rows_B,
    S_data, &RCOND, &RANK,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = Val_long(RANK);

  CAMLreturn(v_res);
}

CAMLprim value LFUN(gelss_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gelss_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11], argv[12], argv[13]);
}

/* General Schur factorization
************************************************************************/

/** GEES */

/* Predefined callbacks for eigenvalue selection */

static inline integer select_left_plane(
  const REAL *re_ptr, const REAL *im_ptr __attribute__((unused)))
{
  return (*re_ptr < 0) ? 1 : 0;
}

static inline integer select_right_plane(
  const REAL *re_ptr, const REAL *im_ptr __attribute__((unused)) )
{
  return (*re_ptr > 0) ? 1 : 0;
}

static inline integer select_disk_interior(
  const REAL *re_ptr, const REAL *im_ptr)
{
  REAL re = *re_ptr;
  REAL im = *im_ptr;
  return (re * re + im * im < 1) ? 1 : 0;
}

static inline integer select_disk_exterior(
  const REAL *re_ptr, const REAL *im_ptr)
{
  REAL re = *re_ptr;
  REAL im = *im_ptr;
  return (re * re + im * im > 1) ? 1 : 0;
}

/* Custom callback handling for eigenvalue selection */

static value select_ocaml_callback = Val_unit;
static value select_ocaml_callback_exn = Val_unit;
static bool select_ocaml_locked_runtime = false;

CAMLprim value LFUN(init_gees)(value __unused v_unit)
{
  caml_register_generational_global_root(&select_ocaml_callback);
  caml_register_generational_global_root(&select_ocaml_callback_exn);
  return Val_unit;
}

static integer select_ocaml_exec_callback(
  const REAL *re_ptr, const REAL *im_ptr)
{
  value v_res, v_arg;

  if (!select_ocaml_locked_runtime) {
    caml_leave_blocking_section(); /* Disallow other threads */
    select_ocaml_locked_runtime = true;
  }

  v_arg = caml_alloc_small(2, Double_array_tag);
  Store_double_field(v_arg, 0, (double) (*re_ptr));
  Store_double_field(v_arg, 1, (double) (*im_ptr));

  v_res = caml_callback_exn(select_ocaml_callback, v_arg);

  if (!Is_exception_result(v_res)) return Bool_val(v_res);
  else {
    /* Callout raised an exception */
    if (select_ocaml_callback_exn == Val_unit) {
      value v_exn = Extract_exception(v_res);
      caml_modify_generational_global_root(&select_ocaml_callback_exn, v_exn);
    }
    return 0;
  }
}

typedef integer (*LAPACK_SELECT2)(const REAL *, const REAL *);

extern void FUN(gees)(
  char *JOBVS, char *SORT,
  LAPACK_SELECT2 SELECT,
  integer *N,
  REAL *A, integer *LDA,
  integer *SDIM,
  REAL *WR, REAL *WI,
  REAL *VS, integer *LDVS,
  REAL *WORK,
  integer *LWORK,
  integer *BWORK,
  integer *INFO);

CAMLprim value LFUN(gees_stub)(
  value vJOBVS, value vSORT,
  value vSELECT, value vSELECT_FUN,
  value vN,
  value vAR, value vAC, value vA,
  value vWR, value vWI,
  value vVSR, value vVSC, value vVS,
  value vWORK,
  value vLWORK,
  value vBWORK)
{
  CAMLparam5(vA, vVS, vWI, vWR, vWORK);
  CAMLxparam2(vBWORK, vSELECT_FUN);
  CAMLlocal1(v_res);

  char GET_INT(JOBVS),
       GET_INT(SORT);

  integer GET_INT(SELECT),
          GET_INT(N),
          GET_INT(LWORK),
          SDIM,
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(VS);
  VEC_PARAMS1(WI);
  VEC_PARAMS1(WORK);
  VEC_PARAMS1(WR);
  INT_VEC_PARAMS(BWORK);

  LAPACK_SELECT2 select_function = NULL;
  bool custom_sort = false;

  if (SORT == 'S') {
    switch (SELECT) {
      case 0 :
        select_function = select_left_plane;
        break;
      case 1 :
        select_function = select_right_plane;
        break;
      case 2 :
        select_function = select_disk_interior;
        break;
      case 3 :
        select_function = select_disk_exterior;
        break;
      case 4 :
        custom_sort = true;
        select_function = select_ocaml_exec_callback;
        while (select_ocaml_callback != Val_unit) {
          caml_enter_blocking_section(); /* Allow other threads */
          /* Wait 1ms before polling again */
          portable_sleep(1);
          caml_leave_blocking_section(); /* Disallow other threads */
        }
        caml_modify_generational_global_root(
          &select_ocaml_callback, vSELECT_FUN);
        break;
      default :
        caml_failwith("internal error: unknown SELECT value in gees_stub");
    }
  }

  caml_enter_blocking_section(); /* Allow other threads */

  FUN(gees)(
    &JOBVS, &SORT,
    select_function,
    &N,
    A_data, &rows_A,
    &SDIM,
    WR_data, WI_data,
    VS_data, &rows_VS,
    WORK_data,
    &LWORK, BWORK_data,
    &INFO);

  if (custom_sort) {
    if (select_ocaml_locked_runtime) select_ocaml_locked_runtime = false;
    else caml_leave_blocking_section(); /* Disallow other threads */
    caml_modify_generational_global_root(&select_ocaml_callback, Val_unit);
    if (select_ocaml_callback_exn != Val_unit) {
      CAMLlocal1(v_exn);
      v_exn = select_ocaml_callback_exn;
      caml_modify_generational_global_root(
        &select_ocaml_callback_exn, Val_unit);
      caml_raise(v_exn);
    }
  } else caml_leave_blocking_section(); /* Disallow other threads */

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(SDIM);
  Field(v_res, 1) = Val_long(INFO);

  CAMLreturn(v_res);
}

CAMLprim value LFUN(gees_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gees_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
      argv[12], argv[13], argv[14], argv[15]);
}

/* General SVD routines
************************************************************************/

/** GESVD */

extern void FUN(gesvd)(
  char *JOBU, char *JOBVT,
  integer *M, integer *N,
  REAL *A, integer *LDA,
  REAL *S,
  REAL *U, integer *LDU,
  REAL *VT, integer *LDVT,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(gesvd_stub)(
  value vJOBU, value vJOBVT,
  value vM, value vN,
  value vAR, value vAC, value vA,
  value vS,
  value vUR, value vUC, value vU,
  value vVTR, value vVTC, value vVT,
  value vWORK, value vLWORK)
{
  CAMLparam5(vA, vS, vU, vVT, vWORK);

  char GET_INT(JOBU),
       GET_INT(JOBVT);

  integer GET_INT(M), GET_INT(N),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  VEC_PARAMS1(S);
  MAT_PARAMS(U);
  MAT_PARAMS(VT);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section(); /* Allow other threads */
  FUN(gesvd)(
    &JOBU, &JOBVT,
    &M, &N,
    A_data, &rows_A,
    S_data,
    U_data, &rows_U,
    VT_data, &rows_VT,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(gesvd_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gesvd_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
      argv[12], argv[13], argv[14], argv[15]);
}


/** GESDD */

extern void FUN(gesdd)(
  char *JOBZ,
  integer *M, integer *N,
  REAL *A, integer *LDA,
  REAL *S,
  REAL *U, integer *LDU,
  REAL *VT, integer *LDVT,
  REAL *WORK, integer *LWORK,
  integer *IWORK,
  integer *INFO);

CAMLprim value LFUN(gesdd_stub)(
  value vJOBZ,
  value vM, value vN,
  value vAR, value vAC, value vA,
  value vS,
  value vUR, value vUC, value vU,
  value vVTR, value vVTC, value vVT,
  value vWORK, value vLWORK,
  value vIWORK)
{
  CAMLparam5(vA, vS, vU, vVT, vWORK);
  CAMLxparam1(vIWORK);

  char GET_INT(JOBZ);

  integer GET_INT(M), GET_INT(N),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  VEC_PARAMS1(S);
  MAT_PARAMS(U);
  MAT_PARAMS(VT);
  VEC_PARAMS1(WORK);
  INT_VEC_PARAMS(IWORK);

  caml_enter_blocking_section(); /* Allow other threads */
  FUN(gesdd)(
    &JOBZ,
    &M, &N,
    A_data, &rows_A,
    S_data,
    U_data, &rows_U,
    VT_data, &rows_VT,
    WORK_data, &LWORK,
    IWORK_data,
    &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(gesdd_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gesdd_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
      argv[12], argv[13], argv[14], argv[15]);
}


/* General eigenvalue problem (simple drivers)
************************************************************************/

/** GEEV */

extern void FUN(geev)(
  char *JOBVL, char *JOBVR,
  integer *N,
  REAL *A, integer *LDA,
  REAL *WR, REAL* WI,
  REAL *VL, integer *LDVL,
  REAL *VR, integer *LDVR,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(geev_stub)(
  value vAR, value vAC, value vA,
  value vN,
  value vOFSWR, value vWR,
  value vOFSWI, value vWI,
  value vVLR, value vVLC, value vVL,
  value vJOBVL,
  value vVRR, value vVRC, value vVR,
  value vJOBVR,
  value vWORK, value vLWORK)
{
  CAMLparam5(vA, vWR, vWI, vVL, vVR);
  CAMLxparam1(vWORK);

  char GET_INT(JOBVL),
       GET_INT(JOBVR);

  integer GET_INT(N),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  VEC_PARAMS(WR);
  VEC_PARAMS(WI);
  MAT_PARAMS(VL);
  MAT_PARAMS(VR);
  VEC_PARAMS1(WORK);

  /* weird GEEV requirement:
   * even when the arrays are not
   * referenced, LD's have to be >= 1 */
  if (JOBVL == 'N') rows_VL = 1;
  if (JOBVR == 'N') rows_VR = 1;

  caml_enter_blocking_section(); /* Allow other threads */
  FUN(geev)(
    &JOBVL, &JOBVR,
    &N,
    A_data, &rows_A,
    WR_data, WI_data,
    VL_data, &rows_VL,
    VR_data, &rows_VR,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(geev_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(geev_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
      argv[12], argv[13], argv[14], argv[15], argv[16], argv[17]);
}


/* Symmetric-matrix eigenvalue and singular value problems (simple drivers)
************************************************************************/

/** SYEV */

extern void FUN(syev)(
  char *JOBZ, char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  REAL *W,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(syev_stub)(
  value vAR,
  value vAC,
  value vA,
  value vN,
  value vJOBZ, value vUPLO,
  value vOFSW, value vW,
  value vWORK, value vLWORK)
{
  CAMLparam3(vA, vW, vWORK);

  char GET_INT(JOBZ),
       GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  VEC_PARAMS(W);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(syev)(
    &JOBZ, &UPLO,
    &N,
    A_data, &rows_A,
    W_data,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(syev_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syev_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9]);
}


/** SYEVD */

extern void FUN(syevd)(
  char *JOBZ, char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  REAL *W,
  REAL *WORK, integer *LWORK,
  integer *IWORK, integer *LIWORK,
  integer *INFO);

CAMLprim value LFUN(syevd_stub)(
  value vAR,
  value vAC,
  value vA,
  value vN,
  value vJOBZ, value vUPLO,
  value vOFSW, value vW,
  value vWORK, value vLWORK,
  value vIWORK, value vLIWORK)
{
  CAMLparam4(vA, vW, vWORK, vIWORK);

  char GET_INT(JOBZ),
       GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(LWORK),
          GET_INT(LIWORK),
          INFO;

  MAT_PARAMS(A);
  VEC_PARAMS(W);
  VEC_PARAMS1(WORK);
  INT_VEC_PARAMS(IWORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(syevd)(
    &JOBZ, &UPLO,
    &N,
    A_data, &rows_A,
    W_data,
    WORK_data, &LWORK,
    IWORK_data, &LIWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(syevd_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syevd_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11]);
}

/** TODO: SPEV */

/** TODO: SPEVD */

/** SBEV */

extern void FUN(sbev)(
  char *JOBZ, char *UPLO,
  integer *N,
  integer *KD,
  REAL *AB, integer *LDAB,
  REAL *W,
  REAL *Z, integer *LDZ,
  REAL *WORK,
  integer *INFO);

CAMLprim value LFUN(sbev_stub)(
  value vABR, value vABC, value vAB,
  value vN, value vKD,
  value vJOBZ, value vUPLO,
  value vOFSW, value vW,
  value vZR, value vZC, value vZ, value vLDZ,
  value vWORK)
{
  CAMLparam4(vAB, vW, vZ, vWORK);

  char GET_INT(JOBZ),
       GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(KD),
          GET_INT(LDZ),
          INFO;

  MAT_PARAMS(AB);
  MAT_PARAMS(Z);
  VEC_PARAMS(W);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sbev)(&JOBZ, &UPLO,
            &N,
            &KD,
            AB_data, &rows_AB,
            W_data,
            Z_data, &LDZ,
            WORK_data,
            &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(sbev_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(sbev_stub)(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                    argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
                    argv[12], argv[13]);
}


/** TODO: SBEVD */

/** TODO: STEV */

/** TODO: STEVD */


/* Symmetric-matrix eigenvalue and singular value problems (expert &
   RRR drivers)
************************************************************************/

/** SYEVR */

extern void FUN(syevr)(
  char *JOBZ, char *RANGE, char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  REAL *VL, REAL *VU,
  integer *IL, integer *IU,
  REAL *ABSTOL,
  integer *M,
  REAL *W,
  REAL *Z, integer *LDZ,
  integer *ISUPPZ,
  REAL *WORK, integer *LWORK,
  integer *IWORK, integer *LIWORK,
  integer *INFO);

CAMLprim value LFUN(syevr_stub)(
  value vAR, value vAC, value vA,
  value vN,
  value vJOBZ, value vRANGE, value vUPLO,
  value vVL, value vVU,
  value vIL, value vIU,
  value vABSTOL,
  value vOFSW, value vW,
  value vZR, value vZC, value vZ,
  value vISUPPZ,
  value vWORK, value vLWORK,
  value vIWORK, value vLIWORK)
{
  CAMLparam5(vA, vW, vZ, vISUPPZ, vWORK);
  CAMLxparam1(vIWORK);

  char GET_INT(JOBZ),
       GET_INT(RANGE),
       GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(IL),
          GET_INT(IU),
          GET_INT(LWORK),
          GET_INT(LIWORK),
          M,
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(Z);

  VEC_PARAMS(W);
  VEC_PARAMS1(WORK);

  INT_VEC_PARAMS(ISUPPZ);
  INT_VEC_PARAMS(IWORK);

  REAL GET_DOUBLE(VL),
       GET_DOUBLE(VU),
       GET_DOUBLE(ABSTOL);

  value v_res;

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(syevr)(
    &JOBZ, &RANGE, &UPLO,
    &N,
    A_data, &rows_A,
    &VL, &VU,
    &IL, &IU,
    &ABSTOL,
    &M,
    W_data,
    Z_data, &rows_Z,
    ISUPPZ_data,
    WORK_data, &LWORK,
    IWORK_data, &LIWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = Val_long(M);

  CAMLreturn(v_res);
}

CAMLprim value LFUN(syevr_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syevr_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11], argv[12], argv[13], argv[14],
      argv[15], argv[16], argv[17], argv[18], argv[19], argv[20], argv[21]);
}

/** TODO: SYEVX */

/** TODO: SYGVX */

/** TODO: SPEVX */

/** TODO: SPGVX */

/** TODO: SBEVX */

/** TODO: SBGVX */

/** TODO: STEVX */

/** TODO: STEVR */


/* Generalized eigenvalue and singular value problems (simple drivers)
************************************************************************/

/** SYGV */

extern void FUN(sygv)(
  integer *ITYPE, char *JOBZ, char *UPLO,
  integer *N,
  REAL *A, integer *LDA,
  REAL *B, integer *LDB,
  REAL *W,
  REAL *WORK, integer *LWORK,
  integer *INFO);

CAMLprim value LFUN(sygv_stub)(
  value vAR,
  value vAC,
  value vA,
  value vBR,
  value vBC,
  value vB,
  value vN,
  value vITYPE,
  value vJOBZ, value vUPLO,
  value vOFSW, value vW,
  value vWORK, value vLWORK)
{
  CAMLparam4(vA, vB, vW, vWORK);

  char GET_INT(JOBZ),
       GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(ITYPE),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  VEC_PARAMS(W);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sygv)(
    &ITYPE, &JOBZ, &UPLO,
    &N,
    A_data, &rows_A,
    B_data, &rows_B,
    W_data,
    WORK_data, &LWORK,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_int(INFO));
}

CAMLprim value LFUN(sygv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(sygv_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11], argv[12], argv[13]);
}


/** TODO: SYGVD */

/** TODO: SPGV */

/** TODO: SPGVD */

/** TODO: SPGV */

/** TODO: SPGVD */

/** SBGV */

extern void FUN(sbgv)(
  char *JOBZ, char *UPLO,
  integer *N,
  integer *KA, integer *KB,
  REAL *AB, integer *LDAB,
  REAL *BB, integer *LDBB,
  REAL *W,
  REAL *Z, integer *LDZ,
  REAL *WORK,
  integer *INFO);

CAMLexport value LFUN(sbgv_stub)(
  value vAR,
  value vAC,
  value vA,
  value vBR,
  value vBC,
  value vB,
  value vN, value vKA, value vKB,
  value vJOBZ, value vUPLO,
  value vOFSW, value vW,
  value vZR,
  value vZC,
  value vZ,
  value vWORK)
{
  CAMLparam5(vA, vB, vW, vZ, vWORK);

  char GET_INT(JOBZ),
       GET_INT(UPLO);

  integer GET_INT(N),
          GET_INT(KA),
          GET_INT(KB),
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  VEC_PARAMS(W);
  MAT_PARAMS(Z);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sbgv)(
    &JOBZ, &UPLO,
    &N,
    &KA, &KB,
    A_data, &rows_A,
    B_data, &rows_B,
    W_data,
    Z_data, &rows_Z,
    WORK_data,
    &INFO);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_int(INFO));
}

CAMLexport value LFUN(sbgv_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(sbgv_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
      argv[8], argv[9], argv[10], argv[11], argv[12], argv[13], argv[14],
      argv[15], argv[16]);
}

/** TODO: SBGVD */
