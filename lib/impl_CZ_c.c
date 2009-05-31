/* File: impl_CZ_c.c

   Copyright (C) 2001-

     Egbert Ammicht
     email: eammicht@lucent.com

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umh.ac.be/an/

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
#include "f2c.h"

/*** BLAS-1 */

/** DOTU */

extern COMPLEX FUN(dotu)(
  integer *N,
  COMPLEX *X, integer *INCX,
  COMPLEX *Y, integer *INCY);

CAMLprim value LFUN(dotu_stub)(
  value vN,
  value vOFSY, value vINCY, value vY,
  value vOFSX, value vINCX, value vX)
{
  CAMLparam2(vX, vY);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  COMPLEX res;

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  res =
    FUN(dotu)(
      &N,
      X_data, &INCX,
      Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(copy_two_doubles(res.r, res.i));
}

CAMLprim value LFUN(dotu_stub_bc)(value *argv, int argn)
{
  return LFUN(dotu_stub)(argv[0], argv[1], argv[2], argv[3],
                         argv[4], argv[5], argv[6]);
}


/** DOTC */

extern COMPLEX FUN(dotc)(
  integer *N,
  COMPLEX *X, integer *INCX,
  COMPLEX *Y, integer *INCY);

CAMLprim value LFUN(dotc_stub)(
  value vN,
  value vOFSY, value vINCY, value vY,
  value vOFSX, value vINCX, value vX)
{
  CAMLparam2(vX, vY);

  integer GET_INT(N),
          GET_INT(INCX),
          GET_INT(INCY);

  COMPLEX res;

  VEC_PARAMS(X);
  VEC_PARAMS(Y);

  caml_enter_blocking_section();  /* Allow other threads */
  res =
    FUN(dotc)(
      &N,
      X_data, &INCX,
      Y_data, &INCY);
  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(copy_two_doubles(res.r, res.i));
}

CAMLprim value LFUN(dotc_stub_bc)(value *argv, int argn)
{
  return LFUN(dotc_stub)(argv[0], argv[1], argv[2], argv[3],
                         argv[4], argv[5], argv[6]);
}


/*** BLAS-2 */

/** TODO: HEMV */

/** TODO: HBMV */

/** TODO: HPMV */


/*** LAPACK */

/* Auxiliary Routines
************************************************************************/

/** LANSY */

extern REAL FUN(lansy)(
  char *NORM, char *UPLO,
  integer *N,
  NUMBER *A, integer *LDA,
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

CAMLprim value LFUN(lansy_stub_bc)(value *argv, int argn)
{
  return
    LFUN(lansy_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}


/* Computational functions
************************************************************************/

/** GECON */

extern void FUN(gecon)(
  char *NORM,
  integer *N,
  COMPLEX *A, integer *LDA,
  REAL *ANORM, REAL *RCOND,
  COMPLEX *WORK, REAL *RWORK,
  integer *INFO);

CAMLprim value LFUN(gecon_stub)(
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vWORK,
  value vRWORK,
  value vNORM,
  value vANORM)
{
  CAMLparam3(vA, vWORK, vRWORK);
  CAMLlocal1(v_rcond);

  char GET_INT(NORM);
  integer GET_INT(N), INFO;
  REAL GET_DOUBLE(ANORM), RCOND;

  value v_res;

  MAT_PARAMS(A);
  VEC_PARAMS1(WORK);
  RVEC_PARAMS1(RWORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(gecon)(
    &NORM, &N,
    A_data, &rows_A,
    &ANORM, &RCOND,
    WORK_data, RWORK_data, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  v_rcond = caml_copy_double(RCOND);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = v_rcond;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(gecon_stub_bc)(value *argv, int argn)
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
  COMPLEX *A, integer *LDA,
  integer *IPIV,
  REAL *ANORM, REAL *RCOND,
  COMPLEX *WORK,
  integer *INFO);

CAMLprim value LFUN(sycon_stub)(
  value vUPLO,
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vIPIV,
  value vWORK,
  value vANORM)
{
  CAMLparam3(vA, vIPIV, vWORK);
  CAMLlocal1(v_rcond);

  char GET_INT(UPLO);
  integer GET_INT(N), INFO;
  REAL GET_DOUBLE(ANORM), RCOND;

  value v_res;

  MAT_PARAMS(A);
  INT_VEC_PARAMS(IPIV);
  VEC_PARAMS1(WORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(sycon)(
    &UPLO, &N,
    A_data, &rows_A,
    IPIV_data,
    &ANORM, &RCOND,
    WORK_data, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  v_rcond = caml_copy_double(RCOND);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = v_rcond;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(sycon_stub_bc)(value *argv, int argn)
{
  return
    LFUN(sycon_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7]);
}

/** POCON */

extern void FUN(pocon)(
  char *UPLO,
  integer *N,
  COMPLEX *A, integer *LDA,
  REAL *ANORM, REAL *RCOND,
  COMPLEX *WORK, REAL *RWORK,
  integer *INFO);

CAMLprim value LFUN(pocon_stub)(
  value vUPLO,
  value vN,
  value vAR,
  value vAC,
  value vA,
  value vWORK,
  value vRWORK,
  value vANORM)
{
  CAMLparam3(vA, vWORK, vRWORK);
  CAMLlocal1(v_rcond);

  char GET_INT(UPLO);
  integer GET_INT(N), INFO;
  REAL GET_DOUBLE(ANORM), RCOND;

  value v_res;

  MAT_PARAMS(A);
  VEC_PARAMS1(WORK);
  RVEC_PARAMS1(RWORK);

  caml_enter_blocking_section();  /* Allow other threads */
  FUN(pocon)(
    &UPLO, &N,
    A_data, &rows_A,
    &ANORM, &RCOND,
    WORK_data, RWORK_data, &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  v_rcond = caml_copy_double(RCOND);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(INFO);
  Field(v_res, 1) = v_rcond;

  CAMLreturn(v_res);
}

CAMLprim value LFUN(pocon_stub_bc)(value *argv, int argn)
{
  return
    LFUN(pocon_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7]);
}


/* General SVD routines
************************************************************************/

/** GESVD */

extern void FUN(gesvd)(
  char* JOBU, char* JOBVT,
  integer *M, integer* N,
  COMPLEX *A, integer *LDA,
  REAL *S,
  COMPLEX *U, integer* LDU,
  COMPLEX *VT, integer *LDVT,
  COMPLEX *WORK, integer *LWORK,
  REAL *RWORK,
  integer *INFO);

CAMLprim value LFUN(gesvd_stub)(
  value vJOBU, value vJOBVT,
  value vM, value vN,
  value vAR, value vAC, value vA,
  value vS,
  value vUR, value vUC, value vU,
  value vVTR, value vVTC, value vVT,
  value vWORK, value vLWORK,
  value vRWORK)
{
  CAMLparam5(vA, vS, vU, vVT, vWORK);
  CAMLxparam1(vRWORK);

  char GET_INT(JOBU),
       GET_INT(JOBVT);

  integer GET_INT(M), GET_INT(N),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  RVEC_PARAMS1(S);
  MAT_PARAMS(U);
  MAT_PARAMS(VT);
  VEC_PARAMS1(WORK);
  RVEC_PARAMS1(RWORK);

  caml_enter_blocking_section(); /* Allow other threads */
  FUN(gesvd)(
    &JOBU, &JOBVT,
    &M, &N,
    A_data, &rows_A,
    S_data,
    U_data, &rows_U,
    VT_data, &rows_VT,
    WORK_data, &LWORK,
    RWORK_data,
    &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(gesvd_stub_bc)(value *argv, int argn)
{
  return
    LFUN(gesvd_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
      argv[12], argv[13], argv[14], argv[15], argv[16]);
}

/** TODO: GESDD */


/* General eigenvalue problem (simple drivers)
************************************************************************/

/** GEEV */

extern void FUN(geev)(
  char *JOBVL, char *JOBVR,
  integer *N,
  COMPLEX *A, integer *LDA,
  COMPLEX *W,
  COMPLEX *VL, integer *LDVL,
  COMPLEX *VR, integer *LDVR,
  COMPLEX *WORK, integer *LWORK,
  REAL *RWORK,
  integer *INFO);

CAMLprim value LFUN(geev_stub)(
  value vAR, value vAC, value vA,
  value vN,
  value vOFSW, value vW,
  value vVLR, value vVLC, value vVL, value vJOBVL,
  value vVRR, value vVRC, value vVR, value vJOBVR,
  value vWORK, value vLWORK, value vRWORK)
{
  CAMLparam5(vA, vW, vVL, vVR, vWORK);
  CAMLxparam1(vRWORK);

  char GET_INT(JOBVL),
       GET_INT(JOBVR);

  integer GET_INT(N),
          GET_INT(LWORK),
          INFO;

  MAT_PARAMS(A);
  VEC_PARAMS(W);
  MAT_PARAMS(VL);
  MAT_PARAMS(VR);
  VEC_PARAMS1(WORK);
  RVEC_PARAMS1(RWORK);

  /* weird GEEV requirement:
   * even when the arrays aren't
   * referenced, LD's have to be >= 1 */
  if(JOBVL == 'N') rows_VL = 1;
  if(JOBVR == 'N') rows_VR = 1;

  caml_enter_blocking_section(); /* Allow other threads */
  FUN(geev)(
    &JOBVL, &JOBVR,
    &N,
    A_data, &rows_A,
    W_data,
    VL_data, &rows_VL,
    VR_data, &rows_VR,
    WORK_data, &LWORK,
    RWORK_data,
    &INFO);
  caml_leave_blocking_section(); /* Disallow other threads */

  CAMLreturn(Val_long(INFO));
}

CAMLprim value LFUN(geev_stub_bc)(value *argv, int argn)
{
  return
    LFUN(geev_stub)(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11],
      argv[12], argv[13], argv[14], argv[15], argv[16]);
}
