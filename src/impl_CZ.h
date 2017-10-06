/* File: impl_CZ.h

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

     Florent Hoareau
     email: h.florent@gmail.com
     WWW: none

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

#define LACAML_COMPLEX
#include "lacaml_macros.h"

/*** BLAS-1 */

/** DOTU */

extern COMPLEX FUN(dotu)(
  integer *N,
  COMPLEX *X, integer *INCX,
  COMPLEX *Y, integer *INCY);

CAMLprim value LFUN(dotu_stub)(
  intnat vN,
  intnat vOFSX, intnat vINCX, value vX,
  intnat vOFSY, intnat vINCY, value vY)
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

CAMLprim value LFUN(dotu_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(dotu_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6]);
}


/** DOTC */

extern COMPLEX FUN(dotc)(
  integer *N,
  COMPLEX *X, integer *INCX,
  COMPLEX *Y, integer *INCY);

CAMLprim value LFUN(dotc_stub)(
  intnat vN,
  intnat vOFSX, intnat vINCX, value vX,
  intnat vOFSY, intnat vINCY, value vY)
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

CAMLprim value LFUN(dotc_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(dotc_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6]);
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

CAMLprim double LFUN(lansy_stub)(
  value vNORM,
  value vUPLO,
  intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vWORK)
{
  CAMLparam2(vA, vWORK);

  char GET_CHAR(NORM), GET_CHAR(UPLO);
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

  CAMLreturnT(double, res);
}

CAMLprim value LFUN(lansy_stub_bc)(value *argv, int __unused argn)
{
  return
    caml_copy_double(
        LFUN(lansy_stub)(
          argv[0],
          argv[1],
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          argv[6]));
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
  intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vWORK,
  value vRWORK,
  value vNORM,
  double vANORM)
{
  CAMLparam3(vA, vWORK, vRWORK);
  CAMLlocal1(v_rcond);

  char GET_CHAR(NORM);
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

CAMLprim value LFUN(gecon_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gecon_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        argv[3],
        argv[4],
        argv[5],
        argv[6],
        Double_val(argv[7]));
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
  intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vIPIV,
  value vWORK,
  double vANORM)
{
  CAMLparam3(vA, vIPIV, vWORK);
  CAMLlocal1(v_rcond);

  char GET_CHAR(UPLO);
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

CAMLprim value LFUN(sycon_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(sycon_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4],
        argv[5],
        argv[6],
        Double_val(argv[7]));
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
  intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vWORK,
  value vRWORK,
  double vANORM)
{
  CAMLparam3(vA, vWORK, vRWORK);
  CAMLlocal1(v_rcond);

  char GET_CHAR(UPLO);
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

CAMLprim value LFUN(pocon_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(pocon_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4],
        argv[5],
        argv[6],
        Double_val(argv[7]));
}

/* General Schur factorization
************************************************************************/

/** GEES */

/* Predefined callbacks for eigenvalue selection */

static inline integer select_left_plane(const COMPLEX *z_ptr)
{
  return (z_ptr->r < 0) ? 1 : 0;
}

static inline integer select_right_plane(const COMPLEX *z_ptr)
{
  return (z_ptr->r > 0) ? 1 : 0;
}

static inline integer select_disk_interior(const COMPLEX *z_ptr)
{
  REAL re = z_ptr->r;
  REAL im = z_ptr->i;
  return (re * re + im * im < 1) ? 1 : 0;
}

static inline integer select_disk_exterior(const COMPLEX *z_ptr)
{
  REAL re = z_ptr->r;
  REAL im = z_ptr->i;
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

static integer select_ocaml_exec_callback(const COMPLEX *z_ptr)
{
  value v_res, v_arg;

  if (!select_ocaml_locked_runtime) {
    caml_leave_blocking_section(); /* Disallow other threads */
    select_ocaml_locked_runtime = true;
  }

  v_arg = caml_alloc_small(2, Double_array_tag);
  Store_double_field(v_arg, 0, (double) z_ptr->r);
  Store_double_field(v_arg, 1, (double) z_ptr->i);

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

typedef integer (*LAPACK_CZ_SELECT1) ( const COMPLEX* );

extern void FUN(gees)(
  char *JOBVS, char *SORT,
  LAPACK_CZ_SELECT1 SELECT,
  integer *N,
  COMPLEX *A, integer *LDA,
  integer *SDIM,
  COMPLEX *W,
  COMPLEX *VS, integer *LDVS,
  COMPLEX *WORK,
  integer *LWORK,
  REAL *RWORK,
  integer *BWORK,
  integer *INFO);

CAMLprim value LFUN(gees_stub)(
  value vJOBVS, value vSORT,
  intnat vSELECT, value vSELECT_FUN,
  intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vW,
  intnat vVSR, intnat vVSC, value vVS,
  value vWORK, intnat vLWORK,
  value vRWORK,
  value vBWORK)
{
  CAMLparam5(vA, vVS, vW, vWORK, vRWORK);
  CAMLxparam2(vBWORK, vSELECT_FUN);
  CAMLlocal1(v_res);

  char GET_CHAR(JOBVS),
       GET_CHAR(SORT);

  integer GET_INT(SELECT),
          GET_INT(N),
          GET_INT(LWORK),
          SDIM,
          INFO;

  MAT_PARAMS(A);
  MAT_PARAMS(VS);
  VEC_PARAMS1(W);
  VEC_PARAMS1(WORK);
  RVEC_PARAMS1(RWORK);
  INT_VEC_PARAMS(BWORK);

  LAPACK_CZ_SELECT1 select_function = NULL;
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
    W_data,
    VS_data, &rows_VS,
    WORK_data,
    &LWORK, RWORK_data, BWORK_data,
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
        argv[0],
        argv[1],
        Int_val(argv[2]),
        argv[3],
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7],
        argv[8],
        Int_val(argv[9]),
        Int_val(argv[10]),
        argv[11],
        argv[12],
        Int_val(argv[13]),
        argv[14],
        argv[15]);
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

CAMLprim intnat LFUN(gesvd_stub)(
  value vJOBU, value vJOBVT,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  value vS,
  intnat vUR, intnat vUC, value vU,
  intnat vVTR, intnat vVTC, value vVT,
  value vWORK, intnat vLWORK,
  value vRWORK)
{
  CAMLparam5(vA, vS, vU, vVT, vWORK);
  CAMLxparam1(vRWORK);

  char GET_CHAR(JOBU),
       GET_CHAR(JOBVT);

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

  CAMLreturn(INFO);
}

CAMLprim value LFUN(gesvd_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(gesvd_stub)(
          argv[0],
          argv[1],
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          argv[6],
          argv[7],
          Int_val(argv[8]),
          Int_val(argv[9]),
          argv[10],
          Int_val(argv[11]),
          Int_val(argv[12]),
          argv[13],
          argv[14],
          Int_val(argv[15]),
          argv[16]));
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

CAMLprim intnat LFUN(geev_stub)(
  intnat vAR, intnat vAC, value vA,
  intnat vN,
  intnat vOFSW, value vW,
  intnat vVLR, intnat vVLC, value vVL, value vJOBVL,
  intnat vVRR, intnat vVRC, value vVR, value vJOBVR,
  value vWORK, intnat vLWORK, value vRWORK)
{
  CAMLparam5(vA, vW, vVL, vVR, vWORK);
  CAMLxparam1(vRWORK);

  char GET_CHAR(JOBVL),
       GET_CHAR(JOBVR);

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

  CAMLreturn(INFO);
}

CAMLprim value LFUN(geev_stub_bc)(value *argv, int __unused argn)
{
  return
    Val_int(
        LFUN(geev_stub)(
          Int_val(argv[0]),
          Int_val(argv[1]),
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8],
          argv[9],
          Int_val(argv[10]),
          Int_val(argv[11]),
          argv[12],
          argv[13],
          argv[14],
          Int_val(argv[15]),
          argv[16]));
}
