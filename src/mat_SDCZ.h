/* File: mat_SDCZ.h

   Copyright (C) 2007-

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

#include <string.h>
#include <assert.h>
#include "lacaml_macros.h"

static integer integer_one = 1;
static NUMBER number_zero = NUMBER_ZERO;
static NUMBER number_one = NUMBER_ONE;
static NUMBER number_minus_one = NUMBER_MINUS_ONE;


/* sum_mat */

static inline NUMBER sum_range(integer N, NUMBER *data, NUMBER acc)
{
  for (int i = 0; i < N; i++) acc = ADD_NUMBER(acc, data[i]);
  return acc;
}

CAMLprim vNUMBER LFUN(sum_mat_stub)(
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA)
{
  CAMLparam1(vA);
  integer GET_INT(M), GET_INT(N);

  NUMBER res = NUMBER_ZERO;

  if (M > 0 && N > 0) {
    MAT_PARAMS(A);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                res = sum_range(PINIT, A_data, res);
                PINIT++;
                A_data += rows_A;
              }
            } else {
              while (PINIT < M) {
                res = sum_range(PINIT, A_data, res);
                PINIT++;
                A_data += rows_A;
              }
              if (M == rows_A) res = sum_range(A_stop - A_data, A_data, res);
              else
                while (A_data < A_stop) {
                  res = sum_range(M, A_data, res);
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
                res = sum_range(MP, A_data, res);
                A_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  res = sum_range(M, A_data, res);
                  A_data += rows_A;
                }
              }
              M--;
            }
            rows_A++;
            while (A_data < A_stop) {
              res = sum_range(M, A_data, res);
              M--;
              A_data += rows_A;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturnNUMBER(res);
}

CAMLprim value LFUN(sum_mat_stub_bc)(value *argv, int __unused argn)
{
  return
    COPY_NUMBER(
        LFUN(sum_mat_stub)(
          argv[0],
          Int_val(argv[1]),
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          argv[6]));
}


/* fill_mat */

static inline void fill_range(integer N, NUMBER *data, NUMBER v)
{
  for (int i = 0; i < N; i++) data[i] = v;
}

CAMLprim value LFUN(fill_mat_stub)(
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  vNUMBER vX)
{
  CAMLparam1(vA);
  integer GET_INT(M), GET_INT(N);

  if (M > 0 && N > 0) {
    MAT_PARAMS(A);
    NUMBER X;
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    INIT_NUMBER(X);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                fill_range(PINIT, A_data, X);
                PINIT++;
                A_data += rows_A;
              }
            } else {
              while (PINIT < M) {
                fill_range(PINIT, A_data, X);
                PINIT++;
                A_data += rows_A;
              }
              if (M == rows_A) fill_range(A_stop - A_data, A_data, X);
              else
                while (A_data < A_stop) {
                  fill_range(M, A_data, X);
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
                fill_range(MP, A_data, X);
                A_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  fill_range(M, A_data, X);
                  A_data += rows_A;
                }
              }
              M--;
            }
            rows_A++;
            while (A_data < A_stop) {
              fill_range(M, A_data, X);
              M--;
              A_data += rows_A;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(fill_mat_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(fill_mat_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        NUMBER_val(argv[7]));
}


/* add_const_mat */

static inline void add_const_range(
    integer N, NUMBER *A_data, NUMBER *B_data, NUMBER v)
{
  for (int i = 0; i < N; i++) B_data[i] = ADD_NUMBER(A_data[i], v);
}

CAMLprim value LFUN(add_const_mat_stub)(
    vNUMBER vC,
    value vPKIND, intnat vPINIT,
    intnat vM, intnat vN,
    intnat vAR, intnat vAC, value vA,
    intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(M), GET_INT(N);

  if (M > 0 && N > 0) {
    NUMBER C;
    MAT_PARAMS(A);
    MAT_PARAMS(B);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    INIT_NUMBER(C);

    caml_enter_blocking_section();  /* Allow other threads */
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                add_const_range(PINIT, A_data, B_data, C);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
              }
            } else {
              while (PINIT < M) {
                add_const_range(PINIT, A_data, B_data, C);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
              }
              if (M == rows_A && M == rows_B)
                add_const_range(A_stop - A_data, A_data, B_data, C);
              else
                while (A_data < A_stop) {
                  add_const_range(M, A_data, B_data, C);
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
                add_const_range(MP, A_data, B_data, C);
                A_data += MP;
                B_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  add_const_range(M, A_data, B_data, C);
                  A_data += rows_A;
                  B_data += rows_B;
                }
              }
              M--;
            }
            rows_A++; rows_B++;
            while (A_data < A_stop) {
              add_const_range(M, A_data, B_data, C);
              M--;
              A_data += rows_A;
              B_data += rows_B;
            }
            break;
          }
      }
    caml_leave_blocking_section();  /* Disallow other threads */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(add_const_mat_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(add_const_mat_stub)(
        NUMBER_val(argv[0]),
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7],
        Int_val(argv[8]),
        Int_val(argv[9]),
        argv[10]);
}


/* swap */

extern void FUN(swap)(
  integer *N,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(swap_mat_stub)(
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);
  integer GET_INT(M), GET_INT(N);

  if (M > 0 && N > 0) {
    MAT_PARAMS(A);
    MAT_PARAMS(B);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                FUN(swap)(&PINIT, A_data, &integer_one, B_data, &integer_one);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
              }
            } else {
              while (PINIT < M) {
                FUN(swap)(&PINIT, A_data, &integer_one, B_data, &integer_one);
                PINIT++;
                A_data += rows_A;
                B_data += rows_B;
              }
              if (M == rows_A && M == rows_B) {
                integer MN = A_stop - A_data;
                FUN(swap)(&MN, A_data, &integer_one, B_data, &integer_one);
              } else
                while (A_data < A_stop) {
                  FUN(swap)(&M, A_data, &integer_one, B_data, &integer_one);
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
                FUN(swap)(&MP, A_data, &integer_one, B_data, &integer_one);
                A_data += MP;
                B_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  FUN(swap)(&M, A_data, &integer_one, B_data, &integer_one);
                  A_data += rows_A;
                  B_data += rows_B;
                }
              }
              M--;
            }
            rows_A++; rows_B++;
            while (A_data < A_stop) {
              FUN(swap)(&M, A_data, &integer_one, B_data, &integer_one);
              M--;
              A_data += rows_A;
              B_data += rows_B;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(swap_mat_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(swap_mat_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        Int_val(argv[7]),
        Int_val(argv[8]),
        argv[9]);
}


/* transpose */

extern void FUN(copy)(
  integer *N,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(transpose_copy_stub)(
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);
  integer GET_INT(M), GET_INT(N);

  if (M > 0 && N > 0) {
    MAT_PARAMS(A);
    MAT_PARAMS(B);
    NUMBER *A_stop = A_data + rows_A * N;
    caml_enter_blocking_section();
      do {
        FUN(copy)(&M, A_data, &integer_one, B_data, &rows_B);
        A_data += rows_A;
        B_data++;
      } while (A_data != A_stop);
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(transpose_copy_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(transpose_copy_stub)(
        Int_val(argv[0]),
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        argv[4],
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7]);
}


/* scal */

extern void FUN(scal)(
  integer *N,
  NUMBER *ALPHA,
  NUMBER *X, integer *INCX);

CAMLprim value LFUN(scal_mat_stub)(
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  vNUMBER vALPHA,
  intnat vAR, intnat vAC, value vA)
{
  CAMLparam1(vA);
  integer GET_INT(M), GET_INT(N);

  if ( M > 0 && N > 0) {
    NUMBER ALPHA;
    MAT_PARAMS(A);
    INIT_NUMBER(ALPHA);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                FUN(scal)(&PINIT, &ALPHA, A_data, &integer_one);
                PINIT++;
                A_data += rows_A;
              }
            } else {
              while (PINIT < M) {
                FUN(scal)(&PINIT, &ALPHA, A_data, &integer_one);
                PINIT++;
                A_data += rows_A;
              }
              if (M == rows_A) {
                integer MN = A_stop - A_data;
                FUN(scal)(&MN, &ALPHA, A_data, &integer_one);
              } else
                while (A_data < A_stop) {
                  FUN(scal)(&M, &ALPHA, A_data, &integer_one);
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
                FUN(scal)(&MP, &ALPHA, A_data, &integer_one);
                A_data += MP;
              } else {
                NUMBER *A_block_stop = A_data + PINIT*rows_A;
                while (A_data < A_block_stop) {
                  FUN(scal)(&M, &ALPHA, A_data, &integer_one);
                  A_data += rows_A;
                }
              }
              M--;
            }
            rows_A++;
            while (A_data < A_stop) {
              FUN(scal)(&M, &ALPHA, A_data, &integer_one);
              M--;
              A_data += rows_A;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(scal_mat_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(scal_mat_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        NUMBER_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7]);
}


/* scal_cols */

CAMLprim value LFUN(scal_cols_stub)(
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  intnat vAR, intnat vAC, value vA,
  intnat vOFSALPHAs, value vALPHAs)
{
  CAMLparam2(vALPHAs, vA);
  integer GET_INT(M), GET_INT(N);

  if (M > 0 && N > 0) {
    VEC_PARAMS(ALPHAs);
    MAT_PARAMS(A);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + rows_A * N;
            if (PINIT + N - 1 <= M) {
              while (A_data < A_stop) {
                FUN(scal)(&PINIT, ALPHAs_data, A_data, &integer_one);
                PINIT++;
                A_data += rows_A;
                ALPHAs_data++;
              }
            } else {
              while (PINIT < M) {
                FUN(scal)(&PINIT, ALPHAs_data, A_data, &integer_one);
                PINIT++;
                A_data += rows_A;
                ALPHAs_data++;
              }
              if (M == rows_A) {
                integer MN = A_stop - A_data;
                FUN(scal)(&MN, ALPHAs_data, A_data, &integer_one);
              } else
                while (A_data < A_stop) {
                  FUN(scal)(&M, ALPHAs_data, A_data, &integer_one);
                  A_data += rows_A;
                  ALPHAs_data++;
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
              NUMBER *A_block_stop = A_data + PINIT*rows_A;
              while (A_data < A_block_stop) {
                FUN(scal)(&M, ALPHAs_data, A_data, &integer_one);
                A_data += rows_A;
                ALPHAs_data++;
              }
              M--;
            }
            rows_A++;
            while (A_data < A_stop) {
              FUN(scal)(&M, ALPHAs_data, A_data, &integer_one);
              M--;
              A_data += rows_A;
              ALPHAs_data++;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(scal_cols_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(scal_cols_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        argv[6],
        Int_val(argv[7]),
        argv[8]);
}


/* scal_rows */

CAMLprim value LFUN(scal_rows_stub)(
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  intnat vOFSALPHAs, value vALPHAs,
  intnat vAR, intnat vAC, value vA)
{
  CAMLparam2(vALPHAs, vA);
  integer GET_INT(M), GET_INT(N);

  if (M > 0 && N > 0) {
    VEC_PARAMS(ALPHAs);
    MAT_PARAMS(A);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *A_stop = A_data + M + (M - PINIT + 1) * rows_A;
            while (--PINIT) {
              FUN(scal)(&N, ALPHAs_data, A_data, &rows_A);
              A_data++;
              ALPHAs_data++;
            }
            while (A_data != A_stop && N > 0) {
              FUN(scal)(&N, ALPHAs_data, A_data, &rows_A);
              A_data += rows_A + 1;
              ALPHAs_data++;
              N--;
            }
            break;
          }
        case LOWER :
          {
            NUMBER *A_stop = A_data + M;
            while (A_data < A_stop && PINIT < N) {
              FUN(scal)(&PINIT, ALPHAs_data, A_data, &rows_A);
              A_data++;
              ALPHAs_data++;
              PINIT++;
            }
            while (A_data < A_stop) {
              FUN(scal)(&PINIT, ALPHAs_data, A_data, &rows_A);
              A_data++;
              ALPHAs_data++;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(scal_rows_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(scal_rows_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        argv[5],
        Int_val(argv[6]),
        Int_val(argv[7]),
        argv[8]);
}


/* axpy_mat */

extern void FUN(axpy)(
  integer *N,
  NUMBER *ALPHA,
  NUMBER *X, integer *INCX,
  NUMBER *Y, integer *INCY);

CAMLprim value LFUN(axpy_mat_stub)(
  vNUMBER vALPHA,
  value vPKIND, intnat vPINIT,
  intnat vM, intnat vN,
  intnat vXR, intnat vXC, value vX,
  intnat vYR, intnat vYC, value vY)
{
  CAMLparam2(vX, vY);
  integer GET_INT(M), GET_INT(N);
  if (M > 0 && N > 0) {
    NUMBER ALPHA;
    MAT_PARAMS(X);
    MAT_PARAMS(Y);
    pentagon_kind PKIND = get_pentagon_kind(vPKIND);
    integer GET_INT(PINIT);
    INIT_NUMBER(ALPHA);
    caml_enter_blocking_section();
      switch (PKIND) {
        case UPPER :
          {
            NUMBER *X_stop = X_data + rows_X * N;
            if (PINIT + N - 1 <= M) {
              while (X_data < X_stop) {
                FUN(axpy)(&PINIT, &ALPHA,
                    X_data, &integer_one, Y_data, &integer_one);
                PINIT++;
                X_data += rows_X;
                Y_data += rows_Y;
              }
            } else {
              while (PINIT < M) {
                FUN(axpy)(&PINIT, &ALPHA,
                    X_data, &integer_one, Y_data, &integer_one);
                PINIT++;
                X_data += rows_X;
                Y_data += rows_Y;
              }
              if (M == rows_X && M == rows_Y) {
                integer MN = X_stop - X_data;
                FUN(axpy)(&MN, &ALPHA,
                    X_data, &integer_one, Y_data, &integer_one);
              } else
                while (X_data < X_stop) {
                  FUN(axpy)(&M, &ALPHA,
                      X_data, &integer_one, Y_data, &integer_one);
                  X_data += rows_X;
                  Y_data += rows_Y;
                }
            }
            break;
          }
        case LOWER :
          {
            NUMBER *X_stop;
            integer stop_col = M + PINIT;
            if (stop_col > N) stop_col = N;
            X_stop = X_data + stop_col*rows_X;
            if (PINIT > 1) {
              if (M == rows_X && M == rows_Y) {
                integer MP = M*PINIT;
                FUN(axpy)(&MP, &ALPHA,
                    X_data, &integer_one, Y_data, &integer_one);
                X_data += MP;
                Y_data += MP;
              } else {
                NUMBER *X_block_stop = X_data + PINIT*rows_X;
                while (X_data < X_block_stop) {
                  FUN(axpy)(&M, &ALPHA,
                      X_data, &integer_one, Y_data, &integer_one);
                  X_data += rows_X;
                  Y_data += rows_Y;
                }
              }
              M--;
            }
            rows_X++; rows_Y++;
            while (X_data < X_stop) {
              FUN(axpy)(&M, &ALPHA,
                  X_data, &integer_one, Y_data, &integer_one);
              M--;
              X_data += rows_X;
              Y_data += rows_Y;
            }
            break;
          }
      }
    caml_leave_blocking_section();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(axpy_mat_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(axpy_mat_stub)(
        NUMBER_val(argv[0]),
        argv[1],
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        Int_val(argv[5]),
        Int_val(argv[6]),
        argv[7],
        Int_val(argv[8]),
        Int_val(argv[9]),
        argv[10]);
}


/* gemm_diag */

#define COMMON_DIAG_LOOP(MFUN) \
  if (NUMBER_EQUAL(ALPHA, number_zero)) \
    FUN(scal)(&N, &BETA, Y_data, &integer_one); \
  else { \
    if (NUMBER_EQUAL(ALPHA, number_one)) { \
      if (NUMBER_EQUAL(BETA, number_zero)) \
        MFUN##_DIAG_LOOP(*Y_data = d) \
      else if (NUMBER_EQUAL(BETA, number_one)) \
        MFUN##_DIAG_LOOP_WITH_Y(*Y_data = ADD_NUMBER(d, y)) \
      else if (NUMBER_EQUAL(BETA, number_minus_one)) \
        MFUN##_DIAG_LOOP_WITH_Y(*Y_data = SUB_NUMBER(d, y)) \
      else \
        MFUN##_DIAG_LOOP_WITH_Y( \
          NUMBER tmp = MUL_NUMBER(y, BETA); \
          *Y_data = ADD_NUMBER(d, tmp)) \
    } else if (NUMBER_EQUAL(ALPHA, number_minus_one)) { \
      if (NUMBER_EQUAL(BETA, number_zero)) \
        MFUN##_DIAG_LOOP(*Y_data = NEG_NUMBER(d)) \
      else if (NUMBER_EQUAL(BETA, number_one)) \
        MFUN##_DIAG_LOOP_WITH_Y(*Y_data = SUB_NUMBER(y, d)) \
      else if (NUMBER_EQUAL(BETA, number_minus_one)) \
        MFUN##_DIAG_LOOP_WITH_Y( \
          NUMBER tmp = ADD_NUMBER(d, y); \
          *Y_data = NEG_NUMBER(tmp)) \
      else \
        MFUN##_DIAG_LOOP_WITH_Y( \
          NUMBER tmp = MUL_NUMBER(y, BETA); \
          *Y_data = SUB_NUMBER(tmp, d)) \
    } else { \
      if (NUMBER_EQUAL(BETA, number_zero)) \
        MFUN##_DIAG_LOOP(*Y_data = MUL_NUMBER(ALPHA, d)) \
      else if (NUMBER_EQUAL(BETA, number_one)) \
        MFUN##_DIAG_LOOP_WITH_Y( \
          NUMBER tmp = MUL_NUMBER(ALPHA, d); \
          *Y_data = ADD_NUMBER(tmp, y)) \
      else if (NUMBER_EQUAL(BETA, number_minus_one)) \
        MFUN##_DIAG_LOOP_WITH_Y( \
          NUMBER tmp = MUL_NUMBER(ALPHA, d); \
          *Y_data = SUB_NUMBER(tmp, y)) \
      else \
        MFUN##_DIAG_LOOP_WITH_Y( \
          NUMBER ad = MUL_NUMBER(ALPHA, d); \
          NUMBER yb = MUL_NUMBER(BETA, y); \
          *Y_data = ADD_NUMBER(ad, yb)) \
    } \
  }

extern NUMBER
DOTU(integer *N, NUMBER *X, integer *INCX, NUMBER *Y, integer *INCY);

#define GEMM_INCR \
  A_data += iter_incr_A; \
  B_data += iter_incr_B; \
  ++Y_data

#define COMMON_GEMM_DIAG_LOOP(DOIT) \
  while (Y_data != stop_Y) { \
    NUMBER d = DOTU(&K, A_data, &dot_incr_A, B_data, &dot_incr_B); \
    DOIT; \
    GEMM_INCR; \
  }

#ifndef LACAML_COMPLEX          /* Real number */
#define GEMM_DIAG_LOOP(DOIT) COMMON_GEMM_DIAG_LOOP(DOIT)
#else                           /* Complex number */

extern NUMBER
DOTC(integer *N, NUMBER *X, integer *INCX, NUMBER *Y, integer *INCY);

#define GEMM_DIAG_LOOP(DOIT) \
  if (TRANSA == 'C') \
    if (TRANSB == 'C') \
      while (Y_data != stop_Y) { \
        NUMBER cd = DOTU(&K, A_data, &dot_incr_A, B_data, &dot_incr_B); \
        NUMBER d = COMLEX_CONJ(cd); \
        DOIT; \
        GEMM_INCR; \
      } \
    else \
      while (Y_data != stop_Y) { \
        NUMBER d = DOTC(&K, A_data, &dot_incr_A, B_data, &dot_incr_B); \
        DOIT; \
        GEMM_INCR; \
      } \
  else if (TRANSB == 'C') \
    while (Y_data != stop_Y) { \
      NUMBER d = DOTC(&K, B_data, &dot_incr_B, A_data, &dot_incr_A); \
      DOIT; \
      GEMM_INCR; \
    } \
  else COMMON_GEMM_DIAG_LOOP(DOIT)
#endif

#define GEMM_DIAG_LOOP_WITH_Y(DOIT) GEMM_DIAG_LOOP(NUMBER y = *Y_data; DOIT)

CAMLprim value LFUN(gemm_diag_stub)(
  value vTRANSA,
  value vTRANSB,
  intnat vN, intnat vK,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB,
  intnat vOFSY,
  value vY,
  vNUMBER vALPHA,
  vNUMBER vBETA
  )
{
  CAMLparam3(vA, vB, vY);

  integer GET_INT(N), GET_INT(K);
  char GET_CHAR(TRANSA), GET_CHAR(TRANSB);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  MAT_PARAMS(B);
  VEC_PARAMS(Y);

  unsigned long iter_incr_A, iter_incr_B;
  integer dot_incr_A, dot_incr_B;
  NUMBER *stop_Y = Y_data + N;

  if (TRANSB == 'N') {
    iter_incr_B = rows_B;
    dot_incr_B = 1;
  } else {
    iter_incr_B = 1;
    dot_incr_B = rows_B;
  }

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */

  if (TRANSA == 'N') {
    iter_incr_A = 1;
    dot_incr_A = rows_A;
  } else {
    iter_incr_A = rows_A;
    dot_incr_A = 1;
  }

  COMMON_DIAG_LOOP(GEMM)

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(gemm_diag_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(gemm_diag_stub)(
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
        argv[11],
        NUMBER_val(argv[12]),
        NUMBER_val(argv[13]));
}


/* syrk_diag */

#define SYRK_INCR \
  A_data += iter_incr_A; \
  ++Y_data

#define SYRK_DIAG_LOOP(DOIT) \
  while (Y_data != stop_Y) { \
    NUMBER d = DOTU(&K, A_data, &dot_incr_A, A_data, &dot_incr_A); \
    DOIT; \
    SYRK_INCR; \
  }

#define SYRK_DIAG_LOOP_WITH_Y(DOIT) SYRK_DIAG_LOOP(NUMBER y = *Y_data; DOIT)

CAMLprim value LFUN(syrk_diag_stub)(
  value vTRANS,
  intnat vN, intnat vK,
  intnat vAR, intnat vAC, value vA,
  intnat vOFSY,
  value vY,
  vNUMBER vALPHA,
  vNUMBER vBETA)
{
  CAMLparam2(vA, vY);

  integer GET_INT(N), GET_INT(K);
  char GET_CHAR(TRANS);

  NUMBER ALPHA;
  NUMBER BETA;

  MAT_PARAMS(A);
  VEC_PARAMS(Y);

  unsigned long iter_incr_A;
  integer dot_incr_A;
  NUMBER *stop_Y = Y_data + N;

  INIT_NUMBER(ALPHA);
  INIT_NUMBER(BETA);

  caml_enter_blocking_section();  /* Allow other threads */

  if (TRANS == 'N') {
    iter_incr_A = 1;
    dot_incr_A = rows_A;
  } else {
    iter_incr_A = rows_A;
    dot_incr_A = 1;
  }

  COMMON_DIAG_LOOP(SYRK)

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(syrk_diag_stub_bc)(value *argv, int __unused argn)
{
  return
    LFUN(syrk_diag_stub)(
        argv[0],
        Int_val(argv[1]),
        Int_val(argv[2]),
        Int_val(argv[3]),
        Int_val(argv[4]),
        argv[5],
        Int_val(argv[6]),
        argv[7],
        NUMBER_val(argv[8]),
        NUMBER_val(argv[9]));
}


/* gemm_trace */

#define GEMM_TRACE_INCR A_data += iter_incr_A; B_data += iter_incr_B

#define COMMON_GEMM_TRACE_LOOP \
  while (A_data != stop_A) { \
    NUMBER d = DOTU(&K, A_data, &dot_incr_A, B_data, &dot_incr_B); \
    res = ADD_NUMBER(res, d); \
    GEMM_TRACE_INCR; \
  }

CAMLprim vNUMBER LFUN(gemm_trace_stub)(
  value vTRANSA,
  value vTRANSB,
  intnat vN, intnat vK,
  intnat vAR, intnat vAC, value vA,
  intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(N), GET_INT(K);
  char GET_CHAR(TRANSA), GET_CHAR(TRANSB);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  unsigned long iter_incr_A, iter_incr_B;
  integer dot_incr_A, dot_incr_B;

  NUMBER res = NUMBER_ZERO;
  NUMBER *stop_A;

  caml_enter_blocking_section();  /* Allow other threads */

  if (TRANSA == 'N') {
    if (TRANSB == 'N') {
      if (N < K) {
        iter_incr_A = 1;
        dot_incr_A = rows_A;
        iter_incr_B = rows_B;
        dot_incr_B = 1;
      } else {
        NUMBER *tmp_A_data = A_data;
        integer tmp_N = N;
        A_data = B_data;
        B_data = tmp_A_data;
        N = K;
        K = tmp_N;
        iter_incr_A = 1;
        dot_incr_A = rows_B;
        iter_incr_B = rows_A;
        dot_incr_B = 1;
      }
    } else {
      if (N == rows_A && N == rows_B) {
        integer NK = N * K;
        res =
#ifdef LACAML_COMPLEX
          (TRANSB == 'C')
          ? DOTC(&NK, B_data, &integer_one, A_data, &integer_one)
          :
#endif
        DOTU(&NK, B_data, &integer_one, A_data, &integer_one);
        goto end;
      } else {
        integer tmp_N = N;
        NUMBER *tmp_A_data = A_data;
        A_data = B_data;
        B_data = tmp_A_data;
        N = K;
        K = tmp_N;
        iter_incr_A = rows_B;
        dot_incr_A = 1;
        iter_incr_B = rows_A;
        dot_incr_B = 1;
      }
    }
  } else {
    if (TRANSB == 'N') {
      if (K == rows_A && K == rows_B) {
        integer NK = N * K;
        res =
#ifdef LACAML_COMPLEX
          (TRANSA == 'C')
          ? DOTC(&NK, A_data, &integer_one, B_data, &integer_one)
          :
#endif
        DOTU(&NK, A_data, &integer_one, B_data, &integer_one);
        goto end;
      } else {
        iter_incr_A = rows_A;
        dot_incr_A = 1;
        iter_incr_B = rows_B;
        dot_incr_B = 1;
      }
    } else {
      if (N < K) {
        NUMBER *tmp_A_data = A_data;
        integer tmp_N = N;
        A_data = B_data;
        B_data = tmp_A_data;
        N = K;
        K = tmp_N;
        iter_incr_A = rows_B;
        dot_incr_A = 1;
        iter_incr_B = 1;
        dot_incr_B = rows_A;
      } else {
        iter_incr_A = rows_A;
        dot_incr_A = 1;
        iter_incr_B = 1;
        dot_incr_B = rows_B;
      }
    }
  }

  stop_A = A_data + N * iter_incr_A;

#ifndef LACAML_COMPLEX          /* Real number */
  COMMON_GEMM_TRACE_LOOP
#else                           /* Complex number */
  if (TRANSA == 'C')
    if (TRANSB == 'C')
      while (A_data != stop_A) {
        NUMBER cd = DOTU(&K, A_data, &dot_incr_A, B_data, &dot_incr_B);
        NUMBER d = COMLEX_CONJ(cd);
        res = ADD_NUMBER(res, d);
        GEMM_TRACE_INCR;
      }
    else
      while (A_data != stop_A) {
        NUMBER d = DOTC(&K, A_data, &dot_incr_A, B_data, &dot_incr_B);
        res = ADD_NUMBER(res, d);
        GEMM_TRACE_INCR;
      }
  else if (TRANSB == 'C')
    while (A_data != stop_A) {
      NUMBER d = DOTC(&K, B_data, &dot_incr_B, A_data, &dot_incr_A);
      res = ADD_NUMBER(res, d);
      GEMM_TRACE_INCR;
    }
  else COMMON_GEMM_TRACE_LOOP
#endif

end:

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturnNUMBER(res);
}

CAMLprim value LFUN(gemm_trace_stub_bc)(value *argv, int __unused argn)
{
  return
    COPY_NUMBER(
        LFUN(gemm_trace_stub)(
          argv[0],
          argv[1],
          Int_val(argv[2]),
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          argv[6],
          Int_val(argv[7]),
          Int_val(argv[8]),
          argv[9]));
}


/* syrk_trace */

CAMLprim vNUMBER LFUN(syrk_trace_stub)(
  intnat vN, intnat vK, intnat vAR, intnat vAC, value vA)
{
  CAMLparam1(vA);
  integer GET_INT(N), GET_INT(K);
  MAT_PARAMS(A);
  NUMBER res = NUMBER_ZERO;
  caml_enter_blocking_section();  /* Allow other threads */
  if (N == rows_A) {
    integer NK = N * K;
    res = DOTU(&NK, A_data, &integer_one, A_data, &integer_one);
  } else {
    NUMBER *stop_A = A_data + K * rows_A;
    while (A_data != stop_A) {
      NUMBER d = DOTU(&N, A_data, &integer_one, A_data, &integer_one);
      res = ADD_NUMBER(res, d);
      A_data += rows_A;
    }
  }
  caml_leave_blocking_section();  /* Disallow other threads */
  CAMLreturnNUMBER(res);
}

CAMLprim value LFUN(syrk_trace_stub_bc)(
    value vN, value vK, value vAR, value vAC, value vA)
{
  return
    COPY_NUMBER(
        LFUN(syrk_trace_stub)(
          Int_val(vN),
          Int_val(vK),
          Int_val(vAR),
          Int_val(vAC),
          vA));
}


/* symm2_trace */

CAMLprim vNUMBER LFUN(symm2_trace_stub)(
  intnat vN,
  value vUPLOA, intnat vAR, intnat vAC, value vA,
  value vUPLOB, intnat vBR, intnat vBC, value vB)
{
  CAMLparam2(vA, vB);

  integer GET_INT(N);
  char GET_CHAR(UPLOA), GET_CHAR(UPLOB);

  MAT_PARAMS(A);
  MAT_PARAMS(B);

  NUMBER diag_sum, res = NUMBER_ZERO;

  if (N == 0) CAMLreturnNUMBER(number_zero);

  caml_enter_blocking_section();  /* Allow other threads */

  diag_sum = MUL_NUMBERP(A_data, B_data);

  if (UPLOA == 'U') {
    if (UPLOB == 'U') {
      integer i = 1;
      while (i != N) {
        NUMBER d, diag, *diag_A, *diag_B;
        A_data += rows_A;
        B_data += rows_B;
        d = DOTU(&i, A_data, &integer_one, B_data, &integer_one);
        res = ADD_NUMBER(res, d);
        diag_A = A_data + i;
        diag_B = B_data + i;
        diag = MUL_NUMBERP(diag_A, diag_B);
        diag_sum = ADD_NUMBER(diag_sum, diag);
        ++i;
      }
    } else {
      while (--N != 0) {
        NUMBER d, diag;
        A_data += rows_A;
        ++B_data;
        d = DOTU(&N, A_data, &rows_A, B_data, &integer_one);
        res = ADD_NUMBER(res, d);
        A_data += 1;
        B_data += rows_B;
        diag = MUL_NUMBERP(A_data, B_data);
        diag_sum = ADD_NUMBER(diag_sum, diag);
      }
    }
  } else {
    if (UPLOB == 'U') {
      while (--N != 0) {
        NUMBER d, diag;
        ++A_data;
        B_data += rows_B;
        d = DOTU(&N, A_data, &integer_one, B_data, &rows_B);
        res = ADD_NUMBER(res, d);
        A_data += rows_A;
        B_data += 1;
        diag = MUL_NUMBERP(A_data, B_data);
        diag_sum = ADD_NUMBER(diag_sum, diag);
      }
    } else {
      while (--N != 0) {
        NUMBER d, diag;
        ++A_data;
        ++B_data;
        d = DOTU(&N, A_data, &integer_one, B_data, &integer_one);
        res = ADD_NUMBER(res, d);
        A_data += rows_A;
        B_data += rows_B;
        diag = MUL_NUMBERP(A_data, B_data);
        diag_sum = ADD_NUMBER(diag_sum, diag);
      }
    }
  }

  res = ADD_NUMBER(res, res);
  res = ADD_NUMBER(res, diag_sum);

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturnNUMBER(res);
}

CAMLprim value LFUN(symm2_trace_stub_bc)(value *argv, int __unused argn)
{
  return
    COPY_NUMBER(
        LFUN(symm2_trace_stub)(
          Int_val(argv[0]),
          argv[1],
          Int_val(argv[2]),
          Int_val(argv[3]),
          argv[4],
          argv[5],
          Int_val(argv[6]),
          Int_val(argv[7]),
          argv[8]));
}
