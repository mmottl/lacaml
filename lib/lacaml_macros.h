/* File: lacaml_macros.h

   Copyright (C) 2001-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
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

/* $Id: lacaml_macros.h,v 1.14 2006/01/18 15:03:40 mottl Exp $ */


/* Defines precision-dependent macros */
#ifndef LACAML_DOUBLE           /* Single precision */

#define REAL real
#define COMPLEX complex
#define FABS fabsf

#ifndef LACAML_COMPLEX          /* Real number */
#define FUN(name) s##name##_
#define FUN2(prefix,name) prefix##s##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_S##name

#define CREATE_NUMBERP(name) \
  real name = Double_val(v##name); \
  real *p##name = &name

#define INIT_NUMBER(name)

#else                           /* Complex number */

#define FUN(name) c##name##_
#define FUN2(prefix,name) prefix##c##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_C##name

#define CREATE_NUMBERP(name) \
  complex name; \
  complex *p##name = &name

#define INIT_NUMBER(name) \
  name.r = Double_field(v##name, 0); \
  name.i = Double_field(v##name, 1)

#endif

#else                           /* Double precision */

#define REAL doublereal
#define COMPLEX doublecomplex
#define FABS fabs

#ifndef LACAML_COMPLEX          /* Real number */
#define FUN(name) d##name##_
#define FUN2(prefix,name) prefix##d##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_D##name

#define CREATE_NUMBERP(name) \
  doublereal name; \
  doublereal *p##name = &name

#define INIT_NUMBER(name) \
  name = Double_val(v##name)

#else                           /* Complex number */
#define FUN(name) z##name##_
#define FUN2(prefix,name) prefix##z##name##_ /* -> IxAMAX */
#define LFUN(name) lacaml_Z##name

#define CREATE_NUMBERP(name) \
  doublecomplex name; \
  doublecomplex *p##name = &name

#define INIT_NUMBER(name) \
  name.r = Double_field(v##name, 0); \
  name.i = Double_field(v##name, 1)

#endif

#endif  /* LACAML_DOUBLE */

/* Defines kind of number */
#ifndef LACAML_COMPLEX          /* Real number */
#define NUMBER REAL
#else                           /* Complex number */
#define NUMBER COMPLEX
#endif  /* LACAML_COMPLEX */

/* Fetch integer parameters */
#define GET_INT(V) V = Int_val(v##V)

/* Fetch double parameters */
#define GET_DOUBLE(V) V = Double_val(v##V)

/* Fetch matrix parameters from bigarray */
#define MAT_PARAMS(M) \
  struct caml_bigarray * big_##M = Bigarray_val(v ## M); \
  long *dims_ ## M = big_##M->dim; \
  integer M##R = Int_val(v##M##R); \
  integer M##C = Int_val(v##M##C); \
  integer rows_##M = *dims_##M++; \
  integer cols_##M = *dims_##M; \
  NUMBER *M##_data = (NUMBER *) big_##M->data + M##R + rows_##M*(M##C - 1) - 1

/* Fetch vector parameters from bigarray */
#define VEC_PARAMS(V) \
  struct caml_bigarray * big_##V = Bigarray_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  NUMBER * V##_data = ((NUMBER *) big_##V->data) + (Int_val(vOFS##V) - 1)

/* Fetch vector parameters from real bigarray */
#define RVEC_PARAMS(V) \
  struct caml_bigarray * big_##V = Bigarray_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  REAL * V##_data = ((REAL *) big_##V->data) + (Int_val(vOFS##V) - 1)

/* Fetch vector parameters from bigarray with offset 1 */
#define VEC_PARAMS1(V) \
  struct caml_bigarray * big_##V = Bigarray_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  NUMBER * V##_data = big_##V->data

/* Fetch vector parameters from bigarray with offset 1 */
#define RVEC_PARAMS1(V) \
  struct caml_bigarray * big_##V = Bigarray_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  REAL * V##_data = big_##V->data

/* Fetch vector parameters from integer bigarray */
#define INT_VEC_PARAMS(V) \
  struct caml_bigarray * big_##V = Bigarray_val(v##V); \
  integer dim_##V = *big_##V->dim; \
  integer * V##_data = big_##V->data

/* Split an integer couple (int * int) into two ints */
#define INT_COUPLE(V) \
  integer V##1 = Int_val(Field(v##V, 0)); \
  integer V##2 = Int_val(Field(v##V, 1))
