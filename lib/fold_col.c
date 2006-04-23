/* File: fold_col.c

   Copyright (C) 2001-2005

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

/* $Id: fold_col.c,v 1.17 2006/01/18 15:03:39 mottl Exp $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include "f2c.h"

#include "lacaml_macros.h"
#include "lacaml_utils_c.h"

CAMLprim value NAME(value vN, value vOFSX, value vINCX, value vX)
{
  CAMLparam1(vX);

  int GET_INT(N),
      GET_INT(INCX);

  VEC_PARAMS(X);

  NUMBER *start, *last, acc = INIT;

  caml_enter_blocking_section();  /* Allow other threads */

  if (INCX > 0) {
    start = X_data;
    last = start + N*INCX;
  }
  else {
    last = X_data - 1;
    start = last + N*INCX;
  };

  while  (start != last) {
    NUMBER x = *start;
    FUNC(acc, x);
    start += INCX;
  };

  caml_leave_blocking_section();  /* Disallow other threads */

  CAMLreturn(COPY_NUMBER(acc));
}

#undef NAME
#undef INIT
#undef FUNC
