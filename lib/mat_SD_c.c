/* File: mat_SD_c.c

   Copyright (C) 2005-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

     Liam Stewart
     email: liam@cs.toronto.edu
     WWW: http://www.cs.toronto.edu/~liam

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

/* $Id: mat_SD_c.c,v 1.5 2005/03/22 16:18:06 mottl Exp $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/signals.h>

#include <math.h>

#include "f2c.h"

#include "lacaml_macros.h"

CAMLprim value LFUN(map_stub)(
  value vM, value vN,
  value vAR, value vAC,
  value vA,
  value vCR, value vCC,
  value vC,
  value vClosure)
{
  CAMLparam3(vA, vC, vClosure);
  CAMLlocal1(v_res);

  int GET_INT(M), GET_INT(N);

  MAT_PARAMS(A);
  MAT_PARAMS(C);

  REAL *entry_a, *col_a;
  REAL *entry_c, *col_c;
  REAL *col_end = A_data + N*M, *entry_end;

  int cnt;

  for (col_a = A_data, col_c = C_data;
       col_a < col_end;
       col_a += rows_A, col_c += rows_C)
  {
    for (entry_a = col_a, entry_end = col_a + M, entry_c = col_c;
         entry_a < entry_end;
         entry_a++, entry_c++)
    {
      value v_entry_a = copy_double(*entry_a);
      v_res = caml_callback(vClosure, v_entry_a);
      *entry_c = Double_val(v_res);
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value LFUN(map_stub_bc)(value *argv, int argn)
{
  return LFUN(map_stub)(
    argv[0], argv[1], argv[2], argv[3], argv[4],
    argv[5], argv[6], argv[7], argv[8]);
}
