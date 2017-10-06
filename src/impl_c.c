/* File: impl_c.c

   Copyright (C) 2001-

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

#include "lacaml_macros.h"

/** ILAENV: fetch problem-dependent parameters for LAPACK-functions */

extern integer ilaenv_(
  integer *ISPEC, char *NAME, char *OPTS,
  integer *N1, integer *N2, integer *N3, integer *N4,
  ftnlen name_len, ftnlen opts_len);

CAMLprim intnat lacaml_ilaenv_stub(
  intnat vISPEC, value vNAME, value vOPTS,
  intnat vN1, intnat vN2, intnat vN3, intnat vN4)
{
  integer GET_INT(ISPEC),
          GET_INT(N1),
          GET_INT(N2),
          GET_INT(N3),
          GET_INT(N4);

  char *NAME = String_val(vNAME),
       *OPTS = String_val(vOPTS);

  ftnlen NAME_LEN = caml_string_length(vNAME),
         OPTS_LEN = caml_string_length(vOPTS);

  return ilaenv_(&ISPEC, NAME, OPTS, &N1, &N2, &N3, &N4, NAME_LEN, OPTS_LEN);
}

CAMLprim value lacaml_ilaenv_stub_bc(value *argv, int __unused argn)
{
  return
    Val_int (
        lacaml_ilaenv_stub(
          Int_val(argv[0]),
          argv[1],
          argv[2],
          Int_val(argv[3]),
          Int_val(argv[4]),
          Int_val(argv[5]),
          Int_val(argv[6])));
}
