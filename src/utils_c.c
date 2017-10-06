/* File: utils_c.c

   Copyright (C) 2005-

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

// Enable POSIX.1-2001 for use of `struct timespec' and `nanosleep'
#define _POSIX_C_SOURCE 200112L

#include <math.h>
#include <caml/alloc.h>

#include "utils_c.h"

#ifdef WIN32
  #include <windows.h>
#else
  #include <time.h>
#endif // WIN32

/* Store two doubles in an OCaml-block (complex number) */
value copy_two_doubles(double d0, double d1)
{
  value res = caml_alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, d0);
  Store_double_field(res, 1, d1);
  return res;
}


/* Portable sleep function */
int portable_sleep(int milliseconds)
{
#ifdef WIN32
  Sleep(milliseconds);
  return 0;
#else
  struct timespec tim, tim2;
  tim.tv_sec = 0;
  tim.tv_nsec = milliseconds * 1000000;

  return nanosleep(&tim , &tim2);
#endif // WIN32
}


/* exp10 */

#ifdef EXTERNAL_EXP10

#ifndef M_LN10
#define M_LN10 2.30258509299404568402  /* log_e 10 */
#endif

float exp10f(float arg) { return expf(M_LN10 * arg); }
double exp10(double arg) { return exp(M_LN10 * arg); }

#endif /* EXTERNAL_EXP10 */
