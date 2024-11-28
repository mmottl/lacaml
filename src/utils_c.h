/* File: utils_c.h

   Copyright © 2005-

   Markus Mottl <markus.mottl@gmail.com>

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
*/

#ifndef UTILS_C
#define UTILS_C

#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdio.h>

/* Compiler pragmas and inlining */

#if defined(__GNUC__) && __GNUC__ >= 3
#ifndef __pure
#define __pure __attribute__((pure))
#endif
#ifndef __const
#define __const __attribute__((const))
#endif
#ifndef __malloc
#define __malloc __attribute__((malloc))
#endif
#ifndef __unused
#define __unused __attribute__((unused))
#endif
#ifndef __likely
#define likely(x) __builtin_expect(!!(x), 1)
#endif
#ifndef __unlikely
#define unlikely(x) __builtin_expect(!!(x), 0)
#endif
#else
#ifndef __pure
#define __pure
#endif
#ifndef __const
#define __const
#endif
#ifndef __malloc
#define __malloc
#endif
#ifndef __unused
#define __unused
#endif
#ifndef __likely
#define likely(x) (x)
#endif
#ifndef __unlikely
#define unlikely(x) (x)
#endif
#endif

/* Create an OCaml record of two floats */
value copy_two_doubles(double d0, double d1);

/* Tries to sleep the given number of milliseconds.
 * Returns 0 on success. */
int portable_sleep(int milliseconds);

typedef enum { UPPER, LOWER } pentagon_kind;

static inline pentagon_kind get_pentagon_kind(value vPKIND) {
  return (pentagon_kind)Int_val(vPKIND);
}

extern double exp10(double arg);

#endif /* UTILS_C */
