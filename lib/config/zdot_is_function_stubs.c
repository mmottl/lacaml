#include <stdio.h>
#include <math.h>
#include <caml/mlvalues.h>

#include "f2c.h"

#ifndef __unused
# if __GNUC__ >= 3
#  define __unused __attribute__ ((unused))
# else
#  define __unused
# endif
#endif

extern doublecomplex zdotc_(
    integer *N,
    doublecomplex *X, integer *OFSX,
    doublecomplex *Y, integer *OFSY);

CAMLprim value lacaml_zdot_is_function_stub(value __unused v_unit)
{
  doublecomplex RES, C = { 42, 43 };
  integer N = 1;
  double expected = C.r * C.r + C.i * C.i;

  RES = zdotc_(&N, &C, &N, &C, &N);

  return Val_bool(fabs(expected - RES.r) < 1e-15);
}
