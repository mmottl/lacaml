/* DOTU and DOTC wrappers, due to inconsistent BLAS APIs */

#ifdef ZDOT_IS_PROCEDURE
extern void NAME(
    COMPLEX *RES,
#elif ZDOT_IS_FUNCTION
extern COMPLEX NAME(
#else
#error "Could not detect BLAS API for complex DOT"
#endif
    integer *N,
    COMPLEX *X, integer *INCX,
    COMPLEX *Y, integer *INCY);

COMPLEX WRAPPER_NAME(
  integer *N, COMPLEX *X, integer *INCX, COMPLEX *Y, integer *INCY)
{
#ifdef ZDOT_IS_PROCEDURE
  COMPLEX RES;
  NAME(&RES, N, X, INCX, Y, INCY);
  return RES;
#elif ZDOT_IS_FUNCTION
  return NAME(N, X, INCX, Y, INCY);
#endif
}

#undef WRAPPER_NAME
#undef NAME
