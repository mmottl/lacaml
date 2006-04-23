/* File: exp10.c */

/* extern double exp10(double);
 *   From http://ftp.gnu.org/gnu/glibc/ glibc-2.3.5.tar.gz/math/math.c and
 *   glibc-2.3.5.tar.gz/sysdeps/generic/e_exp10.c
 *     CHANGED BY P. COUSOT FOR MACOSX */

#define M_LN10 2.30258509299404568402  /* log_e 10 */
double exp10 (double arg) { return exp(M_LN10 * arg); }
