/* File: vec_sort.c

   Copyright (C) 2012-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/anum/

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

/* Implementation of quicksort based on the glibc one. */

#ifndef _VEC_SORT
#define _VEC_SORT

#define SWAP(TY, a, b)                          \
  do {                                          \
    TY tmp = *a;                                \
    *a = *b;                                    \
    *b = tmp;                                   \
  } while(0)

/* Discontinue quicksort algorithm when partition gets below this size.
   This particular magic number was chosen to work best on a Sun 4/260. */
#define MAX_THRESH 4

/* The next 4 #defines implement a very fast in-line stack abstraction
   to store unfulfilled partition obligations.  The stack node
   declaration is performed in the body of the function below.

     typedef struct
       {
         NUMBER *lo;
         NUMBER *hi;
       } stack_node;
*/
/* The stack needs log (total_elements) entries (we could even subtract
   log(MAX_THRESH)).  Since total_elements has type size_t, we get as
   upper bound for log (total_elements):
   bits per byte (CHAR_BIT) * sizeof(size_t).  */
#define STACK_SIZE      (8 * sizeof(size_t))
#define PUSH(low, high) ((void) ((top->lo = (low)), (top->hi = (high)), ++top))
#define POP(low, high)  ((void) (--top, (low = top->lo), (high = top->hi)))
#define STACK_NOT_EMPTY (stack < top)

#endif /* _VEC_SORT */

#define QUICKSORT(TY, base_ptr, INCX, max_thresh)                       \
  if (N > MAX_THRESH)                                                   \
    {                                                                   \
      TY *lo = base_ptr;                                                \
      TY *hi = &lo[(N - 1) * INCX];                                     \
      struct {                                                          \
        TY *lo;                                                         \
        TY *hi;                                                         \
      } stack[STACK_SIZE], *top = stack;                                \
                                                                        \
      PUSH (NULL, NULL);                                                \
                                                                        \
      while (STACK_NOT_EMPTY)                                           \
        {                                                               \
          TY *left_ptr;                                                 \
          TY *right_ptr;                                                \
                                                                        \
          /* Select median value from among LO, MID, and HI. Rearrange  \
             LO and HI so the three values are sorted. This lowers the  \
             probability of picking a pathological pivot value and      \
             skips a comparison for both the LEFT_PTR and RIGHT_PTR in  \
             the while loops. */                                        \
                                                                        \
          TY *mid = lo + INCX * ((hi - lo) / INCX >> 1);                \
                                                                        \
          if (QUICKSORT_LT(mid, lo)) {                                  \
            SWAP (TY, mid, lo);                                         \
          }                                                             \
          if (QUICKSORT_LT(hi, mid)) {                                  \
            SWAP (TY, mid, hi);                                         \
          }                                                             \
          else                                                          \
            goto jump_over;                                             \
          if (QUICKSORT_LT(mid, lo)) {                                  \
            SWAP (TY, mid, lo);                                         \
          }                                                             \
          jump_over:;                                                   \
                                                                        \
          left_ptr  = lo + INCX;                                        \
          right_ptr = hi - INCX;                                        \
                                                                        \
          /* Here's the famous ``collapse the walls'' section of quicksort. \
             Gotta like those tight inner loops!  They are the main reason \
             that this algorithm runs much faster than others. */       \
          do                                                            \
            {                                                           \
              while (QUICKSORT_LT(left_ptr, mid))                       \
                left_ptr += INCX;                                       \
                                                                        \
              while (QUICKSORT_LT(mid, right_ptr))                      \
                right_ptr -= INCX;                                      \
                                                                        \
              if (left_ptr < right_ptr)                                 \
                {                                                       \
                  SWAP (TY, left_ptr, right_ptr);                       \
                  if (mid == left_ptr)                                  \
                    mid = right_ptr;                                    \
                  else if (mid == right_ptr)                            \
                    mid = left_ptr;                                     \
                  left_ptr += INCX;                                     \
                  right_ptr -= INCX;                                    \
                }                                                       \
              else if (left_ptr == right_ptr)                           \
                {                                                       \
                  left_ptr += INCX;                                     \
                  right_ptr -= INCX;                                    \
                  break;                                                \
                }                                                       \
            }                                                           \
          while (left_ptr <= right_ptr);                                \
                                                                        \
          /* Set up pointers for next iteration.  First determine whether \
             left and right partitions are below the threshold size.  If so, \
             ignore one or both.  Otherwise, push the larger partition's \
             bounds on the stack and continue sorting the smaller one. */ \
                                                                        \
          if ((size_t) (right_ptr - lo) <= max_thresh)                  \
            {                                                           \
              if ((size_t) (hi - left_ptr) <= max_thresh)               \
                /* Ignore both small partitions. */                     \
                POP (lo, hi);                                           \
              else                                                      \
                /* Ignore small left partition. */                      \
                lo = left_ptr;                                          \
            }                                                           \
          else if ((size_t) (hi - left_ptr) <= max_thresh)              \
            /* Ignore small right partition. */                         \
            hi = right_ptr;                                             \
          else if ((right_ptr - lo) > (hi - left_ptr))                  \
            {                                                           \
              /* Push larger left partition indices. */                 \
              PUSH (lo, right_ptr);                                     \
              lo = left_ptr;                                            \
            }                                                           \
          else                                                          \
            {                                                           \
              /* Push larger right partition indices. */                \
              PUSH (left_ptr, hi);                                      \
              hi = right_ptr;                                           \
            }                                                           \
        }                                                               \
    }                                                                   \
                                                                        \
  /* Once the BASE_PTR array is partially sorted by quicksort the rest  \
     is completely sorted using insertion sort, since this is efficient \
     for partitions below MAX_THRESH size. BASE_PTR points to the beginning \
     of the array to sort, and END_PTR points at the very last element in \
     the array (*not* one beyond it!). */                               \
  {                                                                     \
    TY *const end_ptr = &base_ptr[(N - 1) * INCX];                      \
    TY *tmp_ptr = base_ptr;                                             \
    TY *thresh = /* min(end_ptr, base_ptr + max_thresh) */              \
      end_ptr < (base_ptr + max_thresh) ? end_ptr : (base_ptr + max_thresh); \
    register TY *run_ptr;                                               \
                                                                        \
    /* Find smallest element in first threshold and place it at the     \
       array's beginning.  This is the smallest array element,          \
       and the operation speeds up insertion sort's inner loop. */      \
                                                                        \
    for (run_ptr = tmp_ptr + INCX; run_ptr <= thresh; run_ptr += INCX)  \
      if (QUICKSORT_LT(run_ptr, tmp_ptr))                               \
        tmp_ptr = run_ptr;                                              \
                                                                        \
    if (tmp_ptr != base_ptr) {                                          \
      SWAP (TY, tmp_ptr, base_ptr);                                     \
    }                                                                   \
                                                                        \
    /* Insertion sort, running from left-hand-side up to right-hand-side.  */ \
                                                                        \
    run_ptr = base_ptr + INCX;                                          \
    while ((run_ptr += INCX) <= end_ptr)                                \
      {                                                                 \
        tmp_ptr = run_ptr - INCX;                                       \
        while (QUICKSORT_LT(run_ptr, tmp_ptr))                          \
          tmp_ptr -= INCX;                                              \
                                                                        \
        tmp_ptr += INCX;                                                \
        if (tmp_ptr != run_ptr)                                         \
          {                                                             \
            TY *trav;                                                   \
                                                                        \
            trav = run_ptr + INCX;                                      \
            while (--trav >= run_ptr)                                   \
              {                                                         \
                TY c = *trav;                                           \
                TY *hi, *lo;                                            \
                                                                        \
                for (hi = lo = trav; (lo -= INCX) >= tmp_ptr; hi = lo)  \
                  *hi = *lo;                                            \
                *hi = c;                                                \
              }                                                         \
          }                                                             \
      }                                                                 \
  }


CAMLprim value NAME(value vCMP, value vN,
                    value vOFSX, value vINCX, value vX)
{
  CAMLparam2(vCMP, vX);
#if defined(OCAML_SORT_CALLBACK)
  CAMLlocal2(va, vb);
#endif
  const size_t GET_INT(N);
  int GET_INT(INCX);
  VEC_PARAMS(X);

  NUMBER *const base_ptr = X_data;
  const size_t max_thresh = MAX_THRESH * sizeof(NUMBER) * INCX;

  if (N == 0) CAMLreturn(Val_unit);

#ifndef OCAML_SORT_CALLBACK
  caml_enter_blocking_section();  /* Allow other threads */
#endif

#define QUICKSORT_LT(a, b) OCAML_SORT_LT((*a), (*b))
  QUICKSORT(NUMBER, base_ptr, INCX, max_thresh);
#undef QUICKSORT_LT

#ifndef OCAML_SORT_CALLBACK
  caml_leave_blocking_section();  /* Disallow other threads */
#endif

  CAMLreturn(Val_unit);
}


CAMLprim value NAME_PERM(value vCMP, value vN,
                         value vOFSP, value vINCP, value vP,
                         value vOFSX, value vINCX, value vX)
{
  CAMLparam3(vCMP, vP, vX);
#if defined(OCAML_SORT_CALLBACK)
  CAMLlocal2(va, vb);
#endif
  const size_t GET_INT(N);
  int GET_INT(INCX),
      GET_INT(INCP);
  VEC_PARAMS(X);
  intnat OFSX = Long_val(vOFSX);
  intnat *P_data = ((intnat *) Caml_ba_data_val(vP)) + (Long_val(vOFSP) - 1);
  int i;

  NUMBER *const X = X_data - OFSX;  /* so P values are FORTRAN indices */
  intnat *const base_ptr = P_data;
  const size_t max_thresh = MAX_THRESH * sizeof(intnat) * INCP;

  if (N == 0) CAMLreturn(Val_unit);

#ifndef OCAML_SORT_CALLBACK
  caml_enter_blocking_section();  /* Allow other threads */
#endif

  /* Initialize the permutation to the "identity". */
  for(i = 0; i < N; i += 1)
    P_data[i * INCP] = OFSX + i * INCX;
#define QUICKSORT_LT(a, b) OCAML_SORT_LT((X[*a]), (X[*b]))
  QUICKSORT(intnat, base_ptr, INCP, max_thresh);
#undef QUICKSORT_LT

#ifndef OCAML_SORT_CALLBACK
  caml_leave_blocking_section();  /* Disallow other threads */
#endif

  CAMLreturn(Val_unit);
}


CAMLprim value BC_NAME_PERM(value *argv, int argn)
{
  return NAME_PERM(argv[0], argv[1], argv[2], argv[3], argv[4],
                   argv[5], argv[6], argv[7]);
}

#undef NAME
#undef NAME_PERM
#undef BC_NAME_PERM
#undef OCAML_SORT_LT
