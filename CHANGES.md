### 11.0.0 (2018-02-28)

  _WARNINIG_ _WARNING_ _WARNING_

  * User code changes in existing code required!  User code may still compile,
    but can behave differently!

    The following functions are affected:

      * `potrf`
      * `potri`
      * `potrs`

    The above functions now do not support the `jitter` argument anymore.
    Users should remove the flag from calls to the above functions and
    call the new `Mat.add_const_diag` function if they need to add jitter.
    This call should happen right before the (now required) call to `potrf`.

    More importantly, `potri` and `potrs` now do not support the `factorize`
    flag anymore, which would call `potrf` automatically beforehand.  This was
    the (ill-conceived) default, which makes it harder to port LAPACK code
    to Lacaml.  In order to upgrade your code, please do the following:

      * If `potri` or `potrs` were passed `~factorize:false`, just remove
        the flag.

      * If `potri` or `potrs` were passed `~factorize:true` _or were called
        without the `factorize` flag_, remove the flag if necessary and call
        `potrf` with the exactly corresponding arguments before.

    Luckily, these functions are typically used rarely, and the changes
    are trivial.  Apologies anyway for the churn!

  * New functions

      * `Mat.add_const_diag` for adding a constant to the diagonal of a
        (sub-)matrix.

  * `orgqr` now detects if `m < n` and raises an exception instead of printing
    a Fortran error message and continuing.

  * Eigenvalue offsets should now work correctly.

  * Improved documentation.


### 10.0.2 (2017-11-08)

  * Fixed bugs accessing lower pentagonal matrix patterns

  * Fixed library override issue on Mac OS X


### 10.0.1 (2017-10-21)

  * Fixed wrongly capitalized build targets missed due to Mac OS X file
    system case insensitivity.


### 10.0.0 (2017-10-20)

  * Switched to jbuilder and topkg

  * API changes

      * `trmm` and `trsm` now do not label argument `a` anymore

      * Many matrix functions now support an optional `patt` argument,
        which can be used to specify rectangular, triagonal, trapezoidal, and
        pentagonal patterns on which to perform an operation.

      * New functions

        * `Mat.sum_prod` computes the sum of element-wise products of
          two matrices.  Some use cases are already covered by `Mat.gemm_trace`,
          but the latter does not support patterns.

  * Improved C-code to better support SIMD compiler optimizations

  * Many internal improvements, including untagged and unboxed passing of
    parameters to and from external functions.

  * Compilation now uses `-march=native -O3 -ffast-math` by default, which
    should be safe and exploit SIMD on platforms that support it to greatly
    improve performance of some operations.

  * Improved documentation

  * Improved configuration and build process
