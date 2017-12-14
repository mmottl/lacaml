## Lacaml - Linear Algebra for OCaml

### What is Lacaml?

This [OCaml](http://www.ocaml.org)-library interfaces two widely used
mathematical FORTRAN-libraries:

  * [BLAS - Basic Linear Algebra Subroutines](http://www.netlib.org/blas/index.html)
  * [LAPACK - Linear Algebra PACKage](http://www.netlib.org/lapack/index.html)

This allows developers to write high-performance numerical code for
applications that require linear algebra.

### Features

  * The BLAS- and LAPACK-libraries have evolved over about two decades of
    time and are therefore extremely mature both in terms of stability
    and performance.

  * Lacaml interfaces most of the functions in BLAS and LAPACK (many
    hundreds!).  It supports among other things linear equations, least
    squares problems, eigenvalue problems, singular value decomposition
    (SVD), Cholesky and QR-factorization, etc.

  * Many convenience functions for creating and manipulating matrices.

  * Powerful printing functions for large vectors and matrices and supplemental
    information (e.g. row and column headers).  Users can specify easily how
    much context to print.  For example, it is usually sufficient to print
    small blocks of the four corners of a large result matrix to manually
    verify the correctness of an algorithm.  Lacaml uses this approach to
    limit the output to human-manageable size.

  * Integration into the OCaml-toplevel allows for easy experimentation for
    students and researchers as well as demonstration for lecturers.
    Values of vector and matrix type will be printed automatically without
    cluttering the screen.

  * The OCaml-interface was designed in a way to combine both the possibility
    of gaining optimum efficiency (e.g. by allowing the creation of work arrays
    outside of loops) with simplicity (thanks to labels and default arguments).

  * The code is precision-independent and supports both real and complex
    transforms in a consistent way.  There are four modules that implement
    the same interface modulo the precision type and specialized real/complex
    functions.  If you refer to elements in this interface only, your code
    becomes precision- and (if meaningful) real/complex independent, too:
    you can choose at anytime whether you want to use single-precision or
    double-precision simply by referring to the required module.

  * You can fully exploit the library within multithreaded programs.  Many
    numerical routines are likely to run for a long time, but they will never
    block other threads.  This also means that you can execute several routines
    at the same time on several processors if you use POSIX-threads in OCaml.

  * To make things easy for developers used to the "real" implementation
    in FORTRAN but also for beginners who need detailed documentation, both
    function- and argument names have been kept compatible to the ones used
    in the BLAS- and LAPACK-documentation.  Only exception: you need not
    prefix functions with `s`, `d`, `c` or `z` to indicate the precision
    and type of numbers, because the OCaml module system provides us with
    a more convenient means of choosing them.

  * (Almost) all errors are handled within OCaml.  Typical mistakes like
    passing non-conforming matrices, parameters that are out of range, etc.,
    will be caught before calling Fortran code and will raise exceptions.
    These exceptions will explain the error in detail, for example the
    received illegal parameter and the range of expected legal values.

### Using Lacaml

You can make use of this library by referring to the corresponding module
for the required precision and number type.  E.g.:

```ocaml
open Lacaml.S  (* Single-precision real numbers *)
open Lacaml.D  (* Double-precision real numbers *)
open Lacaml.C  (* Single-precision complex numbers *)
open Lacaml.Z  (* Double-precision complex numbers *)
```

These modules become available if you link the `lacaml`-library with your
application.  The widely used OCaml-tool `findlib` will take care of linking
`lacaml` correctly.  If you do not use this tool, you will also have to link
in the `bigarray`-library provided by the OCaml-distribution.

The `Lacaml.?`-modules implement the BLAS/LAPACK-interface.  Their
corresponding submodules `Vec` and `Mat` provide for vector and matrix
operations that relate to the given precision and number type.

Most functions were implemented using optional arguments (= default arguments).
If you do not provide them at the call-site, sane defaults will be used
instead.  Here is an example of a function call:

```ocaml
let rank = gelss in_mat out_mat in
(* ... *)
```

This example computes the solution to a general least squares problem (=
linear regression) using the SVD-algorithm with `in_mat` as the matrix
containing the predictor variables and `out_mat` as the matrix containing
(possibly many) response variables (this function can handle several response
variables at once).  The result is the rank of the matrix.  The matrices
provided in the arguments will be overwritten with further results (here:
the singular vectors and the solution matrix).

If the above happened in a loop, this would be slightly inefficient, because
a work-array would have to be allocated (and later deallocated) at each call.
You can hoist the creation of this work array out of the loop, e.g. (`m`,
`n`, `nrhs` are problem dependent parameters):

```ocaml
let work = gelss_min_work ~m ~n ~nrhs in
for i = 1 to 1000 do
  (* ... *)
  let rank = gelss in_mat ~work out_mat in
  (* ... *)
done
```

All matrices can be accessed in a restricted way, i.e. you can specify
submatrices for all matrix parameters.  For example, if some matrix is called
`a` in the interface documentation, you can specify the left upper corner of
the wanted submatrix for the operation by setting `ar` for the row and `ac`
for the column (1 by default).  A vector `y` would have an extra optional
parameter `ofsy` (also 1 by default).  Parameters like `m` or `n` typically
specify the numbers of rows or columns.

#### Printing vectors and matrices

Here is a toplevel example of printing a large random matrix:

```ocaml
# #require "lacaml";;
# open Lacaml.D;;
# let mat = Mat.random 100 200;;
val mat : Lacaml.D.mat =
              C1        C2        C3          C198       C199      C200
    R1 -0.314362 -0.530711  0.309887 ...  0.519965  -0.230156 0.0479154
    R2  0.835658  0.581404  0.161607 ... -0.749358  -0.630019 -0.858998
    R3 -0.403421  0.458116 -0.497516 ...  0.210811   0.422094  0.589661
             ...       ...       ... ...       ...        ...       ...
   R98 -0.352474  0.878897  0.357842 ...  0.150786   -0.74011  0.353253
   R99  0.104805  0.984924 -0.319127 ... -0.143679  -0.858269  0.859059
  R100  0.419968  0.333358  0.237761 ... -0.483535 -0.0224016  0.513944
```

Only the corner sections of the matrix, which would otherwise be too large
to display readably, are being printed, and ellipses (`...`) are used in
place of the removed parts of the matrix.

If the user required even less context, the `Lacaml.Io.Toplevel.lsc` function,
which is also available in each precision module for convenience (here:
`Lacaml.D`), could be used to indicate how much.  In the following example
only two-by-two blocks are requested in each corner of the matrix:

```ocaml
# lsc 2;;
- : unit = ()
# mat;;
- : Lacaml.D.mat =
            C1        C2           C199      C200
  R1 -0.314362 -0.530711 ...  -0.230156 0.0479154
  R2  0.835658  0.581404 ...  -0.630019 -0.858998
           ...       ... ...        ...       ...
 R99  0.104805  0.984924 ...  -0.858269  0.859059
R100  0.419968  0.333358 ... -0.0224016  0.513944
```

Applications can use the standard `Format`-module in the OCaml-distribution
together with Lacaml printing functions to output vectors and matrices.
Here is an example using labels and showing the high customizability of the
printing functions:

```ocaml
open Lacaml.D
open Lacaml.Io

let () =
  let rows, cols = 200, 100 in
  let a = Mat.random rows cols in
  Format.printf "@[<2>This is an indented random matrix:@\n@\n%a@]@."
    (Lacaml.Io.pp_lfmat
      ~row_labels:
        (Array.init rows (fun i -> Printf.sprintf "Row %d" (i + 1)))
      ~col_labels:
        (Array.init cols (fun i -> Printf.sprintf "Col %d" (i + 1)))
      ~vertical_context:(Some (Context.create 2))
      ~horizontal_context:(Some (Context.create 3))
      ~ellipsis:"*"
      ~print_right:false
      ~print_foot:false ())
    a
```

The above code might print:

```text
This is an indented random matrix:

              Col 1     Col 2       Col 3      Col 98    Col 99   Col 100
    Row 1  0.852078 -0.316723    0.195646 *  0.513697  0.656419  0.545189
    Row 2 -0.606197  0.411059    0.158064 * -0.368989    0.2174    0.9001
                  *         *           * *         *         *         *
  Row 199 -0.684374 -0.939027 0.000699582 *  0.117598 -0.285587 -0.654935
  Row 200  0.929341 -0.823264    0.895798 *  0.198334  0.725029 -0.621723
```

Many other options, e.g. for different padding, printing numbers in
other formats or with different precision, etc., are available for output
customization.

#### Error handling

Though Lacaml is quite thorough in checking arguments for consistency with
BLAS/LAPACK, an exception to the above is illegal contents of vectors and
matrices.  This can happen, for example, when freshly allocated matrices
are used without initialization.  Some LAPACK-algorithms may not be able
to deal with floats that correspond to NaNs, infinities, or are subnormal.
Checking matrices on every call would seem excessive.  Some functions also
expect matrices with certain properties, e.g. positive-definiteness, which
would be way too costly to verify beforehand.

Degenerate value shapes, e.g. empty matrices and vectors, and zero-sized
operations may also be handled inconsistently by BLAS/LAPACK itself.  It is
rather difficult to detect all such corner cases and to predetermine for
all on how they should be handled to provide a sane workaround.

_Users are well-advised to to ensure the sanity of the contents of values
passed to Lacaml functions and to avoid calling Lacaml with values having
degenerate dimensions.  User code should either raise exceptions if values
seem degenerate or handle unusual corner cases explicitly._

#### Other sources of usage information

##### API documentation

Besides the Lacaml interface file, the API documentation can also be found
[online](http://mmottl.github.io/lacaml/api/lacaml).

##### BLAS/LAPACK man pages

BLAS and LAPACK binary packages for Unix operating systems usually come
with appropriate man-pages.  E.g. to quickly look up how to factorize a
positive-definite, complex, single precision matrix, you might enter:

```sh
man cpotrf
```

The corresponding function in Lacaml would be `Lacaml.C.potrf`.  The naming
conventions and additional documentation for BLAS and LAPACK can be found
at their respective websites.

##### Examples

The `examples`-directory contains several demonstrations of how to use this
library for various linear algebra problems.

### Improving Performance

It is highly recommended that users install a variant of BLAS (or even
LAPACK) that has been optimized for their system.  Processor vendors
(e.g. Intel) usually sell the most optimized implementations for their
CPU-architectures.  Some computer and OS-vendors like Apple distribute their
own implementations with their products, e.g. `vecLib`, which is part of
Apple's `Accelerate`-framework.

There is also [ATLAS](http://www.netlib.org/atlas), a very efficient and
compatible substitute for BLAS.  It specializes code for the architecture it
is compiled on.  Binary packages (e.g. RPMs) for Linux should be available
from your distribution vendor's site (you must recompile the package to
make sure it is suited to your distribution, see the package documentation
for more details.).

Another alternative for BLAS is [OpenBLAS](https://github.com/xianyi/OpenBLAS).

If a non-standard library or library location is required, the user can override
the platform-dependent default by setting the following environment variables:

  * `LACAML_CFLAGS`
  * `LACAML_LIBS`

The first one can be used to add compilation flags, and the second one to
override the default linking flags (`-lblas` and `-llapack`).

Lacaml already passes `-O3 -march=native -ffast-math` as compiler flags to fully
exploit SIMD instructions when supported by the used platform.  The current
Lacaml code base is probably safe with these options.

### Contact Information and Contributing

Please submit bugs reports, feature requests, contributions and similar to
the [GitHub issue tracker](https://github.com/mmottl/lacaml/issues).

Up-to-date information is available at: <https://mmottl.github.io/lacaml>
