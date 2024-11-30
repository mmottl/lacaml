# Lacaml - Linear Algebra for OCaml

## Overview

Lacaml is an [OCaml](http://www.ocaml.org) library that interfaces with two
popular FORTRAN libraries, enabling developers to create high-performance
numerical applications requiring linear algebra. Ideal for researchers,
engineers, and developers working on scientific computing and data analysis.

- [BLAS](http://www.netlib.org/blas): Basic Linear Algebra Subroutines
- [LAPACK](http://www.netlib.org/lapack): Linear Algebra PACKage

## Features

- BLAS/LAPACK are mature libraries, offering stability and performance.
- Lacaml interfaces with most BLAS/LAPACK functions, supporting linear
  equations, least squares, eigenvalue problems, SVD, Cholesky, QR
  factorization, and more.
- Provides functions for easy matrix creation and manipulation.
- Offers powerful printing functions for large matrices, allowing users to
  specify how much context to print.
- Integrates with the OCaml toplevel for easy experimentation and demonstration.
- Designed for efficiency and simplicity, with support for pre-allocated work
  arrays to optimize performance in iterative computations.
- Supports both 32-bit and 64-bit floating-point precision for real and
  complex numbers.
- Compatible with multithreaded programs, allowing concurrent execution.
- Function and argument names align with BLAS/LAPACK documentation for
  familiarity.
- OCaml manages most errors, with exceptions for illegal vector/matrix contents.

## Usage

To use Lacaml, open the appropriate module for the desired precision and
number type:

```ocaml
open Lacaml.S  (* Single-precision real numbers *)
open Lacaml.D  (* Double-precision real numbers *)
open Lacaml.C  (* Single-precision complex numbers *)
open Lacaml.Z  (* Double-precision complex numbers *)
```

Link the `lacaml` library with your application. If not using `findlib`,
also link the `bigarray` library.

The `Lacaml.?` modules offer the BLAS/LAPACK interface, with `Vec` and
`Mat` submodules for vector and matrix operations.

Functions use optional arguments with defaults. For example:

```ocaml
let rank = gelss in_mat out_mat in
(* `gelss` solves linear least squares problems *)
...
```

To optimize, create work arrays outside loops:

```ocaml
let work = gelss_min_work ~m ~n ~nrhs in
for i = 1 to 1000 do
  let rank = gelss in_mat ~work out_mat in
  ...
done
```

Access submatrices by specifying parameters like `ar` and `ac` for row and
column offsets, and `m` and `n` for the submatrix dimensions.

### Printing

To print large matrices in the OCaml toplevel, you can use Lacaml's printing
functions. Here's an example with a large random matrix:

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

The output displays the corners of the matrix, with ellipses (`...`) for
omitted parts. To reduce context further, use `Lacaml.Io.Toplevel.lsc` to
specify the number of rows and columns to display:

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

For custom output, use the `Format` module with Lacaml's printing functions.
Here's an example with labels and custom settings:

```ocaml
open Lacaml.D
open Lacaml.Io

let () =
  let rows, cols = (200, 100) in
  let a = Mat.random rows cols in
  Format.printf "@[<2>This is an indented random matrix:@\n@\n%a@]@."
    (Lacaml.Io.pp_lfmat
       ~row_labels:(Array.init rows (fun i -> Printf.sprintf "Row %d" (i + 1)))
       ~col_labels:(Array.init cols (fun i -> Printf.sprintf "Col %d" (i + 1)))
       ~vertical_context:(Some (Context.create 2))
       ~horizontal_context:(Some (Context.create 3))
       ~ellipsis:"*" ~print_right:false ~print_foot:false ())
    a
```

This code might produce:

```text
This is an indented random matrix:

              Col 1     Col 2       Col 3      Col 98    Col 99   Col 100
    Row 1  0.852078 -0.316723    0.195646 *  0.513697  0.656419  0.545189
    Row 2 -0.606197  0.411059    0.158064 * -0.368989    0.2174    0.9001
                  *         *           * *         *         *         *
  Row 199 -0.684374 -0.939027 0.000699582 *  0.117598 -0.285587 -0.654935
  Row 200  0.929341 -0.823264    0.895798 *  0.198334  0.725029 -0.621723
```

Lacaml provides options for customizing output, such as padding, number
formats, and precision.

### Error Handling

Lacaml extensively checks arguments to ensure consistency with BLAS/LAPACK
but does not verify the contents of vectors and matrices. Checking for
NaNs, infinities, or subnormal numbers in every matrix on each call is
computationally expensive. Furthermore, some functions require matrices with
specific properties, like positive-definiteness, which are costly to verify.

BLAS/LAPACK may inconsistently handle degenerate shapes, such as empty
matrices or zero-sized operations. Detecting all corner cases and providing
workarounds is challenging.

Users should ensure that data passed to Lacaml functions is valid and avoid
using values with degenerate dimensions. User code should raise exceptions
for suspicious values or explicitly handle unusual cases.

### Supplementary Resources

#### API Documentation

The Lacaml API documentation is available both in the interface file and
[online](http://mmottl.github.io/lacaml/api/lacaml).

#### BLAS/LAPACK Man Pages

Unix systems typically include man pages for BLAS/LAPACK. For example,
to learn about factorizing a positive-definite, complex, single-precision
matrix, use:

```sh
man cpotrf
```

In Lacaml, this corresponds to `Lacaml.C.potrf`. Further naming conventions
and documentation are available on the BLAS/LAPACK websites.

#### Examples

The `examples` directory contains demonstrations for linear algebra problems
using Lacaml.

## Performance Optimization

For optimal performance, install a BLAS variant optimized for your system.
Processor vendors, such as Intel, offer highly optimized implementations. Apple
includes `vecLib` in its `Accelerate` framework.

[ATLAS](http://www.netlib.org/atlas) is another efficient BLAS substitute,
tailored to the architecture it compiles on. Linux users can find binary
packages from their distribution vendors, but recompilation may be necessary
for optimal performance.

[OpenBLAS](https://github.com/xianyi/OpenBLAS) is another alternative.

To use a non-standard library or location, set these environment variables:

- `LACAML_CFLAGS` for extra compilation flags.
- `LACAML_LIBS` to override default linking flags (`-lblas` and `-llapack`).

For CPU-specific optimization, use `-march=native`. In cloud environments,
be cautious as VM changes might affect the CPU. The `-ffast-math` option
can improve performance by allowing aggressive optimizations, but it may
alter standard floating-point behavior, potentially affecting numerical
accuracy and compliance with IEEE standards. Use with caution in applications
requiring precise numerical results. Generally, `-O3` enhances performance,
and Lacaml should perform well with these settings, potentially utilizing
SIMD instructions.

## Contact Information and Contributing

Report bugs, request features, or contribute via the
[GitHub issue tracker](https://github.com/mmottl/lacaml/issues).

For the latest information, visit: <https://mmottl.github.io/lacaml>
