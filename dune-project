(lang dune 1.10)
(name lacaml)

(generate_opam_files true)

(source (github mmottl/lacaml))
(license "LGPL-2.1+ with OCaml linking exception")
(homepage "https://mmottl.github.io/lacaml")
(documentation "https://mmottl.github.io/lacaml/api")

(maintainers
  "Markus Mottl <markus.mottl@gmail.com>"
  "Christophe Troestler <Christophe.Troestler@umons.ac.be>"
)

(authors
  "Egbert Ammicht <eammicht@lucent.com>"
  "Patrick Cousot <Patrick.Cousot@ens.fr>"
  "Sam Ehrlichman <sehrlichman@janestreet.com>"
  "Florent Hoareau <h.florent@gmail.com>"
  "Markus Mottl <markus.mottl@gmail.com>"
  "Liam Stewart <liam@cs.toronto.edu>"
  "Christophe Troestler <Christophe.Troestler@umons.ac.be>"
  "Oleg Trott <ot14@columbia.edu>"
  "Martin Willensdorfer <ma.wi@gmx.at>"
)

(package
  (name lacaml)
  (synopsis "Lacaml - OCaml-bindings to BLAS and LAPACK")
  (description "\
Lacaml interfaces the BLAS-library (Basic Linear Algebra Subroutines) and
LAPACK-library (Linear Algebra routines).  It also contains many additional
convenience functions for vectors and matrices.")
  (depends
    (ocaml (>= 4.08))
    (dune (>= 1.10))
    dune-configurator
    (conf-blas :build)
    (conf-lapack :build)
    base-bytes
    base-bigarray
  )
  (tags (clib:lapack clib:blas))
)
