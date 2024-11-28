#!/usr/bin/env bash

set -eu

dune build "${@/%.ml/.exe}"

for file in *.ml; do
  ex=$(basename "$file" .ml)
  exexec=$ex.exe
  echo TESTING "$ex" ==================================================
  ../_build/default/examples/"$exexec"
done
