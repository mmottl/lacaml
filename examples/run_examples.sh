#!/bin/bash

set -e

dune build `echo *.ml | sed -e 's/.ml/.exe/g'`

for file in *.ml; do
  ex=`basename $file .ml`
  exexec=$ex.exe
  echo TESTING $dir/$ex ==================================================
  ../_build/default/examples/$dir/$exexec
done
