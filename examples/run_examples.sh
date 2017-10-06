#!/bin/bash

set -e

for dir in `find . -mindepth 1 -maxdepth 1 -type d`; do
  cd $dir
  for file in *.ml; do
    ex=`basename $file .ml`
    exexec=$ex.exe
    echo TESTING $dir/$ex ==================================================
    jbuilder build $exexec
    ../../_build/default/examples/$dir/$exexec
  done
  cd ..
done
