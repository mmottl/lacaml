#!/bin/bash

set -e

for dir in `find . -mindepth 1 -maxdepth 1 -type d`; do
  cd $dir
  for file in *.ml; do
    ex=`basename $file .ml`.exe
    echo TESTING $dir/$ex ==================================================
    jbuilder build $ex
    ../../_build/default/examples/$dir/$ex
  done
  cd ..
done
