#!/bin/sh

if [ ! -f cl-ppcre.sbcl ]; then
  echo "PPCRE core not found, attempting to build..."
  sbcl --noinform --no-userinit --eval "(require :asdf)" --load build-ppcre-core.lisp
fi

cat "$@" | ./cl-ppcre.sbcl --no-userinit --load syntax.lisp --noprint --noinform
