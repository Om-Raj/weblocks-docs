#!/bin/sh

set -e

### code highlighting preprocessor
if [ ! -f cl-ppcre.sbcl ]; then
  echo "PPCRE core not found, attempting to build..."
  sbcl --noinform --no-userinit --eval "(require :asdf)" --load build-ppcre-core.lisp
fi

rm -f tmp

cat "$1" | ./cl-ppcre.sbcl --no-userinit --load syntax.lisp --noprint --noinform > tmp

mv tmp "$1"

### stx2any

stx2any -thtml "$1" > "$1.html"

