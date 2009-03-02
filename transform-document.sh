#!/bin/sh

set -e

### code highlighting preprocessor
if [ ! -f cl-ppcre.sbcl ]; then
  echo "PPCRE core not found, attempting to build..."
  sbcl --noinform --no-userinit --eval "(require :asdf)" --load build-ppcre-core.lisp
fi

if [ -z $1 ]; then
  echo "Usage: $0 SOURCEFILE"
  exit 1
fi

rm -f tmp

cat "$1" | ./cl-ppcre.sbcl --no-userinit --load syntax.lisp --noprint --noinform > tmp

### stx2any

stx2any -thtml --table-of-contents on --numbering on tmp > "$1.html"

