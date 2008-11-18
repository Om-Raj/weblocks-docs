#!/bin/sh
cat "$@" | ./cl-ppcre.sbclcore --no-userinit --load syntax.lisp --noprint --noinform
