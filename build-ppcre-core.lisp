(with-output-to-string (*standard-output*)
  (push #p"/Users/Nandan/nb/clbuild/systems/" asdf:*central-registry*)
  (require :cl-ppcre))

(save-lisp-and-die "cl-ppcre.sbcl" :executable t)
