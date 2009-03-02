#+clisp(load "/home/sky/projects/lisp/asdf.lisp")
(push "/home/sky/.sbcl/systems/" asdf:*central-registry*)
(setf asdf::*verbose-out* nil)
(asdf:oos 'asdf:load-op 'cl-ppcre)

;(trace read-line)

(defun filter (&optional (in *standard-input*) (out *standard-output*))
  (loop for line = (read-line in nil)
        while line
        ;do (format t "line: ~S~%" line)
        do (if (cl-ppcre:all-matches "^CODE_BEGIN.*$" line)
              (let* ((lines (loop for line = (read-line in)
                                  while line
                                  until (cl-ppcre:all-matches "^CODE_END" line)
                                  collect (append (coerce line 'list) '(#\Newline))))
                     (lines (apply #'concatenate 'string lines))) ;; FIXME reduce
                (run-program "/usr/bin/pygmentize" '("-lcl" "-fhtml")
                             :input (make-string-input-stream lines)
                             :output out))
            (progn
              (write-sequence line out)
              (terpri out)))))

(filter *standard-input* *standard-output*)
