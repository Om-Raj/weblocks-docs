#+clisp(load "/home/sky/projects/lisp/asdf.lisp")
(push "/home/sky/.sbcl/systems/" asdf:*central-registry*)
(setf asdf::*verbose-out* nil)
(asdf:oos 'asdf:load-op 'cl-ppcre)

;(trace read-line)

(defmacro push-end (value place &environment env)
  "Like PUSH, except that the value goes on the end of the PLACE list.  
If PLACE is (), then (value) is returned."
  #+Explorer (declare (ignore env))
  (multiple-value-bind (tmpvars tmpvals storevar storeform refform)
      (get-setf-expansion place #-Explorer env)
    (let ((oldval  (gensym "OLDVAL"))
          (newcons (gensym "NEWCONS")))
    `(let* (,@(mapcar #'list tmpvars tmpvals)
            (,oldval ,refform)
            (,newcons (cons ,value nil)))
       (if ,oldval
           (progn (setf (cdr (last ,oldval)) ,newcons)
                  ,oldval)
           (let ((,@storevar ,newcons))
             ,storeform))))))

(defun filter (&optional (in *standard-input*) (out *standard-output*))
  (loop for line = (read-line in nil)
        while line
        ;do (format t "line: ~S~%" line)
        do (if (cl-ppcre:all-matches "^CODE_BEGIN.*$" line)
              (let* ((newline-string (format nil "~%"))
                     (lines (loop for line = (read-line in)
                                  while line
                                  until (cl-ppcre:all-matches "^CODE_END" line)
                                  collect (concatenate 'string line newline-string)))
                     (lines (apply #'concatenate 'string lines))) ;; FIXME reduce
                (format out "{{{~%")
                (finish-output out)
                (run-program "/usr/bin/pygmentize" '("-lcl" "-fhtml")
                             :input (make-string-input-stream lines)
                             :output out)
                (write "}}}" :stream out :escape nil))
              (progn
                (write-sequence line out)
                (terpri out)))))

(filter *standard-input* *standard-output*)

