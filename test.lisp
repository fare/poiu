":" ; : "-*- Lisp -*-" \
; case "${1:-sbcl}" in (sbcl) : \
; sbcl --load test.lisp
;; (ccl) : \
; ../single-threaded-ccl/stccl --load test.lisp
;; (clisp) : \
; clisp -i ../asdf/asdf.lisp -i test.lisp
;; (*) echo "Unrecognized/unsupported Lisp: $1" ; exit 42
;; esac 2>&1 | tee foo ; exit

(in-package :cl-user)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(require "asdf")

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(in-package :asdf)

(defmacro dbg (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of source expressions and their resulting values, returns the last values"
  (let ((res (gensym))(f (gensym)))
  `(let ((,res))
    (flet ((,f (fmt &rest args) (apply #'format *trace-output* fmt args)))
      (,f "~&~A~%" ,tag)
      ,@(mapcan
         #'(lambda (x)
            `((,f "~&  ~S => " ',x)
              (,f "~{~S~^ ~}~%" (setf ,res (multiple-value-list ,x)))))
         exprs)
      (apply 'values ,res)))))

(load-system :poiu :verbose t)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(format *error-output* "~&POIU ~A~%" *poiu-version*)

(defun print-backtrace (out)
  "Print a backtrace (implementation-defined)"
  (declare (ignorable out))
  #+clozure (let ((*debug-io* out))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output out))
  #+sbcl
  (sb-debug:backtrace most-positive-fixnum out))

#+(or)
(trace operate traverse make-checked-dependency-trees
       run-in-background-p
       can-run-in-background-p operation-executed-p operation-done-p
       input-files output-files file-write-date
       component-operation-time mark-operation-done
       call-queue/forking make-communicating-subprocess
       perform perform-with-restarts perform-plan compile-file)
;;#+clisp (trace posix-wexitstatus posix-wait)

(defvar *fare* (asdf::user-homedir))
(defun subnamestring (base sub)
  (namestring (asdf::subpathname base sub)))

(block nil
  (handler-bind ((error #'(lambda (condition)
                            (format t "~&ERROR:~%~A~%" condition)
                            (print-backtrace *standard-output*)
                            (format t "~&ERROR:~%~A~%" condition)
                            (finish-output)
                            (return))))
    (asdf:parallel-load-system :exscribe :verbose t :force t)
    (funcall (find-symbol "PROCESS-COMMAND-LINE" "EXSCRIBE")
             `("-I" ,(subnamestring *fare* "fare/www/")
               "-o" "-" "-H" ,(subnamestring *fare* "fare/www/index.scr")))))

(format t "~&~S~%" (asdf::implementation-identifier))
(format t "~&Compiled with as many as ~D forked subprocesses~%" *max-actual-forks*)

(asdf::posix-exit 0)
