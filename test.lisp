":" "-*- Lisp -*-" ; : \
; case "${1:-sbcl}" in (sbcl) : \
; sbcl --load test.lisp
;; (ccl) : \
; ../single-threaded-ccl/stccl --load test.lisp
;; (clisp) : \
; clisp -i ../asdf/asdf.lisp -i test.lisp
;; (*) echo "Unrecognized/unsupported Lisp: $1" ; exit 42
;; esac ; exit

(in-package :cl-user)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(require :asdf)
(asdf:load-system :asdf) ;; reload always, so we have the latest;
;; otherwise, poiu will pull the latest, anyway, and that will cause package trouble
;; if it's different from the current.

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

;(trace operate traverse make-checked-dependency-trees
       ;;can-run-in-background-p call-queue/forking
       ;;perform
       ;;operation-done-p operation-executed-p
;       perform-with-restarts)
;#+clisp (trace posix-wexitstatus posix-wait)

(asdf:parallel-load-system :exscribe :verbose t)

(exscribe::process-command-line
 '("-I" "/home/fare/fare/www" "-o" "-" "-H" "/home/fare/fare/www/index.scr"))

(cl-launch:quit 0)
