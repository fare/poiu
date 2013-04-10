":" ; : "-*- Lisp -*-" \
; case "${1:-sbcl}" in (sbcl) sbcl --load test.lisp \
;; (allegro) alisp -L test.lisp \
;; (ccl) ../single-threaded-ccl/stccl --load test.lisp \
;; (clisp) clisp -i test.lisp \
;; (*) echo "Unrecognized/unsupported Lisp: $1" ; exit 42
;; esac 2>&1 | tee foo ; exit

;;; TODO: have a deterministic variant that maximizes in-image then out-image actions.
;;; i.e. maintain two queues, for in-image and out-image; exhaust one, then the other.

(in-package :cl-user)

(setf *load-verbose* nil
      *load-print* nil
      *compile-verbose* nil
      *compile-print* nil)

(ignore-errors (funcall 'require "asdf"))
#-asdf2 (load "../asdf/build/asdf.lisp")

(asdf:load-system :asdf)

(in-package :asdf) ;; in case there was a punt, be in the NEW asdf package.

#+clisp (trace asdf::read-file-form asdf::read-file-forms)

(pushnew :DBG *features*)
(defmacro DBG (tag &rest exprs)
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
      *compile-print* t
      *asdf-verbose* t)

(format *error-output* "~&POIU ~A~%" *poiu-version*)

#+(or)
(trace
 traverse ;; traverse-component
 make-parallel-plan
 ;; mark-as-done
 ;; process-return process-result ;; action-result-file
 ;; input-files output-files file-write-date
 ;; component-operation-time mark-operation-done
 ;; call-queue/forking make-communicating-subprocess
 ;; perform perform-with-restarts
 ;; compile-file load
 operate call-recording-breadcrumbs perform-plan
)
#+allegro (trace posix-fork posix-wexitstatus posix-wait excl::getpid quit)

(defvar *fare* (asdf/common-lisp:user-homedir-pathname))
(defun subnamestring (base sub)
  (namestring (asdf/driver:subpathname base sub)))

(block nil
  (handler-bind ((error #'(lambda (condition)
                            (format t "~&ERROR:~%~A~%" condition)
                            (print-backtrace :stream *standard-output*)
                            (format t "~&ERROR:~%~A~%" condition)
                            (finish-output)
                            (return))))
    (asdf:parallel-load-system
     :exscribe :verbose t
     :force :all
     :breadcrumbs-to "/tmp/breadcrumbs.text")
    (funcall (asdf/package:find-symbol* :process-command-line :exscribe)
             `("-I" ,(subnamestring *fare* "fare/www/")
               "-o" "-" "-H" ,(subnamestring *fare* "fare/www/index.scr")))))

(format t "~&~S~%" (asdf/os:implementation-identifier))
(format t "~&Compiled with as many as ~D forked subprocesses~%" *max-actual-forks*)

(quit 0)
