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

(trace operate traverse make-checked-dependency-trees
       run-in-background-p
       can-run-in-background-p operation-executed-p operation-done-p
       input-files output-files file-write-date
       component-operation-time mark-operation-done
       call-queue/forking make-communicating-subprocess
       perform perform-with-restarts perform-plan compile-file
       )
;#+clisp (trace posix-wexitstatus posix-wait)

(defmethod operation-done-p ((o operation) (c component))
  (let ((out-files (output-files o c))
        (in-files (input-files o c))
        (op-time (component-operation-time o c)))
    (flet ((earliest-out ()
             (reduce #'min (mapcar #'safe-file-write-date out-files)))
           (latest-in ()
             (reduce #'max (mapcar #'safe-file-write-date in-files))))
      (cond
        ((and (not in-files) (not out-files))
         ;; arbitrary decision: an operation that uses nothing to
         ;; produce nothing probably isn't doing much.
         ;; e.g. operations on systems, modules that have no immediate action,
         ;; but are only meaningful through traversed dependencies
         t)
        ((not out-files)
         ;; an operation without output-files is probably meant
         ;; for its side-effects in the current image,
         ;; assumed to be idem-potent,
         ;; e.g. LOAD-OP or LOAD-SOURCE-OP of some CL-SOURCE-FILE.
         (and op-time (>= op-time (latest-in))))
        ((not in-files)
         ;; an operation with output-files and no input-files
         ;; is probably meant for its side-effects on the file-system,
         ;; assumed to have to be done everytime.
         ;; (I don't think there is any such case in ASDF unless extended)
         nil)
        (t
         ;; an operation with both input and output files is assumed
         ;; as computing the latter from the former,
         ;; assumed to have been done if the latter are all older
         ;; than the former.
         ;; e.g. COMPILE-OP of some CL-SOURCE-FILE.
         ;; We use >= instead of > to play nice with generated files.
         ;; This opens a race condition if an input file is changed
         ;; after the output is created but within the same second
         ;; of filesystem time; but the same race condition exists
         ;; whenever the computation from input to output takes more
         ;; than one second of filesystem time (or just crosses the
         ;; second). So that's cool.
         (let ((earliest-out (earliest-out))
               (latest-in (latest-in)))
         (DBG :odp earliest-out latest-in
              (and earliest-out latest-in (>= earliest-out latest-in)))
         (and
          (every #'probe-file* in-files)
          (every #'probe-file* out-files)
          (>= earliest-out latest-in))))))))

(block nil
  (handler-bind ((error #'(lambda (condition)
                            (format t "~&ERROR:~%~A~%" condition)
                            (print-backtrace *standard-output*)
                            (format t "~&ERROR:~%~A~%" condition)
                            (finish-output)
                            (return))))
    (asdf:parallel-load-system :exscribe :verbose t)
    (funcall (find-symbol "PROCESS-COMMAND-LINE" "EXSCRIBE")
             '("-I" "/home/fare/fare/www" "-o" "-" "-H" "/home/fare/fare/www/index.scr"))))

(format t "~&~S~%" (asdf::implementation-identifier))
(format t "~&Compiled with as many as ~D forked subprocesses~%" *max-actual-forks*)

(cl-launch:quit 0)
