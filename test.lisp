; sbcl --load test.lisp
; ../single-threaded-ccl/stccl --load test.lisp
; clisp -i test.lisp

(in-package :cl-user)
(require :asdf)
(unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "1.711"))
  (push "/home/fare/cl/asdf/" asdf:*central-registry*)
  (asdf:oos 'asdf:load-op :asdf))

(in-package :asdf)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

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

(asdf:oos 'asdf:load-op :poiu :verbose t)
(asdf:oos 'asdf:load-op :cl-launch :verbose t)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(format *error-output* "~&POIU ~A~%" asdf::*poiu-version*)

(trace asdf:operate asdf::traverse asdf::make-checked-dependency-trees
       ;;asdf::can-run-in-background-p asdf::call-queue/forking ;; asdf::operation-executed-p
       ;; asdf:perform
       ;;asdf::operation-done-p
       asdf::perform-with-restarts)

#|
(defmethod operation-done-p ((o operation) (c component))
  (let ((out-files (output-files o c))
        (in-files (input-files o c))
        (op-time (gethash (type-of o) (component-operation-times c))))
    (DBG :odp o c out-files in-files op-time)
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
         ;; an operation without output-files and no input-files
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
         (DBG :odp2 (mapcar 'probe-file in-files) (mapcar 'probe-file out-files)
              (earliest-out) (latest-in)
         (and
          (every #'probe-file in-files)
          (every #'probe-file out-files)
          (>= (earliest-out) (latest-in))))))))
)|#

(asdf:oos 'asdf:parallel-load-op :exscribe :verbose t)

(exscribe::process-command-line
 '("-I" "/home/fare/fare/www" "-o" "-" "-H" "/home/fare/fare/www/index.scr"))

(cl-launch:quit 0)
