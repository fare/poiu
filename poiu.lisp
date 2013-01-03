;; -*- mode: Lisp ; coding: utf-8 -*-
;;; This is POIU: Parallel Operator on Independent Units
(cl:in-package :asdf)
(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *poiu-version* "1.29.4")
(defparameter *asdf-version-required-by-poiu* "2.26.51"))
#|
POIU is a modification of ASDF that may operate on your systems in parallel.
This version of POIU was designed to work with ASDF no earlier than specified.

POIU will notably compile each Lisp file in its own forked process,
in parallel with other operations (compilation or loading).
However, it will load FASLs serially as they become available.

POIU will only make a difference with respect to ASDF if the dependencies
are not serial (i.e. no difference for systems using :serial t everywhere).
You can however use Andreas Fuchs's ASDF-DEPENDENCY-GROVEL to autodetect
minimal dependencies from an ASDF system (or a set of multiple such).

POIU may speed up compilation by utilizing all CPUs of an SMP machine.
POIU may also reduce the memory pressure on the main (loading) process.
POIU will enforce separation between compile- and load- time environments,
helping you detect when :LOAD-TOPLEVEL is missing in EVAL-WHEN's
(as needed for incremental compilation even with vanilla ASDF).
POIU will also catch *some* missing dependencies as exist between the
files that it will happen to compile in parallel (but may not catch all
dependencies that may otherwise be missing from your system).

When a compilation fails in a parallel process, POIU will retry compiling
in the main (loading) process so you get the usual ASDF error behavior,
with a chance to debug the issue and restart the operation.

POIU was currently only made to work with SBCL, CCL and CLISP.
Porting to another Lisp implementation that supports ASDF
should not be difficult. [Note: the CLISP port is somewhat less stable.]

Warning to CCL users: you need to save a CCL image that doesn't start threads
at startup in order to use POIU (or anything that uses fork).
Watch QITAB for a package that does just that: SINGLE-THREADED-CCL.

To use POIU, (1) make sure asdf.lisp is loaded.
We require a recent enough ASDF 2; see specific requirement above.
Usually, you can
	(require "asdf")
(2) configure ASDF's SOURCE-REGISTRY or its *CENTRAL-REGISTRY*, then load POIU.
	(require "poiu")
might work on SBCL and CCL. On CLISP, you can definitely
	(asdf:load-system :poiu)
(alternatively, you might manually (load "/path/to/poiu"),
but you might as well test your configuration of ASDF).
(3) Actually use POIU, with such commands as
	(asdf:parallel-load-system :your-system)
Once again, you may want to first use asdf-dependency-grovel to minimize
the dependencies in your system.

POIU was initially written by Andreas Fuchs in 2007
as part of an experiment funded by ITA Software, Inc.
It was subsequently modified by Francois-Rene Rideau at ITA Software, who
adapted POIU for use with XCVB in 2009, wrote the CCL and CLISP ports,
moved code from POIU to ASDF, and
rewrote both of them together in a simpler way.
The original copyright and (MIT-style) licence of ASDF (below) applies to POIU:
|#
;;; ASDF is
;;; Copyright (c) 2001-2003 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(declaim (optimize (speed 1) (debug 3) (safety 3)))

;;; Check versions
(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or clisp clozure sbcl)
  (format *error-output* "POIU doesn't support your Lisp implementation (yet). Help port POIU!")
  #-asdf2
  (error "POIU requires ASDF2.")
  #+asdf2
  (unless (asdf:version-satisfies (asdf:asdf-version) *asdf-version-required-by-poiu*)
    (error "POIU ~A requires ASDF ~A or later, you only have ~A loaded."
           *poiu-version*
           *asdf-version-required-by-poiu* (asdf:asdf-version)))
  #+clisp (ignore-errors (eval '(require "linux")))
  #+sbcl (require :sb-posix)
  (export '(parallel-load-op parallel-compile-op
            parallel-load-system parallel-compile-system))
  (pushnew :poiu *features*))

;;; Some general purpose data structures we use
(defgeneric table-values (table))
(defgeneric table-keys (table))
(defgeneric empty-p (collection))
(defgeneric queue-tail (queue))
(defgeneric (setf queue-tail) (new-tail queue))
(defgeneric enqueue (queue value))
(defgeneric enqueue-new (queue value &key test test-not))
(defgeneric enqueue-in-front (queue value))
(defgeneric empty-p (queue))
(defgeneric dequeue (queue))
(defgeneric enqueue-many (queue list))
(defgeneric queue-contents (queue))
(defgeneric dequeue-all (queue))

(defmethod table-values ((table hash-table))
  (loop :for val :being :the :hash-values :of table :collect val))
(defmethod table-keys ((table hash-table))
  (loop :for key :being :the :hash-keys :of table :collect key))
(defmethod empty-p ((table hash-table))
  (zerop (hash-table-count table)))

(defclass simple-queue ()
  ((head :accessor queue-head :initarg :head)))
(defmethod queue-tail ((q simple-queue))
  (car (queue-head q)))
(defmethod (setf queue-tail) (v (q simple-queue))
  (setf (car (queue-head q)) v))
(defun simple-queue (&optional contents)
  (let ((c (cons 0 (copy-list contents))))
    (setf (car c) (last c))
    (make-instance 'simple-queue :head c)))
(defmethod enqueue ((q simple-queue) x)
  (let ((c (list x)))
    (setf (cdr (queue-tail q)) c
          (queue-tail q) c)
    t))
(defmethod enqueue-new ((q simple-queue) x &rest keys &key test test-not)
  (declare (ignore test test-not))
  (unless (apply #'find x (cdr (queue-head q)) keys)
    (enqueue q x)))
(defmethod enqueue-in-front ((q simple-queue) x)
  (if (empty-p q)
      (enqueue q x)
      (push x (cdr (queue-head q))))
  t)
(defmethod empty-p ((q simple-queue))
  (null (cdr (queue-head q))))
(defmethod dequeue ((q simple-queue))
  (when (null (cdr (queue-head q)))
    (error "Trying to dequeue from an empty queue!"))
  (prog1 (pop (cdr (queue-head q)))
    (when (null (cdr (queue-head q)))
      (setf (queue-tail q) (queue-head q)))))
(defmethod enqueue-many ((q simple-queue) list)
  (dolist (x list) (enqueue q x)) (values))
(defmethod queue-contents ((q simple-queue))
  (copy-list (cdr (queue-head q))))
(defmethod dequeue-all ((q simple-queue))
  (prog1 (cdr (queue-head q))
    (setf (queue-tail q) (queue-head q) (cdr (queue-head q)) nil)))
(defun call-with-queue (fun q)
  (loop :until (empty-p q) :do (let ((x (dequeue q))) (funcall fun x))))
(defmacro with-queue ((var qvar &optional (qval '(simple-queue))) &body body)
  `(let ((,qvar ,qval)) (call-with-queue (lambda (,var) ,@body) ,qvar)))

;;; Reifying and reconstituting objects for sending across processes
(defun cl-symbol-p (x)
  (and (symbolp x) (eq (find-package :cl) (symbol-package x))))
(deftype cl-symbol () '(and symbol (satisfies cl-symbol-p)))
(defun reify-symbol (sym)
  (vector (symbol-name sym) (package-name (symbol-package sym))))
(defun reconstitute-symbol (sym)
  (intern (aref sym 0) (aref sym 1)))
(defun reify-simple-sexp (sexp)
  (etypecase sexp
    ((or cl-symbol keyword number character simple-string pathname) sexp)
    (cons (cons (reify-simple-sexp (car sexp)) (reify-simple-sexp (cdr sexp))))
    (symbol (reify-symbol sexp))))
(defun reconstitute-simple-sexp (sexp)
  (etypecase sexp
    ((or cl-symbol keyword number character simple-string pathname) sexp)
    (cons (cons (reconstitute-simple-sexp (car sexp)) (reconstitute-simple-sexp (cdr sexp))))
    ((simple-vector 2) (reconstitute-symbol sexp))))

;;; Extracting undefined-warnings from the compilation-unit
;;; To be passed through the above reify/reconstitute link, it must be a "simple-sexp"
(defun undefined-warning-sexp (warning)
  #-sbcl (declare (ignore warning))
  #+sbcl
  (list*
   (sb-c::undefined-warning-kind warning)
   (sb-c::undefined-warning-name warning)
   (sb-c::undefined-warning-count warning)
   (mapcar
    #'(lambda (frob)
        ;; the lexenv slot can be ignored for reporting purposes
        `(:enclosing-source ,(sb-c::compiler-error-context-enclosing-source frob)
          :source ,(sb-c::compiler-error-context-source frob)
          :original-source ,(sb-c::compiler-error-context-original-source frob)
          :context ,(sb-c::compiler-error-context-context frob)
          :file-name ,(sb-c::compiler-error-context-file-name frob) ; a pathname
          :file-position ,(sb-c::compiler-error-context-file-position frob) ; an integer
          :original-source-path ,(sb-c::compiler-error-context-original-source-path frob)))
    (sb-c::undefined-warning-warnings warning))))

(defun reconstitute-deferred-warnings (constructor-list)
  #-sbcl (declare (ignore constructor-list))
  #+sbcl
  (dolist (item constructor-list)
    ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
    ;; For *undefined-warnings*, the adjustment is a list of initargs.
    ;; For everything else, it's an integer.
    (destructuring-bind (symbol . adjustment) item
      (case symbol
        ((sb-c::*undefined-warnings*)
         (setf sb-c::*undefined-warnings*
               (nconc (mapcan
                       #'(lambda (stuff)
                           (destructuring-bind (kind name count . rest) stuff
                             (if (and (eq kind :function) (fboundp name))
                                 nil
                                 (list
                                  (sb-c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'sb-c::make-compiler-error-context x))
                                           rest))))))
                       adjustment)
                      sb-c::*undefined-warnings*)))
        (otherwise
         (set symbol (+ (symbol-value symbol) adjustment)))))))

(defun get-deferred-warnings ()
  #-sbcl nil
  #+sbcl
  (when sb-c::*in-compilation-unit*
    ;; Try to send nothing through the pipe if nothing needs to be accumulated
    `(,@(when sb-c::*undefined-warnings*
          `((sb-c::*undefined-warnings* . ,(mapcar #'undefined-warning-sexp sb-c::*undefined-warnings*))))
      ,@(loop for what in '(sb-c::*aborted-compilation-unit-count*
                            sb-c::*compiler-error-count*
                            sb-c::*compiler-warning-count*
                            sb-c::*compiler-style-warning-count*
                            sb-c::*compiler-note-count*)
              for value = (symbol-value what)
              when (plusp value)
                collect `(,what . ,value)))))

(defun reset-deferred-warnings ()
  #+sbcl
  (when sb-c::*in-compilation-unit*
    (setf sb-c::*undefined-warnings* nil
          sb-c::*aborted-compilation-unit-count* 0
          sb-c::*compiler-error-count* 0
          sb-c::*compiler-warning-count* 0
          sb-c::*compiler-style-warning-count* 0
          sb-c::*compiler-note-count* 0)))

;;; Toplevel parallel operations
(defclass parallelizable-operation (operation) ())
(defclass parallel-compile-op (parallelizable-operation) ())
(defclass parallel-load-op (parallelizable-operation) ())

(defgeneric unparallelize-operation (operation))
(defmethod unparallelize-operation ((o parallel-load-op)) (find-operation o 'load-op))
(defmethod unparallelize-operation ((o compile-op)) (find-operation o 'compile-op))
(defmethod component-depends-on ((o parallelizable-operation) c)
  `((,(unparallelize-operation o) ,c) ,@(call-next-method)))

(defun parallel-load-system (system &rest args)
  (apply #'operate 'parallel-load-op system args)
  t)
(defun parallel-compile-system (system &rest args)
  (apply #'operate 'parallel-compile-op system args)
  t)

(defclass parallel-plan (plan-traversal)
  ((starting-points :initform (simple-queue) :reader plan-starting-points)
   (children :initform (make-hash-table :test #'equal) :reader plan-children
            :documentation "map an action to a (hash)set of \"children\" that it depends on")
   (parents :initform (make-hash-table :test #'equal) :reader plan-parents
            :documentation "map an action to a (hash)set of \"parents\" that depend on it")
   (all-actions :initform (make-array '(0) :adjustable t :fill-pointer 0) :reader plan-all-actions)
   (ancestor :initarg :ancestor :reader plan-ancestor)))

(defmethod print-object ((plan parallel-plan) stream)
  (print-unreadable-object (plan stream :type t :identity t)
    (with-standard-io-syntax
      (pprint (summarize-plan plan) stream))))

(defmethod plan-operates-on-p ((plan parallel-plan) (component-path list))
  (with-slots (starting-points children) plan
    (let ((component (find-component () component-path)))
      (remove component (append (queue-contents starting-points) (table-keys children)) :key 'cdr :test-not 'eq))))

(defun record-dependency (parent child parents children)
  (unless (gethash child parents)
    (setf (gethash child parents) (make-hash-table :test #'equal)))
  (when parent
    (unless (gethash parent children)
      (setf (gethash parent children) (make-hash-table :test #'equal)))
    (setf (gethash child (gethash parent children)) t)
    (setf (gethash parent (gethash child parents)) t)))

(defun mark-as-done (operation component parents children)
  ;; marks the action of operation on component as done in the deps hash-tables,
  ;; returns a list of new actions that are enabled by it being done.
  (check-type operation operation)
  (let* ((action (cons operation component))
         (action-parents (aif (gethash action parents) (table-keys it)))
         (action-children (aif (gethash action children) (table-keys it))))
    (remhash action parents)
    (assert (null action-children))
    (remhash action children)
    (values
     (loop :for parent :in action-parents
           :for siblings = (gethash parent children)
           :do (assert siblings)
               (remhash action siblings)
           :when (empty-p siblings)
             :do (remhash parent children)
             :and :collect parent)
     (loop :for child :in action-children
           :for siblings = (gethash child parents)
           :do (assert siblings)
               (remhash action siblings)
           :when (empty-p siblings)
             :do (remhash child parents)
             :and :collect child))))

(defmethod plan-record-dependency ((plan parallel-plan) (o operation) (c component))
  (with-slots (children parents visiting-action-list) plan
    (let ((action (cons o c))
          (parent (first visiting-action-list)))
      (record-dependency parent action parents children))))

(defmethod (setf plan-action-status) :after
    (new-status (p parallel-plan) (o operation) (c component))
  (when (action-planned-p new-status)
    (let ((action (cons o c)))
      (vector-push-extend action (plan-all-actions p))
      (unless (gethash action (plan-children p))
        (enqueue (plan-starting-points p) action)))))

(defun make-parallel-plan (operation component &key)
  (let ((plan (make-instance 'parallel-plan :ancestor operation)))
    (traverse-action plan operation component t)
    plan))

(defun summarize-plan (plan)
  (with-slots (starting-points children ancestor) plan
    `((:starting-points
       ,(loop :for (o . c) :in (queue-contents starting-points)
              :collect (cons (type-of o) (component-find-path c))))
      (:dependencies
       ,(flet ((sexpify (action)
                 (destructuring-bind (o . c) action
                   (cons (type-of o) (component-find-path c)))))
          (mapcar #'rest
                  (sort
                   (loop :for parent :being :the :hash-keys :in children
                         :using (:hash-value progeny)
                         :for (o . c) = parent
                         :collect `(,(action-index (plan-action-status plan o c))
                                    ,(sexpify parent)
                                    ,(if (action-already-done-p plan o c) :- :+)
                                    ,@(loop :for child :being :the :hash-keys :in progeny
                                            :using (:hash-value v)
                                            :when v :collect (sexpify child))))
                   #'< :key #'first)))))))

(defgeneric serialize-plan (plan))
(defmethod serialize-plan ((plan list)) plan)
(defmethod serialize-plan ((plan parallel-plan))
  (with-slots ((a ancestor) all-actions visited-nodes) plan
    (loop :for action :in (reverse (coerce all-actions 'list))
          :for (o . c) = action
          :for (nil done-p nil) = (gethash action visited-nodes)
          :unless done-p :collect action)))

(defgeneric check-invariants (object))

(defmethod check-invariants ((plan parallel-plan))
  ;; This destructively checks that the dependency tree model is coherent.
  (while-collecting (collect)
    (with-slots (starting-points parents children ancestor) plan
      (with-queue (action action-queue starting-points)
        (collect action)
        (destructuring-bind (operation . component) action
          (enqueue-many action-queue
                        (mark-as-done operation component parents children))))
      (unless (empty-p children)
        (error "Cycle detected in the dependency graph:~%~S"
               plan)))))

(defun make-checked-parallel-plan (operation module)
  (check-invariants (make-parallel-plan operation module)) ;; do it once, destructively check it
  (make-parallel-plan operation module)) ;; do it again.

(defmethod traverse ((operation parallelizable-operation) system)
  (make-checked-parallel-plan operation system))

;;; subprocesses: abstraction for the implementation-dependent low-level API

(defun finish-outputs (&rest streams)
  ;; This is notably necessary for CCL, that buffers output
  (map () 'finish-output
       (list* *standard-output* *error-output* *trace-output*
              *terminal-io* *debug-io* *query-io* streams))
  (values))

(defun disable-other-waiters ()
  ;; KLUDGE: Try to undo problems caused by run-program.
  ;; There will still be a race condition if some action calls run-program at load-time.
  ;; But this work-around makes it is safe to call run-program before to invoke poiu
  ;; (it is of course safe after). The true fix to allow run-program to be invoked
  ;; at load-time would be to have an API for a process-waiting callbacks.
  #+sbcl
  (sb-sys:default-interrupt sb-unix:sigchld)) ; ignore-interrupt is undefined for SIGCHLD.

(defparameter *max-forks* 16) ; limit how parallel we will try to be.
(defparameter *max-actual-forks* nil) ; record how parallel we actually went.

#+sbcl
(progn
(defun posix-exit (code)
  (sb-posix:exit code))
;; Simple heuristic: if we have allocated more than the given ratio
;; of what is allowed between GCs, then trigger the GC.
;; Note: can possibly modify parameters and reset in sb-ext:*after-gc-hooks*
(defparameter *prefork-allocation-reserve-ratio* .80) ; default ratio: 80%
(defun can-fork-p ()
  (null (cdr (sb-thread:list-all-threads))))
(defun should-i-gc-p ()
  (let ((available-bytes (- (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long)
                            (sb-kernel:dynamic-usage)))
        (allocation-threshhold (sb-ext:bytes-consed-between-gcs)))
    (< available-bytes (* *prefork-allocation-reserve-ratio* allocation-threshhold))))
(defun posix-fork ()
  (unless (can-fork-p)
    (error "Cannot fork: more than one active thread."))
  (when (should-i-gc-p)
    (sb-ext:gc))
  (sb-posix:fork))
(defun posix-setpgrp ()
  (sb-posix:setpgrp))
(defun posix-wait ()
  (sb-posix:wait))
(defun posix-wexitstatus (x)
  (sb-posix:wexitstatus x))
#|
(defun posix-close (x)
  (sb-posix:close x))
(defun posix-pipe ()
  (sb-posix:pipe))
(defun make-output-stream (fd)
  (sb-sys:make-fd-stream fd :output t))
(defun make-input-stream (fd)
  (sb-sys:make-fd-stream fd :input t))
|#
);sbcl

#+clozure
(progn
(defun can-fork-p ()
  (null (cdr (ccl::all-processes))))
(defun posix-exit (n)
  (ccl:quit n))
(defun posix-fork ()
  (unless (null (cdr (ccl:all-processes)))
    (error "Cannot fork: more than one active thread. Are you using single-threaded-ccl?"))
  (ccl:external-call "fork" :int))
(defun posix-setpgrp ()
  (ccl::external-call "setpgrp" :int))
(defun posix-wait ()
  (ccl::rlet ((status :signed))
    (let* ((retval (ccl::external-call "wait" :address status :signed)))
      (values retval (ccl::pref status :signed)))))
(defun posix-wexitstatus (x)
  (ccl::wexitstatus x))
#|
(defun posix-close (x)
  (ccl::fd-close x))
(defun posix-pipe ()
  (ccl::pipe))
(defun make-output-stream (fd)
  (ccl::make-fd-stream fd :direction :output))
(defun make-input-stream (fd)
  (ccl::make-fd-stream fd :direction :input))
|#
);clozure

#+clisp ;;; CLISP specific fork support
(progn
(defun can-fork-p ()
  (and (find-symbol* 'wait "LINUX") (find-symbol* 'fork "LINUX") t))
(defun posix-exit (n)
  (ext:quit n))
(defun posix-fork ()
  (funcall (find-symbol* 'fork "LINUX")))
(defun posix-setpgrp ()
  (aif (find-symbol* 'setprg 'posix) (funcall it)))
(defun no-child-process-condition-p (c)
  (and (typep c 'system::simple-os-error)
       (equal (simple-condition-format-control c)
                  "UNIX error ~S (ECHILD): No child processes
")))
(defun posix-wait ()
  (handler-case
      (multiple-value-bind (pid status code) (funcall (find-symbol* 'wait "LINUX"))
        (values (and pid (not (= pid -1))) (list pid status code)))
    ((and system::simple-os-error (satisfies no-child-process-condition-p)) ()
      (values nil nil))))
(defun posix-wexitstatus (x)
  (if (eq :exited (second x))
    (third x)
    (cons (second x) (third x))))
#|
(defun posix-close (x)
  (LINUX:close x))
(defun posix-pipe ()
  (multiple-value-bind (code p) (LINUX:pipe)
    (unless (zerop code)
      (error "couldn't make pipes"))
    (values (aref p 0) (aref p 1))))
(defun make-output-stream (fd)
  (ext:make-stream fd :direction :output))
(defun make-input-stream (fd)
  (ext:make-stream fd :direction :input))
|#
);clisp

#-(or sbcl ccl clisp)
(progn
(defun can-fork-p () nil)
(defun posix-exit (n) nil)
(defun posix-fork () nil)
(defun posix-setpgrp () nil)
(defun posix-wait () (values nil nil))
(defun posix-wexitstatus (x) x)
);unsupported implemenetations

;;; Timing the build process

(defvar *time-spent-waiting* 0)

(defmacro timed-do ((time-accumulator) &body body)
  (let ((time-before-thing (gensym)))
    `(let ((,time-before-thing (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
              (incf ,time-accumulator (- (get-internal-real-time)
                                         ,time-before-thing))))))

;;; Handling multiple processes: high-level API

(defclass background-process ()
  ((pid :initarg :pid :accessor process-pid)
   (data :initarg :data :accessor process-data)
   (cleanup :initarg :cleanup :accessor process-cleanup)
   ;; We pass results through a file: pipes may cause deadlocks due to full buffers and naive event loop.
   (result-file :initarg :result-file :accessor process-result-file)))

(define-condition process-failed (error)
  ((exit-status :initarg :exit-status)
   (condition :initform nil :initarg :condition)))

(defun process-return (result-file result condition)
  (with-open-file (s result-file
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*package* (find-package :cl))
            (*read-eval* nil)
            (*print-readably* nil))
        (write (reify-simple-sexp
                `(:process-done
                  ,@(when result `(:result ,result))
                  ,@(when condition `(:condition ,(princ-to-string condition)))))
               :stream s)))))

(defun process-result (process status)
  (block nil
    (when status
      (let ((exit-status (posix-wexitstatus status)))
        (unless (zerop exit-status)
          (return (values nil (make-condition 'process-failed :exit-status exit-status))))))
    (multiple-value-bind (form condition)
        (ignore-errors
         (with-open-file (s (process-result-file process)
                            :direction :input :if-does-not-exist :error)
           (with-standard-io-syntax
             (let ((*package* (find-package :cl))
                   (*read-eval* nil)
                   (*print-readably* nil))
               (reconstitute-simple-sexp (read s))))))
      (when condition
        (return (values nil (make-condition 'process-failed :condition "Could not read result file"))))
      (unless (and (consp form) (eq (car form) :process-done))
        (return (values nil (make-condition 'process-failed :condition "Invalid result file"))))
      (destructuring-bind (&key result condition) (cdr form)
        (return (values result (when condition (make-condition 'process-failed :condition condition))))))))

(defun make-background-process (data function cleanup result-file)
  (disable-other-waiters)
  (finish-outputs)
  (let ((pid (posix-fork)))
    (cond
      ((zerop pid) ; in the child
       ;; don't receive the parent's SIGINTs
       (posix-setpgrp)
       #+sbcl
       (progn
         (sb-ext:disable-debugger)
         (when (find-package :sb-sprof)
           (funcall (intern "STOP-PROFILING" :sb-sprof))))
       #+clozure (setf ccl::*batch-flag* t)
       (reset-deferred-warnings)
       (unwind-protect
            (multiple-value-bind (result condition)
                (ignore-errors (values (funcall function data t)))
              (process-return result-file result condition))
         (finish-outputs)
         (posix-exit 0)))
      (t ; in the parent
       (make-instance 'background-process
                      :pid pid
                      :result-file result-file
                      :cleanup cleanup
                      :data data)))))

(defun call-queue/forking (fun queue
			   &key announce cleanup result-file (background-p (constantly t)))
  ;; assumes a single-threaded parent process
  (declare (optimize debug))
  (let ((processes (make-hash-table :test 'equal)))
    (loop
      (cond
        (;; nothing to do or wait for anymore.
         (and (empty-p queue) (empty-p processes))
         (return))
        (;; we've exceeded the subprocess limit. Wait for a few before continuing.
         (or (>= (hash-table-count processes) *max-forks*)
             (empty-p queue))
         (disable-other-waiters)
         (multiple-value-bind (pid status)
             (timed-do (*time-spent-waiting*) (posix-wait))
           (flet ((cleanup (process status)
                    (multiple-value-bind (result condition)
                        (process-result process status)
                      (funcall (process-cleanup process) (process-data process) result condition t))))
             (if pid
                 (let ((process (gethash pid processes)))
                   (assert process () "couln't find the pid ~A in processes ~S" pid (table-values processes))
                   (remhash pid processes)
                   (cleanup process status))
                 ;; clisp can currently drop signals and get a ENOCHILD...
                 (let ((missed (table-values processes)))
                   (warn "No child left: we must have dropped a signal!")
                       ;;;(warn "blah ~S" entries) ;XXX
                   (clrhash processes)
                   (dolist (process missed)
                     (cleanup process nil)))))))
        (t ;; dequeue an item
         (let* ((item (dequeue queue))
                (backgroundp (funcall background-p item)))
           (funcall announce item backgroundp)
           (cond
             (backgroundp
              (latest-stamp-f *max-actual-forks* (hash-table-count processes))
              (let ((process (make-background-process item fun cleanup (funcall result-file item))))
                (setf (gethash (process-pid process) processes) process)))
             (t
              (multiple-value-bind (result condition)
                  (ignore-errors (values (funcall fun item nil)))
                (funcall cleanup item result condition nil))))))))
    (assert (and (empty-p queue) (empty-p processes)) ()
            "List of processes or list of things to do isn't empty: ~S / ~S~%"
            (queue-contents queue)
            (table-values processes))))

(defmacro doqueue/forking ((queue &key variables
                                    (background-p t) (announce nil) (cleanup nil) result-file)
                           &body body)
  (destructuring-bind (&key item (backgroundp (gensym "BACKGROUNDP")) result condition) variables
    `(call-queue/forking
      #'(lambda (,item ,backgroundp) (declare (ignorable ,item ,backgroundp)) ,@body)
      ,queue
      :result-file #'(lambda (,item) (declare (ignorable ,item)) ,result-file)
      :background-p #'(lambda (,item) (declare (ignorable ,item)) ,background-p)
      :announce #'(lambda (,item ,backgroundp) (declare (ignorable ,item ,backgroundp)) ,announce)
      :cleanup #'(lambda (,item ,result ,condition ,backgroundp)
                   (declare (ignorable ,item ,result ,condition ,backgroundp)) ,cleanup))))

#|
;;; Vague attempt at doing things with threads.
;;; BUT, compilation takes a global lock in CCL, so it's no go.

(defclass communicating-thread ()
  ((thread :initarg :thread :accessor process-thread)
   (data :initarg :data :accessor process-data)
   (cleanup :initarg :cleanup :accessor process-cleanup)
   (lock :initform (ccl:make-lock) :accessor process-lock)
   (status :initform () :accessor process-status)))

#+clozure
(defparameter *null-stream*
  (open "/dev/null" :direction :io :if-does-not-exist :error :if-exists :append))

#+clozure
(defun make-communicating-thread (semaphore data continuation cleanup)
  (let* ((proc (make-instance 'communicating-thread
                 :cleanup cleanup
                 :data data))
         (thread (ccl::process-run-function
                  "worker"
                  (lambda ()
                    (handler-case
                        (let ((*standard-input* *null-stream*))
                          (catch :process-return
                            (funcall continuation data)))
                      (t (c)
                        (declare (ignore c))
                        (ccl::with-lock-grabbed ((process-lock proc))
                          (setf (process-status proc) '(1))))
                      (:no-error (&rest r)
                        (ccl::with-lock-grabbed ((process-lock proc))
                          (setf (process-status proc) (cons 0 r)))))
                    (ccl::signal-semaphore semaphore)))))
    (setf (process-thread proc) thread)
    proc))

#+clozure
(defun process-complete-p (proc)
  (ccl::with-lock-grabbed ((process-lock proc))
    (process-status proc)))

#+clozure
(defun thread-result (proc)
  (second (process-status proc)))

#+clozure
(defun call-queue/threading (thunk queue &key cleanup (background-p (constantly t)))
  ;; will use threads instead of fork
  (declare (optimize debug))
  (let ((elem nil)
        (processes (make-hash-table :test 'equal))
        (pending (ccl:make-semaphore)))
    (loop
      (cond (;; nothing to do or wait for anymore.
             (and (empty-p queue) (empty-p processes))
             (return))
            (;; we've exceeded the subprocess limit. Wait for a few before continuing.
             (or (>= (hash-table-count processes) *max-forks*)
                 (empty-p queue))
             (timed-do (*time-spent-waiting*) (ccl::wait-on-semaphore pending))
             (let ((entry (loop :for process :being :the :hash-values :of processes
                                :thereis (when (process-complete-p process) process))))
               (assert entry () "couln't find a completed process in ~S" processes)
               (remhash (process-thread process) processes)
               (funcall (process-cleanup entry) (process-data entry) (thread-result entry)))))
      (unless (empty-p queue)
        (setf elem (dequeue queue))
        (cond
          ((funcall background-p elem)
           (latest-stamp-f *max-actual-forks* (hash-table-count processes))
           (let ((thread (make-communicating-thread pending elem thunk cleanup)))
             (setf (gethash thread processes) thread)))
          (t
           (unwind-protect (funcall thunk elem)
             (funcall cleanup elem *default-process-result*))))))
    (assert (and (empty-p queue) (empty-p processes)) ()
            "List of processes or list of things to do isn't empty: (~S...)/~S~%"
            (queue-contents queue)
            (table-values processes))
  nil)
|#

;;; Performing a parallel plan
(defun action-result-file (o c)
  (let ((p (component-pathname c)))
    (apply-output-translations
     (make-pathname :name (format nil "~A.ASDF-~A" (file-namestring p) (type-of o))
                    :type "process-result" :defaults p))))

(defmethod perform-plan ((plan parallel-plan) &key)
  (unless (can-fork-p)
    (warn #+(or clozure sbcl) "You are running threads, so it is not safe to fork. Running your build serially."
          #-(or clozure sbcl) "Your implementation cannot fork. Running your build serially.")
    (return-from perform-plan (perform-plan (serialize-plan plan))))
  (with-slots ((action-queue starting-points) children parents ancestor planned-output-action-count) plan
    (let ((all-deferred-warnings nil)
          (ltogo (unless (zerop planned-output-action-count) (ceiling (log planned-output-action-count 10))))
          (*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit ()
        (doqueue/forking
            (action-queue ;; variable for each action, queue object
             :variables (:item action :backgroundp backgroundp :result result :condition condition)
             :background-p (destructuring-bind (o . c) action
                             (not (or (needed-in-image-p o c)
                                      (action-already-done-p plan o c))))
             :announce
             (destructuring-bind (o . c) action
               (format t "~&Will ~:[try~;skip~] ~A in ~:[foreground~;background~]~%"
                       (action-already-done-p plan o c) (operation-description o c) backgroundp))
             :result-file
             (destructuring-bind (o . c) action (action-result-file o c))
             ;; How we cleanup in the foreground after an action is run
             :cleanup
             (destructuring-bind (o . c) action
               (cond
                 (condition
                  (finish-outputs)
                  (warn "Failed ~A~:[~; in the background~]. Retrying~:*~:[~; in the foreground~]."
                        (operation-description o c) backgroundp)
                  (finish-outputs)
                  (perform-with-restarts o c))
                 (t
                  (mark-operation-done o c)
                  (destructuring-bind (&key deferred-warnings &allow-other-keys) result
                    (when deferred-warnings
                      (push deferred-warnings all-deferred-warnings)))))
               (when backgroundp
                 (decf planned-output-action-count)
                 (format t "~&[~vd to go] Done ~A~%"
                         ltogo planned-output-action-count (operation-description o c))
                 (finish-outputs))
               (loop :for enabled-action :in (mark-as-done o c parents children)
                     :for (e-o . e-c) = enabled-action
                     :do (if (needed-in-image-p e-o e-c)
                             (enqueue action-queue enabled-action)
                             (enqueue-in-front action-queue enabled-action)))))
          ;; What we do in each forked process
          (destructuring-bind (o . c) action
            (cond
              (backgroundp
               (perform o c)
               `(:deferred-warnings ,(get-deferred-warnings)))
              ((action-already-done-p plan o c)
               nil)
              (t
               (perform-with-restarts o c)
               nil))))
        (mapc #'reconstitute-deferred-warnings all-deferred-warnings)
        (assert (and (empty-p action-queue) (empty-p children))
                (parents children)
                "Problem with the dependency graph: ~A"
                (summarize-plan plan))))))

;;; Breadcrumbs: feature to replay otherwise non-deterministic builds
(defvar *breadcrumb-stream* nil
  "Stream that records the trail of operations on components.
As the order of ASDF operations in general and parallel operations in
particular are randomized, it is necessary to record them to replay &
debug them later.")
(defvar *breadcrumbs* nil
  "Actual breadcrumbs found, to override traversal for replay and debugging")

(defmethod perform :after (operation component)
  "Record the operations and components in a stream of breadcrumbs."
  (when *breadcrumb-stream*
    (format *breadcrumb-stream* "~S~%" `(,(type-of operation) . ,(component-find-path component)))
    (force-output *breadcrumb-stream*)))

(defun read-breadcrumbs-from (operation pathname)
  (with-open-file (f pathname)
    (loop :for (op . comp) = (read f nil nil) :while op
          :collect (cons (find-operation operation op) (find-component () comp)))))

(defun call-recording-breadcrumbs (pathname record-p thunk)
  (if (and record-p (not *breadcrumb-stream*))
      (let ((*breadcrumb-stream*
              (progn
                (delete-file-if-exists pathname)
                (open pathname :direction :output
                               :if-exists :overwrite
                               :if-does-not-exist :create))))
        (format *breadcrumb-stream* ";; Breadcrumbs~%")
        (unwind-protect
             (funcall thunk)
          (close *breadcrumb-stream*)))
      (funcall thunk)))

(defmacro recording-breadcrumbs ((pathname record-p) &body body)
  `(call-recording-breadcrumbs ,pathname ,record-p (lambda () ,@body)))

(defmethod operate :around (operation system &key
                            (breadcrumbs-to nil record-breadcrumbs-p)
                            ((:using-breadcrumbs-from breadcrumb-input-pathname)
                             (make-broadcast-stream) read-breadcrumbs-p)
                            &allow-other-keys)
  (declare (ignorable system))
  (recording-breadcrumbs (breadcrumbs-to record-breadcrumbs-p)
    (when read-breadcrumbs-p
      (perform-plan (read-breadcrumbs-from operation breadcrumb-input-pathname)))
    (call-next-method)))
