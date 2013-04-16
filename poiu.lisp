;; -*- mode: Lisp ; coding: utf-8 -*-
;;; This is POIU: Parallel Operator on Independent Units
#+xcvb (module (:depends-on ("asdf")))
(in-package :asdf)
(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *poiu-version* "1.30.4")
(defparameter *asdf-version-required-by-poiu* "2.32"))
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
as needed for incremental compilation even with vanilla ASDF.
POIU will also catch *some* missing dependencies as exist between the
files that it will happen to compile in parallel (but may not catch all
dependencies that may otherwise be missing from your system).

When a compilation fails in a parallel process, POIU will retry compiling
in the main (loading) process so you get the usual ASDF error behavior,
with a chance to debug the issue and restart the operation.

POIU was currently only made to work with SBCL, CCL and CLISP.
Porting to another Lisp implementation that supports ASDF
should not be difficult. [Note: the CLISP port is somewhat less stable.]
When unable to fork because the implementation is unsupported,
or because multiple threads are currently in use,
POIU will fall back to compiling everything in the main process.

Warning to CCL users: you need to save a CCL image that doesn't start threads
at startup in order to use POIU (or anything that uses fork).
Watch QITAB for a package that does just that: SINGLE-THREADED-CCL.

To use POIU, (1) make sure asdf.lisp is loaded.
We require a recent enough ASDF 3; see specific requirement above.
Usually, you can
	(require "asdf")
to load ASDF 2, then
	(asdf:load-system "asdf")
to upgrade to ASDF 3.
(2) configure ASDF's SOURCE-REGISTRY or its *CENTRAL-REGISTRY*, then load POIU.
	(require "poiu")
might work on SBCL and CCL. On CLISP, you can definitely
	(asdf:load-system :poiu)
(alternatively, you might manually (load "/path/to/poiu"),
but you might as well test your configuration of ASDF).
(3) POIU is active by default. You can just
	(asdf:load-system :your-system)
and POIU will be used to compile it.
Once again, you may want to first use asdf-dependency-grovel to minimize
the dependencies in your system.

POIU was initially written by Andreas Fuchs in 2007
as part of an experiment funded by ITA Software, Inc.
It was subsequently modified by Francois-Rene Rideau at ITA Software, who
adapted POIU for use with XCVB in 2009, wrote the CCL and CLISP ports,
moved code from POIU to ASDF, and
eventually rewrote both of them together in a simpler way.
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
  #-(and unix (or allegro clisp clozure sbcl))
  (warn "POIU doesn't support forking on your Lisp implementation (yet). Help port POIU!")
  (unless (or #+asdf3 (version<= *asdf-version-required-by-poiu* (asdf:asdf-version)))
    (error "POIU ~A requires ASDF ~A or later, but you only have ~A loaded."
           *poiu-version*
           *asdf-version-required-by-poiu* (asdf:asdf-version)))
  #+(and unix clisp) (ignore-errors (funcall 'require "linux"))
  #+(and unix sbcl) (require :sb-posix)
  (export '(parallel-load-system parallel-compile-system))
  (pushnew :poiu *features*))

;;; Some general purpose data structures we use
(defgeneric empty-p (collection))
(defgeneric size (collection))

(defgeneric table-values (table))
(defgeneric table-keys (table))

(defgeneric queue-tail (queue))
(defgeneric (setf queue-tail) (new-tail queue))
(defgeneric enqueue (queue value))
(defgeneric enqueue-new (queue value &key test test-not))
(defgeneric enqueue-in-front (queue value))
(defgeneric dequeue (queue))
(defgeneric enqueue-many (queue list))
(defgeneric queue-contents (queue))
(defgeneric dequeue-all (queue))

(defmethod empty-p ((x null))
  (declare (ignorable x))
  t)

(defmethod table-values ((table hash-table))
  (loop :for val :being :the :hash-values :of table :collect val))
(defmethod table-keys ((table hash-table))
  (loop :for key :being :the :hash-keys :of table :collect key))
(defmethod size ((table hash-table))
  (hash-table-count table))
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

(defvar *parallel-plan-deterministic-p* t) ;; Use the deterministic build by default.

(defclass parallel-plan (plan-traversal)
  ((starting-points
    :initform (simple-queue) :reader plan-starting-points
    :documentation "a queue of actions with no dependencies")
   (children
    :initform (make-hash-table :test #'equal) :reader plan-children
    :documentation "map an action to a (hash)set of \"children\" that it depends on")
   (parents
    :initform (make-hash-table :test #'equal) :reader plan-parents
    :documentation "map an action to a (hash)set of \"parents\" that depend on it")
   (all-actions
    :initform (make-array '(0) :adjustable t :fill-pointer 0) :reader plan-all-actions)
   (deterministic-p
    :initform *parallel-plan-deterministic-p* :initarg :deterministic-p
    :type boolean :reader plan-deterministic-p
    :documentation "is this plan supposed to be executed in deterministic way?")))

(defun parallel-operate (operation system &rest keys)
  (apply 'operate operation system :plan-class 'parallel-plan keys))
(defun parallel-load-system (system &rest args)
  (apply 'load-system system :plan-class 'parallel-plan args))
(defun parallel-compile-system (system &rest args)
  (apply 'compile-system system :plan-class 'parallel-plan args))
(defun parallel-build-system (system &rest args)
  (apply 'build-system system :plan-class 'parallel-plan args))
(defun parallel-test-system (system &rest args)
  (apply 'test-system system :plan-class 'parallel-plan args))

(defmethod print-object ((plan parallel-plan) stream)
  (print-unreadable-object (plan stream :type t :identity t)
    (with-safe-io-syntax (:package :asdf)
      (format stream "~A" (coerce-name (plan-system plan)))
      #|(pprint (summarize-plan plan) stream)|#)))

(defmethod plan-operates-on-p ((plan parallel-plan) (component-path list))
  (with-slots (starting-points children) plan
    (let ((component (find-component () component-path)))
      (remove component (append (queue-contents starting-points)
                                (mapcar 'node-action (action-map-keys children)))
              :key 'cdr :test-not 'eq))))

(defun action-node (action)
  (destructuring-bind (o . c) action
    (check-type o operation)
    (check-type c component)
    (cons (type-of o) c)))
(defun node-action (node)
  (destructuring-bind (oc . c) node
    (check-type oc symbol)
    (check-type c component)
    (cons (make-operation oc) c)))

(defun make-action-map ()
  (make-hash-table :test 'equal))
(defun action-map (map action)
  (gethash (action-node action) map))
(defun action-unmap (map action)
  (remhash (action-node action) map))
(defun (setf action-map) (value map action)
  (setf (gethash (action-node action) map) value))
(defun action-map-values (map)
  (table-values map))
(defun action-map-keys (map)
  (mapcar 'node-action (table-keys map)))

(defun record-dependency (parent child parents children)
  (unless (action-map parents child)
    (setf (action-map parents child) (make-action-map)))
  (when parent
    (unless (action-map children parent)
      (setf (action-map children parent) (make-action-map)))
    (setf (action-map (action-map children parent) child) t)
    (setf (action-map (action-map parents child) parent) t)))

(defun mark-as-done (plan operation component)
  ;; marks the action of operation on component as done in the deps hash-tables,
  ;; returns a list of new actions that are enabled by it being done.
  (check-type operation operation)
  (with-slots (starting-points parents children) plan
    (let* ((action (cons operation component))
           (action-parents (if-let (it (action-map parents action))
                             (action-map-keys it)))
           (action-children (if-let (it (action-map children action))
                              (action-map-keys it))))
      (action-unmap parents action)
      (action-unmap children action)
      (let ((enabled-parents
              (loop :for parent :in action-parents
                    :for siblings = (action-map children parent)
                    :do (assert siblings)
                        (action-unmap siblings action)
                    :when (empty-p siblings)
                      :do (action-unmap children parent)
                      :and :collect parent))
            (forlorn-children
              (loop :for child :in action-children
                    :for spouses = (action-map parents child)
                    :do (assert spouses)
                        (action-unmap spouses action)
                    :when (empty-p spouses)
                      :do (action-unmap parents child)
                      :and :collect child)))
        (loop :for enabled-action :in enabled-parents
            :for (e-o . e-c) = enabled-action
            :do (if (and (needed-in-image-p e-o e-c) (not (action-already-done-p plan e-o e-c)))
                    (enqueue starting-points enabled-action)
                    (enqueue-in-front starting-points enabled-action)))
        (values enabled-parents forlorn-children)))))

(defmethod plan-record-dependency ((plan parallel-plan) (o operation) (c component))
  (with-slots (children parents visiting-action-list) plan
    (let ((action (cons o c))
          (parent (first visiting-action-list)))
      (record-dependency parent action parents children))))

(defmethod (setf plan-action-status) :before
    (new-status (p parallel-plan) (o operation) (c component))
  (declare (ignorable new-status))
  (unless (gethash (node-for o c) (asdf/plan::plan-visited-actions p)) ; already visited?
    (let ((action (cons o c)))
      (vector-push-extend action (plan-all-actions p))
      (when (empty-p (action-map (plan-children p) action))
        (enqueue (plan-starting-points p) action)))))

(defgeneric* (make-parallel-plan) (operation component &key &allow-other-keys))
(define-convenience-action-methods make-parallel-plan (o c &key))
(defmethod make-parallel-plan ((operation operation) (component component) &rest keys &key &allow-other-keys)
  (let ((plan (apply 'make-instance 'parallel-plan
                     :system (component-system component) keys)))
    (traverse-action plan operation component t)
    plan))

(defun reify-action (action)
  (destructuring-bind (o . c) action
    (check-type o operation)
    (check-type c component)
    (cons (type-of o) (component-find-path c))))

(defun summarize-plan (plan)
  (with-slots (starting-points children) plan
    `((:starting-points
       ,(loop :for action :in (queue-contents starting-points)
              :collect (reify-action action)))
      (:dependencies
       ,(mapcar #'rest
                  (sort
                   (loop :for parent-node :being :the :hash-keys :in children
                         :using (:hash-value progeny)
                         :for parent = (node-action parent-node)
                         :for (o . c) = parent
                         :collect `(,(action-index (plan-action-status plan o c))
                                    ,(reify-action parent)
                                    ,(if (action-already-done-p plan o c) :- :+)
                                    ,@(loop :for child-node :being :the :hash-keys :in progeny
                                            :using (:hash-value v)
                                            :for child = (node-action child-node)
                                            :when v :collect (reify-action child))))
                   #'< :key #'first))))))

(defgeneric serialize-plan (plan))
(defmethod serialize-plan ((plan list)) plan)
(defmethod serialize-plan ((plan parallel-plan))
  (with-slots (all-actions visited-actions) plan
    (loop :for action :in (reverse (coerce all-actions 'list))
          :for (o . c) = action
          :for status = (plan-action-status plan o c)
          :when (action-planned-p status) :collect action)))

(defgeneric check-invariants (object))

(defmethod check-invariants ((plan parallel-plan))
  ;; This destructively checks that the dependency tree model is coherent.
  (while-collecting (collect)
    (with-slots (starting-points parents children) plan
      (with-queue (action action-queue starting-points)
        (collect action)
        (destructuring-bind (operation . component) action
          (mark-as-done plan operation component)))
      (unless (empty-p children)
        (error "Cycle detected in the dependency graph:~%~S"
               plan)))))

(defmethod traverse :before ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
  (when (eq (or plan-class *default-plan-class*) 'parallel-plan)
    ;; make a plan once already and destructively check it
    (check-invariants (apply 'make-parallel-plan o c keys))))

(defmethod plan-actions ((plan parallel-plan))
  plan)

(setf *default-plan-class* 'parallel-plan)

;;; subprocesses: abstraction for the implementation-dependent low-level API

(defun disable-other-waiters ()
  ;; KLUDGE: Try to undo problems caused by run-program.
  ;; There will still be a race condition if some action calls run-program at load-time.
  ;; But this work-around makes it is safe to call run-program before to invoke poiu
  ;; (it is of course safe after). The true fix to allow run-program to be invoked
  ;; at load-time would be to have an API for a process-waiting callbacks.
  #+(and sbcl unix)
  (sb-sys:default-interrupt sb-unix:sigchld)) ; ignore-interrupt is undefined for SIGCHLD.

(defparameter *max-forks* 16) ; limit how parallel we will try to be.
(defparameter *max-actual-forks* nil) ; record how parallel we actually went.

#+(and sbcl unix)
(progn
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
(defconstant +echild+ sb-posix:echild)
(defun posix-waitpid (pid &key nohang untraced)
  (handler-case
      (sb-posix:waitpid
       (or pid -1)
       (logior (if nohang sb-posix:wnohang 0)
               (if untraced sb-posix:wuntraced 0)))
    (sb-posix:syscall-error (c)
      (values -1 (sb-posix:syscall-errno c)))))
(defun posix-wexitstatus (x)
  (sb-posix:wexitstatus x))
#|
(defun posix-close (x)
  (sb-posix:close x))
(defun posix-pipe ()
  (sb-posix:pipe))
(defun fd-output-stream (fd)
  (sb-sys:make-fd-stream fd :output t))
(defun fd-input-stream (fd)
  (sb-sys:make-fd-stream fd :input t))
|#
);sbcl

#+(and clozure unix)
(progn
(defun can-fork-p ()
  (null (cdr (ccl::all-processes))))
(defun posix-fork ()
  (unless (null (cdr (ccl:all-processes)))
    (error "Cannot fork: more than one active thread. Are you using single-threaded-ccl?"))
  (ccl:external-call "fork" :int))
(defun posix-setpgrp ()
  (ccl::external-call "setpgrp" :int))
(defconstant +echild+ #.(read-from-string "#$ECHILD"))
(defun posix-waitpid (pid &key nohang untraced continued)
  (ccl::rlet ((status :signed))
    (let ((retval (ccl::external-call
                   "waitpid"
                   :integer (or pid -1)
                   :address status
                   :integer (logior (if nohang #.(read-from-string "#$WNOHANG") 0)
                                    (if untraced #.(read-from-string "#$WUNTRACED") 0)
                                    (if continued #.(read-from-string "#$WCONTINUED") 0))
                   :signed)))
      (case retval
        (0 (values 0 ()))
        (-1 (values -1 (ccl::%get-errno)))
        (t (values retval (ccl::pref status :signed)))))))
(defun posix-wexitstatus (x)
  (ccl::wexitstatus x))
#|
(defun posix-close (x)
  (ccl::fd-close x))
(defun posix-pipe ()
  (ccl::pipe))
(defun fd-output-stream (fd)
  (ccl::make-fd-stream fd :direction :output))
(defun fd-input-stream (fd)
  (ccl::make-fd-stream fd :direction :input))
|#
);clozure

#+(and clisp unix) ;;; CLISP specific fork support
(progn
(defun can-fork-p ()
  (and (find-symbol* 'wait "POSIX" nil) (find-symbol* 'fork "LINUX" nil) t nil))
(defun posix-fork ()
  (funcall (find-symbol* 'fork "LINUX")))
(defun posix-setpgrp ()
  (if-let (it (find-symbol* 'setprg 'posix nil)) (funcall it)))
(defun no-child-process-condition-p (c)
  (and (typep c 'system::simple-os-error)
       (equal (simple-condition-format-control c)
                  "UNIX error ~S (ECHILD): No child processes
")))
(defconstant +echild+ :echild)
(defun posix-waitpid (pid &key nohang untraced continued)
  (handler-case
      (multiple-value-bind (pid status code)
          (symbol-call "POSIX" 'wait :pid pid :nohang nohang :untraced untraced :continued continued)
        (case pid
          (0 (values 0 ()))
          (-1 (values -1 :error))
          (t (values pid (list pid status code)))))
    ((and system::simple-os-error (satisfies no-child-process-condition-p)) ()
      (values -1 +echild+))))
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
(defun fd-output-stream (fd)
  (ext:make-stream fd :direction :output))
(defun fd-input-stream (fd)
  (ext:make-stream fd :direction :input))
|#
);clisp

#+(and allegro unix) ;;; Allegro specific fork support
(progn
(defun can-fork-p ()
  (null (cdr mp:*all-processes*)))
(defun posix-fork ()
  (excl.osi:fork))
(defun posix-setpgrp ()
  (excl.osi:setpgrp))
(defconstant +echild+ excl::*echild*)
(defun posix-waitpid (pid &key nohang)
  (multiple-value-bind (exit-status pid signal)
      (sys:reap-os-subprocess :pid (or pid -1) :wait (not nohang))
    (etypecase exit-status
      (null (if nohang (values 0 ()) (values -1 +echild+)))
      (integer (values pid (list exit-status signal))))))
(defun posix-wexitstatus (x)
  (first x))
);allegro

#-(or (and allegro unix) (and clisp linux) (and clozure unix) (and sbcl unix))
(progn
(defun can-fork-p () nil)
(defun posix-fork () nil)
(defun posix-setpgrp () nil)
(defun posix-waitpid (pid &key &allow-other-keys) (values -1 :einval))
(defun posix-wexitstatus (x) x)
);unsupported implementations

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
    (with-safe-io-syntax ()
      (write (reify-simple-sexp
              `(:process-done
                ,@(when result `(:result ,result))
                ,@(when condition `(:condition ,(princ-to-string condition)))))
             :stream s))))

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
           (with-safe-io-syntax ()
             (unreify-simple-sexp (read s)))))
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
         (quit 0 nil)))
      (t ; in the parent
       (make-instance 'background-process
                      :pid pid
                      :result-file result-file
                      :cleanup cleanup
                      :data data)))))

(defun call-queue/forking (fun fg-queue bg-queue
			   &key announce cleanup result-file deterministic-order)
  ;; assumes a single-threaded parent process
  (declare (optimize debug))
  (let ((processes (make-hash-table :test 'equal)))
    (labels
        ((fg-perform (action)
           (funcall announce action nil)
           (multiple-value-bind (result condition)
               (ignore-errors (values (funcall fun action nil)))
             (funcall cleanup action result condition nil)))
         (cleanup-one (process status)
           (multiple-value-bind (result condition)
               (process-result process status)
             (funcall (process-cleanup process)
                      (process-data process) result condition t)))
         (reap (&key wait)
           (disable-other-waiters)
           (multiple-value-bind (pid status)
               (timed-do (*time-spent-waiting*) (posix-waitpid -1 :nohang (not wait)))
             (etypecase pid
               ((eql 0) ;; no process ended and nohang? Just return NIL.
                nil)
               ((integer 1 *) ;; some process ended? reap it!
                (let ((process (gethash pid processes)))
                  (assert process () "couln't find the pid ~A in processes ~S" pid (table-values processes))
                  (remhash pid processes)
                  (cleanup-one process status))
                t)
               ((eql -1) ;; error?
                (assert (eql status +echild+) (status))
                ;; we were waiting for some process(es),
                ;; but the OS says everything was already reaped?
                ;; Our implementation or some library may have disabled the SIGCHLD signal
                ;; or preempted our wait. Mark all processes as completed.
                (let ((missed (table-values processes)))
                  (warn "No child left: we must have dropped a signal!")
                  (clrhash processes)
                  (dolist (process missed)
                    (cleanup-one process nil)))
                t)))))
      (loop
        (let* ((no-fg-item? (empty-p fg-queue))
               (fg-item? (not no-fg-item?))
               (no-bg-item? (empty-p bg-queue))
               (bg-item? (not no-bg-item?))
               (no-processes? (empty-p processes))
               (processes? (not no-processes?))
               (no-bg-workers? (>= (size processes) *max-forks*))
               (bg-workers? (not no-bg-workers?))
               (work-to-fork? (and bg-item? bg-workers?)))
          (cond
            (;; Opportunistically reap any completed background process with no wait;
             ;; wait and reap if nothing else to do.
             (and processes?
                  (reap :wait (and (not work-to-fork?)
                                   (or no-fg-item? deterministic-order)))))
            (;; Can run stuff in the background? Keep those CPUs busy!
             work-to-fork?
             (let ((item (dequeue bg-queue)))
               (funcall announce item t)
               (let ((process (make-background-process item fun cleanup (funcall result-file item))))
                 (setf (gethash (process-pid process) processes) process)
                 (latest-stamp-f *max-actual-forks* (size processes)))))
            (;; foreground actions in non-deterministic mode? Opportunistically run one
             (and fg-item? (not deterministic-order))
             (fg-perform (dequeue fg-queue)))
            (;; foreground actions in deterministic mode after exhausting background actions?
             ;; run them all in traversal order
             (and fg-item? deterministic-order no-processes? no-bg-item?)
             (map () #'fg-perform (sort (dequeue-all fg-queue) #'< :key deterministic-order)))
            (;; Nothing to do or wait for anymore? done!
             (and no-fg-item? no-bg-item? no-processes?)
             (return))
            (t
             (assert nil (bg-queue fg-queue processes)))))))))

(defmacro doqueue/forking ((fg-queue bg-queue
                            &key variables deterministic-order
                              (announce nil) (cleanup nil) result-file)
                           &body body)
  (destructuring-bind (&key item backgroundp result condition) variables
    `(call-queue/forking
      #'(lambda (,item ,backgroundp) (declare (ignorable ,item ,backgroundp)) ,@body)
      ,fg-queue ,bg-queue
      :deterministic-order ,deterministic-order
      :result-file #'(lambda (,item) (declare (ignorable ,item)) ,result-file)
      :announce #'(lambda (,item ,backgroundp) (declare (ignorable ,item ,backgroundp)) ,announce)
      :cleanup #'(lambda (,item ,result ,condition ,backgroundp)
                   (declare (ignorable ,item ,result ,condition ,backgroundp)) ,cleanup))))

;;; Performing a parallel plan
(defun action-result-file (o c)
  (let ((p (component-pathname c)))
    (apply-output-translations
     (make-pathname :name (format nil "~A.ASDF-~A" (file-namestring p) (type-of o))
                    :type "process-result" :defaults p))))

(defun action-effectively-done-p (plan operation component &key force)
  (or (action-already-done-p plan operation component)
      (and (not force)
           (nth-value 1 (compute-action-stamp nil operation component)))))

(defmethod perform-plan ((plan parallel-plan) &key force verbose &allow-other-keys)
  (unless (can-fork-p)
    (warn #+(or clozure sbcl) "You are running threads, so it is not safe to fork. Running your build serially."
          #-(or clozure sbcl) "Your implementation cannot fork. Running your build serially.")
    (return-from perform-plan (perform-plan (serialize-plan plan))))
  (with-slots (starting-points children parents planned-output-action-count) plan
    (let* ((all-deferred-warnings nil)
           (ltogo (unless (zerop planned-output-action-count) (ceiling (log planned-output-action-count 10))))
           (fg-queue (simple-queue))
           (bg-queue (simple-queue)))
      (labels ((background-p (action)
                 (destructuring-bind (o . c) action
                   (not (or (needed-in-image-p o c)
                            (action-effectively-done-p plan o c :force force)))))
               (categorize-starting-points ()
                 (loop :for action :in (dequeue-all starting-points) :do
                   (enqueue (if (background-p action) bg-queue fg-queue) action))))
        (categorize-starting-points)
        (doqueue/forking
            (fg-queue bg-queue
             :variables (:item action :backgroundp backgroundp :result result :condition condition)
             :deterministic-order
             (when (plan-deterministic-p plan)
               #'(lambda (action)
                   (destructuring-bind (o . c) action
                     (action-index (plan-action-status plan o c)))))
             :announce
             (when verbose
               (destructuring-bind (o . c) action
                 (format t "~&Will ~:[try~;skip~] ~A in ~:[foreground~;background~]~%"
                         (action-effectively-done-p plan o c :force force)
                         (action-description o c) backgroundp)))
             :result-file
             (destructuring-bind (o . c) action (action-result-file o c))
             ;; How we cleanup in the foreground after an action is run
             :cleanup
             (destructuring-bind (o . c) action
               (cond
                 (condition
                  (finish-outputs)
                  (warn "Failed ~A~:[~; in the background~]. Retrying~:*~:[~; in the foreground~]."
                        (action-description o c) backgroundp)
                  (finish-outputs)
                  (perform-with-restarts o c))
                 (t
                  (mark-operation-done o c)
                  (destructuring-bind (&key &allow-other-keys) result)))
               (when backgroundp
                 (decf planned-output-action-count)
                 (format t "~&[~vd to go] Done ~A~%"
                         ltogo planned-output-action-count (action-description o c))
                 (finish-outputs))
               (mark-as-done plan o c)
               (categorize-starting-points)))
          ;; What we do in each forked process
          (destructuring-bind (o . c) action
            (cond
              (backgroundp
               (perform o c)
               `(:deferred-warnings ,(reify-deferred-warnings)))
              ((action-effectively-done-p plan o c)
               (unless (or (not (needed-in-image-p o c))
                           (action-already-done-p nil o c))
                 (warn "WTF? aedp ~A" (action-description o c)))
               nil)
              (t
               (perform-with-restarts o c)
               nil))))
        (mapc #'unreify-deferred-warnings all-deferred-warnings)
        (assert (and (empty-p fg-queue) (empty-p bg-queue) (empty-p children))
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
    (format *breadcrumb-stream* "~S~%" (reify-action (cons operation component)))
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

(defmethod operate :before ((operation operation) system &key
                            (breadcrumbs-to nil record-breadcrumbs-p)
                            ((:using-breadcrumbs-from breadcrumb-input-pathname)
                             (make-broadcast-stream) read-breadcrumbs-p)
                            &allow-other-keys)
  (declare (ignorable system))
  (recording-breadcrumbs (breadcrumbs-to record-breadcrumbs-p)
    (when read-breadcrumbs-p
      (perform-plan (read-breadcrumbs-from operation breadcrumb-input-pathname)))))
