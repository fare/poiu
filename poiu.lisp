;;; This is POIU: Parallel Operator on Independent Units
(cl:in-package #:asdf)
(defparameter *poiu-version* "1.006")
(defparameter *asdf-version-required-by-poiu* "1.702")
#|
POIU is a modification of ASDF that may operate on your systems in parallel.
This version of POIU was designed to work with ASDF no earlier than specified.

POIU will notably compile each Lisp file in its own forked process,
in parallel with other operations (compilation or loading). However,
it will load FASLs serially as they become available.

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
files that it will happen to compile in parallel (but won't catch other
dependencies that may otherwise be missing from your system).

When a compilation fails in a parallel process, POIU will retry compiling
in the main (loading) process so you get the usual ASDF error behavior,
with a chance to debug the issue and restart the operation.

POIU was currently only made to work with SBCL and CCL. Porting to another
Lisp implementation that supports ASDF should not be difficult.

Warning to CCL users: you need to save a CCL image that doesn't start threads
at startup in order to use POIU (or anything that uses fork). Watch QRITA
for some package that does just that.

To use POIU, (1) make sure asdf.lisp is loaded - we recommend ASDF 1.366 or
later but you might have luck with an earlier version. Usually, you can
	(require :asdf)
(2) configure ASDF's *CENTRAL-REGISTRY*, then load POIU. (require :poiu)
might work, and on a recent ASDF you can definitely (asdf:load-system :poiu)
but the surest way to tell ASDF to load POIU is
	(asdf:operate 'asdf:load-op :poiu)
(alternatively, you might manually (load "/path/to/poiu"), but you might as
well test your configuration of ASDF).
(3) Actually use POIU, with such commands as
	(asdf:operate 'asdf:parallel-load-op :your-system)
Once again, you may want to first use asdf-dependency-grovel to minimize
the dependencies in your system.

POIU was initially written by Andreas Fuchs in 2007
as part of an experiment funded by ITA Software, Inc.
It was subsequently modified by Francois-Rene Rideau at ITA Software, who
wrote the CCL port, and eventually adapted it for use with XCVB in 2009.
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or clisp sbcl clozure)
  (error "POIU doesn't support your Lisp implementation.")
  #-asdf2
  (error "POIU requires ASDF2.")
  #+asdf2
  (unless (asdf:version-satisfies (asdf:asdf-version) *asdf-version-required-by-poiu*)
    (error "POIU ~A requires ASDF ~A or later."
           *poiu-version*
           *asdf-version-required-by-poiu*))
  #+sbcl (require :sb-posix)
  (export '(parallel-load-op parallel-compile-op operation-necessary-p))
  (pushnew :poiu *features*))

(define-modify-macro nconcf (x) nconc)

(defmacro remove-method-if-defined
    (method-name specializers &optional qualifiers)
  `(when (find-method (function ,method-name) ',qualifiers
                      ',specializers
                      nil)
     (remove-method (function ,method-name)
                    (find-method (function ,method-name)
                                 ',qualifiers
                                 ',specializers))))

(defclass parallelizable-operation (operation) ())

(defclass parallel-op (parallelizable-operation)
  ((operations :initarg :operations :accessor parallel-operations)))

(defvar *breadcrumb-stream* (make-broadcast-stream)
  "Stream that records the trail of operations on components.
As the order of ASDF operations in general and parallel operations in
particular are randomized, it is necessary to record them to replay &
debug them later.")

(defgeneric can-run-in-background-p (operation)
  (:method ((operation parallelizable-operation))
    nil))

(defgeneric dependee-operations-necessary-p (operation component)
  (:method ((op compile-op) component)
    (declare (ignorable op component))
    t)
  (:method (op component)
    (declare (ignorable op component))
    nil))

(defgeneric operation-necessary-p (operation component)
  (:method ((op compile-op) component)
    (declare (ignorable op component))
    t)
  (:method (op component)
    (declare (ignorable op component))
    nil))

(defclass parallel-compile-op (compile-op parallelizable-operation) ())

(defmethod can-run-in-background-p ((op parallel-compile-op))
  t)

(defclass parallel-load-op (load-op parallelizable-operation) ())

(macrolet ((def-depend-method (class)
               `(defmethod component-depends-on ((operation ,class) c)
                  (declare (ignorable c))
                  (let ((what-would-the-regular-op-do (call-next-method)))
                    (mapcar (lambda (deed)
                              (case (car deed)
                                (compile-op (cons 'parallel-compile-op (cdr deed)))
                                (load-op (cons 'parallel-load-op (cdr deed)))
                                (otherwise deed)))
                            what-would-the-regular-op-do)))))
  (def-depend-method parallel-compile-op)
  (def-depend-method parallel-load-op))

;;; XXX: (operate 'parallel-compile-op :system) ; is broken.
;;; This is caused by my inability to comprehend what it /should/ do.
(defmethod component-depends-on :around ((op parallel-load-op) (c module))
  `((parallel-load-op ,@(component-load-dependencies c))))

(defun component-equal (c1 c2)
  (or (and (null c1) (null c2))
      (and (equal (component-name c1) (component-name c2))
           (component-equal (component-parent c1) (component-parent c2)))))

(defun deed-equal (deed1 deed2)
  (and (eql (car deed1) (car deed2))
       (component-equal (second deed1) (second deed2))))

(defun make-dependency-trees (operation module &optional
                              ;; component -> dependency map
                              (direct-entries (make-hash-table :test #'equal))
                              ;; dependency -> component map
                              (indirect-entries (make-hash-table :test #'equal))
                              additional-dependencies)
  (let (starting-points)
    (labels ((ensure-component (coid)
               (etypecase coid
                 (component coid)
                 ((or symbol string) (find-component module (coerce-name coid)))))
             (ensure-operation (opoid)
               (etypecase opoid
                 (symbol (make-instance opoid))
                 (operation opoid)))
             (add-to-tree (dependency this-operation)
               ;; don't record dependencies from component on itself
               (when (equal dependency this-operation)
                 (return-from add-to-tree nil))
               (unless (gethash this-operation direct-entries)
                 (setf (gethash this-operation direct-entries)
                       (make-hash-table :test #'equal)))
               (unless (gethash dependency indirect-entries)
                 (setf (gethash dependency indirect-entries)
                       (make-hash-table :test #'equal)))
               (setf (gethash dependency (gethash this-operation direct-entries)) t)
               (setf (gethash this-operation (gethash dependency indirect-entries)) t))
             (is-in-tree-p (dependency)
               (gethash dependency direct-entries nil))
             (normalize-dependencies (deps)
               (loop :for (op . dep) :in deps
                 :append (mapcan (lambda (dep* &aux (dep (ensure-component dep*)))
                                   (typecase dep
                                     (module
                                      (normalize-dependencies
                                       `((,(class-name (class-of (ensure-operation op)))
                                           ,@(module-components dep)))))
                                     (component (list (list op dep)))))
                                 dep)))
             (do1 (operation component)
               (when (is-in-tree-p (list (class-name (class-of operation)) component))
                 (return-from do1 nil))
               (typecase component
                 (module
                  (let* ((component-parents (loop :for parent = component :then (component-parent parent)
                                              :while parent :collect parent))
                         (deps (loop :for (op . deps) :in (component-depends-on operation component)
                                 :for real-deps =
                                 (set-difference (mapcar (lambda (dep)
                                                           (find-component (component-parent component)
                                                                           (coerce-name dep)))
                                                         deps)
                                                 component-parents)
                                 :when real-deps :collect `(,op ,@real-deps))))
                    (nconcf starting-points
                            (make-dependency-trees
                             operation component direct-entries indirect-entries
                             (append additional-dependencies
                                     (when deps
                                       (normalize-dependencies deps)))))
                    (dolist (d deps)
                      (do1 (ensure-operation (first d)) (ensure-component (second d))))))
                 (component
                  (let* ((this-op (list (class-name (class-of operation))
                                        component))
                         (deps (normalize-dependencies
                                (component-depends-on operation component)))
                         (all-deps (append additional-dependencies deps)))
                    (unless all-deps
                      (pushnew this-op starting-points :test #'equal))
                    (loop :for d :in all-deps
                      :do (add-to-tree d this-op))
                    (loop :for d :in deps
                      :do (do1 (ensure-operation (first d))
                                  (ensure-component (second d)))))))))
      (dolist (component (module-components module))
        (do1 operation component))
      (values starting-points
              indirect-entries
              direct-entries))))

(defun mark-as-done (operation component indirect-deps direct-deps)
  (let* ((this-op (list (class-name (class-of operation))
                        component))
         (dependees (when (gethash this-op indirect-deps)
                      (loop :for dependee :being
                        the hash-keys :in (gethash this-op indirect-deps)
                        :collect dependee))))
    (remhash this-op direct-deps)
    (loop :for dependee :in dependees
      :do (assert (gethash dependee direct-deps))
      :do (remhash this-op (gethash dependee direct-deps))
      :when (zerop (hash-table-count (gethash dependee direct-deps)))
      :collect dependee
      :and :do (remhash dependee direct-deps))))

(defun summarize-direct-deps (dir)
  (sort (loop :for key :being the hash-key :in dir :using (:hash-value val)
          :collect (list key
                         (loop :for innerkey :being the hash-key :in val :using (:hash-value v)
                           :when v :collect innerkey)))
        #'< :key (lambda (depl) (length (cdr depl)))))

(defun check-dependency-trees (module starting-points indirect-entries direct-entries)
  (loop :until (null starting-points) :do
    (destructuring-bind (op-class component) (pop starting-points)
      (nconcf starting-points
              (mark-as-done (make-instance op-class) component
                            indirect-entries direct-entries))))
  (unless (zerop (hash-table-count direct-entries))
    (error "Cycle detected in the dependency graph of ~A. Direct dependencies are:~%~S"
           module (summarize-direct-deps direct-entries))))

(defun make-checked-dependency-trees (operation module)
  (multiple-value-call #'check-dependency-trees
    module (make-dependency-trees operation module))
  (make-dependency-trees operation module))

(defparameter *max-forks* 16)

;;; subprocesses: data structure, ipc

(defvar *current-subprocess*)

(defparameter *default-process-result*
  '())
(defparameter *failed-process-result*
  '(:failure-p t :performed-p t))

(defclass communicating-subprocess ()
  ((pid :initarg :pid :accessor process-pid)
   (data :initarg :data :accessor process-data)
   (cleanup-thunk :initarg :cleanup :accessor process-cleanup)
   (status-pipe :accessor status-pipe)))

#|
(defclass communicating-thread ()
  ((thread :initarg :thread :accessor process-thread)
   (data :initarg :data :accessor process-data)
   (cleanup-thunk :initarg :cleanup :accessor process-cleanup)
   (lock :initform (ccl:make-lock) :accessor process-lock)
   (status :initform () :accessor process-status)))
|#

(defun process-result (status result-pipe)
  (prog1
      (if (= 0 (posix-wexitstatus status))
        (read result-pipe nil nil)
        *failed-process-result*)
    (close result-pipe)))

(defun process-return (proc result)
  (prin1 result (status-pipe proc)))

(defun finish-outputs ()
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (values))

#+sbcl
(progn

(defun posix-exit (n)
  (sb-unix:unix-exit n))

;; Simple heuristic: if we have allocated more than the given ratio
;; of what is allowed between GCs, then trigger the GC.
;; Note: can possibly modify parameters and reset in sb-ext:*after-gc-hooks*
(defparameter *prefork-allocation-reserve-ratio* .80) ; default ratio: 80%

(defun should-i-gc-p ()
  (let ((available-bytes (- (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long)
                            (sb-kernel:dynamic-usage)))
        (allocation-threshhold (sb-ext:bytes-consed-between-gcs)))
    (< available-bytes (* *prefork-allocation-reserve-ratio* allocation-threshhold))))

(defun posix-fork ()
  (unless (null (cdr (sb-thread:list-all-threads)))
    (error "Cannot fork: more than one active thread."))
  (when (should-i-gc-p)
    (sb-ext:gc))
  (sb-posix:fork))

(defun posix-close (x)
  (sb-posix:close x))

(defun posix-setpgrp ()
  (sb-posix:setpgrp))

(defun posix-wait ()
  (sb-posix:wait))

(defun posix-wexitstatus (x)
  (sb-posix:wexitstatus x))

(defun posix-pipe ()
  (sb-posix:pipe))

(defun make-output-stream (fd)
  (sb-sys:make-fd-stream fd :output t))

(defun make-input-stream (fd)
  (sb-sys:make-fd-stream fd :input t))

)      ;#+sbcl


#+clozure
(progn

(defun posix-exit (n)
  (ccl:quit n))

(defun posix-fork ()
  (unless (null (cdr (ccl:all-processes)))
    (error "Cannot fork: more than one active thread. Are you using single-threaded-ccl?"))
  (ccl:external-call "fork" :int))

(defun posix-close (x)
  (ccl::fd-close x))

(defun posix-setpgrp ()
  (ccl::external-call "setpgrp" :int))

(defun posix-wait ()
  (ccl::rlet ((status :signed))
    (let* ((retval (ccl::external-call "wait" :address status :signed)))
      (values retval (ccl::pref status :signed)))))

(defun posix-wexitstatus (x)
  (ccl::wexitstatus x))

(defun posix-pipe ()
  (ccl::pipe))

(defun make-output-stream (fd)
  (ccl::make-fd-stream fd :direction :output))

(defun make-input-stream (fd)
  (ccl::make-fd-stream fd :direction :input))

)      ;#+clozure

#+clisp ;;; CLISP specific fork support
(progn

(defun posix-exit (n)
  (ext:quit n))

(defun posix-fork ()
  (linux:fork))

(defun posix-close (x)
  (linux:close x))

(defun posix-setpgrp ()
  (posix:setpgrp))

(defun posix-wait ()
  (multiple-value-bind (pid status code) (posix:wait)
    (values pid (list pid status code))))

(defun posix-wexitstatus (x)
  (if (eq :exited (second x))
    (third x)
    (cons (second x) (third x))))

(defun posix-pipe ()
  (multiple-value-bind (code p) (linux:pipe)
    (unless (zerop code)
      (error "couldn't make pipes"))
    (values (aref p 0) (aref p 1))))

(defun make-output-stream (fd)
  (ext:make-stream fd :direction :output))

(defun make-input-stream (fd)
  (ext:make-stream fd :direction :input))

);#+clisp

(defun make-communicating-subprocess (data continuation cleanup)
  (multiple-value-bind (read-fd write-fd) (posix-pipe)
    ;; Try to undo problems caused by sb-ext:run-program. XXX: hack.
    ;; Will still cause a race condition if an ASDF op calls run-program at load-time.
    ;; But this work-around makes it is safe to call run-program before to invoke poiu
    ;; (it is of course safe after). The true fix to allow run-program to be invoked
    ;; at load-time would be to define and hook into an exported interface for process interaction.
    #+sbcl
    (sb-sys:default-interrupt sb-unix:sigchld) ; ignore-interrupt is undefined for SIGCHLD.
    (finish-outputs)
    (let* ((pid (posix-fork))
           (proc (make-instance 'communicating-subprocess
                    :pid pid
                    :cleanup cleanup
                    :data data)))
      (cond ((zerop pid)
             ;; don't receive the parent's SIGINTs
             (posix-setpgrp)
             ;; close the read end, set the write end to be the status reporter.
             (posix-close read-fd)
             (setf (status-pipe proc) (make-output-stream write-fd))
             (when (find-package :sb-sprof)
               (funcall (intern "STOP-PROFILING" :sb-sprof)))
             (let ((*current-subprocess* proc))
               #+sbcl (sb-ext:disable-debugger)
               #+clozure (setf ccl::*batch-flag* t)
               (unwind-protect (funcall continuation data)
                 (close (status-pipe proc))
                 (finish-outputs)
                 (posix-exit 0))))
            (t
             ;; close the write end, set up the read end
             (posix-close write-fd)
             (setf (status-pipe proc) (make-input-stream read-fd))
             proc)))))

#+clozure
(defparameter *null-stream*
  (open "/dev/null" :direction :io :if-does-not-exist :error :if-exists :append))

#|
#+clozure
(defun make-communicating-thread (semaphore data continuation cleanup)
  (let* ((proc (make-instance 'communicating-thread
                 :cleanup cleanup
                 :data data))
         (thread (ccl::process-run-function
                  "worker"
                  (lambda ()
                    (handler-case
                        (let ((*current-subprocess* proc)
                              (*standard-input* *null-stream*))
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
|#

;;; Timing the build process

(defvar *time-spent-waiting* 0)

(defmacro timed-do ((time-accumulator) &body body)
  (let ((time-before-thing (gensym)))
    `(let ((,time-before-thing (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
              (incf ,time-accumulator (- (get-internal-real-time)
                                         ,time-before-thing))))))

;;; Handling multiple processes

(defun call-queue/forking (thunk queue-empty-p queue-popper &key announcer cleanup (background-p (constantly t)))
  ;; assumes a single-threaded parent process
  (declare (optimize debug))
  (let ((elem nil)
        (pid-map nil)
        (count 0))
    (loop
      (cond (;; nothing to do or wait for anymore.
             (and (funcall queue-empty-p) (null pid-map))
             (return))
            (;; we've exceeded the subprocess limit. Wait for a few before continuing.
             (or (>= count *max-forks*)
                 (funcall queue-empty-p))
             (multiple-value-bind (pid status)
                 (timed-do (*time-spent-waiting*) (posix-wait))
               (let ((entry (find pid pid-map :key #'process-pid)))
                 (assert entry () "couln't find the pid ~A in pid-map ~S" pid pid-map)
                 (setf pid-map (delete entry pid-map))
                 (decf count)
                 (funcall (process-cleanup entry) (process-data entry)
                          (process-result status (status-pipe entry)))))))
      (unless (funcall queue-empty-p)
        (setf elem (funcall queue-popper))
        (funcall announcer elem)
        (cond
          ((funcall background-p elem)
           (incf count)
           (push (make-communicating-subprocess elem thunk cleanup) pid-map))
          (t
           (unwind-protect (funcall thunk elem)
             (funcall cleanup elem *default-process-result*))))))
    (assert (and (funcall queue-empty-p) (null pid-map)) ()
            "List of processes or list of things to do isn't empty: (~S...)/~S~%"
            (funcall queue-popper)
            pid-map))
  nil)

#|
#+clozure
(defun call-queue/threading (thunk queue-empty-p queue-popper &key cleanup (background-p (constantly t)))
  ;; will use threads instead of fork
  (declare (optimize debug))
  (let ((elem nil)
        (processes nil)
        (count 0)
        (pending (ccl:make-semaphore)))
    (loop
      (cond (;; nothing to do or wait for anymore.
             (and (funcall queue-empty-p) (null processes))
             (return))
            (;; we've exceeded the subprocess limit. Wait for a few before continuing.
             (or (>= count *max-forks*)
                 (funcall queue-empty-p))
             (timed-do (*time-spent-waiting*) (ccl::wait-on-semaphore pending))
             (let ((entry (find-if #'process-complete-p processes)))
               (assert entry () "couln't find a completed process in ~S" processes)
               (setf processes (delete entry processes))
               (decf count)
               (funcall (process-cleanup entry) (process-data entry) (thread-result entry)))))
      (unless (funcall queue-empty-p)
        (setf elem (funcall queue-popper))
        (cond
          ((funcall background-p elem)
           (incf count)
           (push (make-communicating-thread pending elem thunk cleanup) processes))
          (t
           (unwind-protect (funcall thunk elem)
             (funcall cleanup elem *default-process-result*))))))
    (assert (and (funcall queue-empty-p) (null processes)) ()
            "List of processes or list of things to do isn't empty: (~S...)/~S~%"
            (funcall queue-popper)
            processes))
  nil)
|#

(defmacro dolist/forking (((var list &key
                                (result (gensym "RESULT")))
                           &key (background-p t) (announce nil) (cleanup nil))
                          &body body)
  `(call-queue/forking
    #'(lambda (,var)
        (declare (ignorable ,var))
        ,@body)
    #'(lambda () (null ,list))
    #'(lambda () (pop ,list))
    :cleanup #'(lambda (,var ,result)
                  (declare (ignorable ,var ,result))
                  ,cleanup)
    :announcer #'(lambda (,var)
                  (declare (ignorable ,var))
                  ,announce)
    :background-p #'(lambda (,var)
                      (declare (ignorable ,var))
                      ,background-p)))

(defmethod perform :after ((operation parallel-compile-op) c)
  (setf (gethash 'compile-op (component-operation-times c))
        (get-universal-time)))

(defmethod perform :after ((operation parallel-load-op) c)
  (setf (gethash 'load-op (component-operation-times c))
        (get-universal-time)))

(defmethod perform :after ((operation load-op) c)
  (setf (gethash 'parallel-load-op (component-operation-times c))
        (get-universal-time)))

(defmethod perform :after ((operation compile-op) c)
  (setf (gethash 'parallel-compile-op (component-operation-times c))
        (get-universal-time)))

(defmethod perform :after ((operation operation) c)
  "Record the operations and components in a stream of breadcrumbs."
  (labels ((component-module-path (c)
             (unless (typep c 'system)
               (cons (coerce-name (component-name c))
                     (component-module-path (component-parent c))))))
    (format *breadcrumb-stream* "~S~%"
            `(,(class-name (class-of operation))
               ,(coerce-name (component-name (component-system c)))
               ,@(component-module-path c)))
    (force-output *breadcrumb-stream*)))

(defmethod perform ((operation parallelizable-operation) (module module))
  (multiple-value-bind (ops ind dir) (make-checked-dependency-trees operation module)
    (labels ((opspec-op (opspec)
               (make-instance (first opspec)))
             (opspec-component (opspec)
               (second opspec))
             (opspec-necessary-p (opspec)
               (third opspec)))
      (unless (null ops)
        (dolist/forking
            ((op ops :result result)
             :background-p (and (can-run-in-background-p (opspec-op op))
                                (or (not (operation-done-p
                                          (opspec-op op)
                                          (opspec-component op)))
                                    (opspec-necessary-p op)))
             :cleanup
             (destructuring-bind (&key failure-p performed-p &allow-other-keys)
                 result
               (when failure-p
                 (finish-outputs)
                 (warn "Operation ~A has failure-p set. Retrying in this process." op)
                 (finish-outputs)
                 (perform (opspec-op op) (opspec-component op)))
               (dolist (opened-op (mark-as-done (opspec-op op)
                                                (opspec-component op)
                                                ind dir))
                 (when (or (opspec-necessary-p op)
                           (and performed-p
                                (dependee-operations-necessary-p
                                 (opspec-op op)
                                 (opspec-component op))))
                   (nconcf opened-op
                           (list (operation-necessary-p
                                  (opspec-op opened-op)
                                  (opspec-component opened-op)))))
                 (if (can-run-in-background-p (opspec-op opened-op))
                     (push opened-op ops)
                     (setf ops (nconc ops (list opened-op)))))))
          (when (or (not (operation-done-p (opspec-op op) (opspec-component op)))
                    (opspec-necessary-p op))
            (perform (opspec-op op) (opspec-component op)))))
      (assert (zerop (hash-table-count dir))
              (dir ind)
              "Direct dependency table is not empty - there is a problem ~
               with the dependency trees:~%~S" (summarize-direct-deps dir)))))

(defmethod traverse ((operation parallelizable-operation) (c module))
  (when (component-visiting-p operation c)
    (error 'circular-dependency
           :components (list c)))
  (setf (visiting-component operation c) t)
  (prog1
      (unless (component-visited-p operation c)
        (nconc
         (loop
           :for (required-op . deps) :in (component-depends-on operation c)
           :for required-deeds =
           (loop
             :for req-c :in deps
             :for dep-c = (or (find-component
                               (component-parent c)
                               (coerce-name req-c)) ;; TODO: version
                              (error 'missing-dependency
                                     :required-by c
                                     :requires req-c))
             :for dep-op = (make-sub-operation c operation dep-c required-op)
             :collect (cons dep-op dep-c))
           :append (loop :for (dep-op . dep-c) :in required-deeds
                     :append (traverse dep-op dep-c)))
         (list (cons operation c))))
    (setf (visiting-component operation c) nil)
    (visit-component operation c t)))

(defmethod perform :before ((operation parallel-compile-op) (c source-file))
  (map nil #'ensure-directories-exist (output-files operation c)))

(defmethod perform ((op parallel-compile-op) (c cl-source-file))
  (let ((compile-status (list
                         :input-file (car (input-files op c))
                         :performed-p t
                         :output-truename (car (output-files op c))
                         :warnings-p nil
                         :failure-p t))
        warnings-p failure-p output-truename)
    (unwind-protect (progn
                      (multiple-value-setq (output-truename warnings-p failure-p)
                          (compile-file (car (input-files op c))
                                        :output-file (car (output-files op c))))
                      (setf compile-status
                            (list :input-file (car (input-files op c))
                                  :performed-p t
                                  :output-truename output-truename
                                  :warnings-p warnings-p
                                  :failure-p failure-p)))
      (finish-outputs)
      (cond
        ((boundp '*current-subprocess*)
         (process-return *current-subprocess* compile-status))
        (t
         (when warnings-p
           (case (operation-on-warnings op)
             (:warn (warn
                     "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
                     op c))
             (:error (error 'compile-warned :component c :operation op))
             (:ignore nil)))
         (when failure-p
           (case (operation-on-failure op)
             (:warn (warn
                     "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
                     op c))
             (:error (error 'compile-failed :component c :operation op))
             (:ignore nil)))
         (unless output-truename
           (error 'compile-error :component c :operation op)))))))

(defmethod operation-done-p ((op parallelizable-operation) (c module))
  "A lazy operation on a module is done only when the op on all its
components is done."
  (labels ((dependency-done-p (op sub-c)
             (loop :for (dep-op-name . dep-component-names)
               :in (component-depends-on op sub-c)
               :for dep-op = (make-instance dep-op-name)
               :do (loop :for dep-component-name :in dep-component-names
                     :for dep-c = (find-component (component-parent sub-c)
                                                  dep-component-name)
                     :do (unless (operation-done-p dep-op dep-c)
                           (return-from dependency-done-p nil))))
             t))
    (every (lambda (sub-c)
             (and (dependency-done-p op sub-c)
                  (operation-done-p op sub-c)))
           (module-components c))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (remove-method-if-defined traverse (operation component))
(defmethod traverse ((operation operation) (c component))
  (let ((forced nil))
    (labels ((do-one-dep (required-op required-c required-v)
               (let* ((dep-c (or (find-component
                                  (component-parent c)
                                  ;; XXX tacky.  really we should build the
                                  ;; in-order-to slot with canonicalized
                                  ;; names instead of coercing this late
                                  (coerce-name required-c) required-v)
                                 (error 'missing-dependency :required-by c
                                        :version required-v
                                        :requires required-c)))
                      (op (make-sub-operation c operation dep-c required-op)))
                 (traverse op dep-c)))
             (do-dep (op dep)
               (cond ((eq op 'feature)
                      (or (member (car dep) *features*)
                          (error 'missing-dependency :required-by c
                                 :requires (car dep) :version nil)))
                     (t
                      (dolist (d dep)
                        (cond ((consp d)
                               (assert (string-equal
                                        (symbol-name (first d))
                                        "VERSION"))
                               (appendf forced
                                        (do-one-dep op (second d) (third d))))
                              (t
                               (appendf forced (do-one-dep op d nil)))))))))
      (aif (component-visited-p operation c)
           (return-from traverse
             (if (cdr it) (list (cons 'pruned-op c)) nil)))
      ;; dependencies
      (if (component-visiting-p operation c)
          (error 'circular-dependency :components (list c)))
      (setf (visiting-component operation c) t)
      (loop :for (required-op . deps) :in (component-depends-on operation c)
        :do (do-dep required-op deps))
      ;; constituent bits
      (let ((module-ops
             (when (typep c 'module)
               (let ((at-least-one nil)
                     (forced nil)
                     (error nil))
                 (loop :for kid :in (module-components c)
                   :do (handler-case
                           (appendf forced (traverse operation kid ))
                         (missing-dependency (condition)
                           (if (eq (module-if-component-dep-fails c) :fail)
                               (error condition))
                           (setf error condition))
                         (:no-error (c)
                           (declare (ignore c))
                           (setf at-least-one t))))
                 (when (and (eq (module-if-component-dep-fails c) :try-next)
                            (not at-least-one))
                   (error error))
                 forced))))
        ;; now the thing itself
        (when (or forced module-ops
                  (not (operation-done-p operation c))
                  (let ((f (operation-forced (operation-ancestor operation))))
                    (and f (or (not (consp f))
                               (member (component-name
                                        (operation-ancestor operation))
                                       (mapcar #'coerce-name f)
                                       :test #'string=)))))
          ;; only difference with original traverse is the absence of
          ;; the do-first thing.
          (setf forced (append (delete 'pruned-op forced :key #'car)
                               (delete 'pruned-op module-ops :key #'car)
                               (list (cons operation c))))))
      (setf (visiting-component operation c) nil)
      (visit-component operation c (and forced t))
      forced))))

(defmethod component-depends-on ((operation compile-op) (c component))
  (let ((load-deps (component-load-dependencies c)))
    (append (when load-deps
              `((load-op ,@default-deps)))
           (cdr (assoc 'compile-op (slot-value c 'in-order-to))))))

(defmethod operation-done-p ((operation compile-op) (c static-file))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (remove-method-if-defined component-depends-on (load-op component))
(defmethod component-depends-on ((operation load-op) (c component))
  `((load-op ,@(component-load-dependencies c))
    (compile-op ,(component-name c))
    ,@(call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoking operations

(defun read-breadcrumbs-from (pathname)

  (labels ((resolve-component-path (component path)
             (if (null path)
                 component
                 (resolve-component-path (find-component component (first path))
                                         (rest path)))))
    (with-open-file (f pathname)
      (loop :for (op-name system-name . component-path) = (read f nil nil)
        :until (null op-name)
        :collect (cons (make-instance op-name)
                       (resolve-component-path (find-system system-name)
                                               component-path))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'operate)
  (defun call-recording-breadcrumbs (pathname record-p thunk)
    (if record-p
        (with-open-file (*breadcrumb-stream*
                         pathname :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
          (funcall thunk))
        (funcall thunk)))
  (defmacro recording-breadcrumbs ((pathname record-p) &body body)
    `(call-recording-breadcrumbs ,pathname ,record-p (lambda () ,@body)))

  (defun operate (operation-class system &rest args &key (verbose t) version
                  (breadcrumbs-to nil record-breadcrumbs-p)
                  ((:using-breadcrumbs-from breadcrumb-input-pathname)
                   (make-broadcast-stream) read-breadcrumbs-p)
                  &allow-other-keys)
    (let* ((op (apply #'make-instance operation-class
                      :original-initargs args
                      args))
           (*verbose-out* (if verbose *trace-output* (make-broadcast-stream)))
           (system (if (typep system 'component) system (find-system system))))
      (unless (version-satisfies system version)
        (error 'missing-component :requires system :version version))
      (recording-breadcrumbs (breadcrumbs-to record-breadcrumbs-p)
        (labels ((operation-necessary (op c)
                   (not (operation-done-p op c))))
          (let ((steps (if read-breadcrumbs-p
                           (read-breadcrumbs-from breadcrumb-input-pathname)
                           (traverse op system))))
            (with-compilation-unit ()
              (loop :for (op . component) :in steps :do
                (loop
                  (restart-case
                      (progn (when (operation-necessary op component)
                               (perform op component))
                             (return))
                    (retry ()
                      :report
                      (lambda (s)
                        (format s "~@<Retry performing ~S on ~S.~@:>"
                                op component)))
                    (accept ()
                      :report
                      (lambda (s)
                        (format s
                                "~@<Continue, treating ~S on ~S as ~
                               having been successful.~@:>"
                                op component))
                      (setf (gethash (type-of op)
                                     (component-operation-times component))
                            (get-universal-time))
                      (return))))))))))))
