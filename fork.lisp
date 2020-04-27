;;; Low-level interface to implementation-dependent fork API
(uiop/package:define-package :poiu/fork
  (:recycle :poiu/fork :asdf)
  (:use :uiop/common-lisp :uiop/utility :uiop/os :uiop/run-program)
  (:export
   #:disable-other-waiters
   #:ncpus #:*max-forks* #:*max-actual-forks*
   #:can-fork-p #:can-fork-or-warn
   #:posix-fork #:posix-setpgrp #:posix-waitpid #:posix-wexitstatus #:+echild+
   ;; posix-close posix-pipe fd-output-stream fd-input-stream
   ))

(in-package :poiu/fork)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(and os-unix (or allegro clisp clozure sbcl))
  (warn "POIU doesn't support forking on your Lisp implementation (yet). Help port POIU!")
  #+(and clisp os-unix) (ignore-errors (funcall 'require "linux")))

(defvar *warned-no-fork-p* nil)

(defun can-fork-or-warn ()
  (or (can-fork-p)
      (unless *warned-no-fork-p*
        (setf *warned-no-fork-p* t)
        (warn
         #.(progn
             "Your implementation cannot fork. Running your build serially."
             #+(or allegro clisp clozure sbcl)
             "You are running threads, so it is not safe to fork. Running your build serially.")))))

(defun disable-other-waiters ()
  ;; KLUDGE: Try to undo problems caused by run-program.
  ;; There will still be a race condition if some action calls run-program at load-time.
  ;; But this work-around makes it safe to call run-program before to invoke poiu
  ;; (it is of course safe after). The true fix to allow run-program to be invoked
  ;; at load-time would be to have an API for a process-waiting callbacks.
  #+(and sbcl unix)
  (sb-sys:default-interrupt sb-unix:sigchld)) ; ignore-interrupt is undefined for SIGCHLD.

(defun ncpus ()
  (let ((ncpus (cond ((featurep :linux)
                      (run-program '("grep" "-c" "^processor.:" "/proc/cpuinfo") :output :string))
                     ((featurep :bsd) ;; reported to work on :darwin :freebsd :netbsd :openbsd.
                      (run-program '("sysctl" "-n" "hw.ncpu") :output :string))
                     ((os-windows-p)
                      (getenv "NUMBER_OF_PROCESSORS")))))
     (and ncpus (ignore-errors (parse-integer ncpus :junk-allowed t)))))

(defparameter *max-forks* (or (ncpus) 16)) ; limit how parallel we will try to be.
(defparameter *max-actual-forks* 0) ; record how parallel we actually went.

#+(and sbcl os-unix)
(progn
  ;; Simple heuristic: if we have allocated more than the given ratio
  ;; of what is allowed between GCs, then trigger the GC.
  ;; Note: can possibly modify parameters and reset in sb-ext:*after-gc-hooks*
  (defparameter *prefork-allocation-reserve-ratio* .80) ; default ratio: 80%
  (defun should-i-gc-p ()
    (let ((available-bytes (- (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long)
                              (sb-kernel:dynamic-usage)))
          (allocation-threshhold (sb-ext:bytes-consed-between-gcs)))
      (< available-bytes (* *prefork-allocation-reserve-ratio* allocation-threshhold)))))

#+(and clisp os-unix)
(defun no-child-process-condition-p (c)
  (and (typep c 'ext::os-error)
       (equal (simple-condition-format-control c)
              "UNIX error ~S (ECHILD): No child processes
")))

(defun can-fork-p ()
  "Is it possible to fork the current process?"
  #+(and allegro os-unix)
  (null (cdr mp:*all-processes*))
  #+(and clisp os-unix)
  (and (find-symbol* 'wait "POSIX" nil) (find-symbol* 'fork "LINUX" nil) t nil)
  #+(and clozure os-unix)
  (null (cdr (ccl::all-processes)))
  #+(and sbcl os-unix)
  (null (cdr (sb-thread:list-all-threads)))
  #-(and os-unix (or allegro clisp clozure sbcl))
  nil)

(defun posix-fork ()
  "Assuming the current process can fork, do it, and
return the child PID in the parent, -1 in the child"
  #+(and os-unix (or allegro clisp clozure sbcl))
  (unless (can-fork-p)
    (error #.(strcat "Cannot fork: more than one active thread."
                     #+clozure " Are you using single-threaded-ccl?")))
  #+(and allegro os-unix)
  (excl.osi:fork)
  #+(and clisp os-unix)
  (funcall (find-symbol* 'fork "LINUX"))
  #+(and clozure os-unix)
  (ccl:external-call "fork" :int)
  #+(and sbcl os-unix)
  (progn
    (when (should-i-gc-p)
      (sb-ext:gc))
    (sb-posix:fork))
  #-(and os-unix (or allegro clisp clozure sbcl))
  (not-implemented-error 'posix-fork))

(defun posix-setpgrp ()
  "POSIX System V style setpgrp(), makes the current process its own process group."
  #+(and allegro os-unix)
  (excl.osi:setpgrp)
  #+(and clisp os-unix)
  (if-let (it (find-symbol* 'setprg 'posix nil))
    (funcall it)
    (not-implemented-error 'posix-setpgrp))
  #+(and clozure os-unix)
  (ccl::external-call "setpgrp" :int)
  #+(and sbcl os-unix)
  (sb-posix:setpgrp)
  #-(and os-unix (or allegro clisp clozure sbcl))
  (not-implemented-error 'posix-setpgrp))

(defconstant +echild+
  #+(and allegro os-unix) excl::*echild*
  #+(and clisp os-unix) :echild
  #+(and clozure os-unix) #.(read-from-string "#$ECHILD")
  #+(and sbcl os-unix) sb-posix:echild
  #-(and os-unix (or allegro clisp clozure sbcl)) nil
  "Second value returned by WAITPID if there is no child to wait for")

(defun posix-waitpid (pid &key nohang untraced continued)
  "POSIX waitpid(), takes makes the current process its own process group.
PID is the pid of the specific child being waited for, or -1 for any child,
or 0 or a negative number as per waitpid().
nohang, untraced and continued are booleans indicated whether to use the
respective waitpid options WNOHANG, WUNTRACED and WCONTINUED.
Return two values, the pid of a child process (or -1 if none was found),
and the status of said process, to pass to posix-wexitstatus."
  #+(and allegro os-unix)
  (if-let ((flag (or (and untraced :untraced) (and continued :continued))))
    (parameter-error "~S doesn't support ~S" 'posix-waitpid flag)
    (multiple-value-bind (exit-status pid signal)
        (sys:reap-os-subprocess :pid (or pid -1) :wait (not nohang))
      (etypecase exit-status
        (null (if nohang (values 0 ()) (values -1 +echild+)))
        (integer (values pid (list exit-status signal))))))
  #+(and clisp os-unix)
  (handler-case
      (multiple-value-bind (pid status code)
          (symbol-call "POSIX" 'wait
                       :pid pid :nohang nohang :untraced untraced :continued continued)
        (case pid
          (0 (values 0 ()))
          (-1 (values -1 :error))
          (t (values pid (list pid status code)))))
    ((and ext::os-error (satisfies no-child-process-condition-p)) ()
      (values -1 +echild+)))
  #+(and clozure os-unix)
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
        (t (values retval (ccl::pref status :signed))))))
  #+(and sbcl os-unix)
  (progn
    (when continued (parameter-error "~S doesn't support ~S" 'posix-waitpid :continued))
    (handler-case
        (sb-posix:waitpid
         (or pid -1)
         (logior (if nohang sb-posix:wnohang 0)
                 (if untraced sb-posix:wuntraced 0)))
      (sb-posix:syscall-error (c)
        (values -1 (sb-posix:syscall-errno c)))))
  #-(and os-unix (or allegro clisp clozure sbcl))
  (progn pid nohang untraced continued
         (not-implemented-error 'posix-setpgrp)))

(defun posix-wexitstatus (x)
  "Convert the status return by POSIX-WAITPID to an exit code between 0 and 255"
  #+(and allegro os-unix)
  (first x)
  #+(and clisp os-unix)
  (if (eq :exited (second x))
    (third x)
    (cons (second x) (third x)))
  #+(and clozure os-unix)
  (ccl::wexitstatus x)
  #+(and sbcl os-unix)
  (sb-posix:wexitstatus x)
  #-(and os-unix (or allegro clisp clozure sbcl))
  (not-implemented-error 'posix-wexitstatus))

(defun posix-pipe ()
  "Create a POSIX pipe and return as two values the corresponding input and output streams"
  #+(and allegro os-unix)
  (excl:make-pipe-stream)
  #+(and clisp os-unix)
  (multiple-value-bind (code p) (LINUX:pipe)
    (unless (zerop code)
      (error "couldn't make pipes"))
    (values (ext:make-stream (aref p 0) :direction :input)
            (ext:make-stream (aref p 1) :direction :output)))
  #+(and clozure os-unix)
  (multiple-value-bind (read-fd write-fd) (ccl::pipe)
    (values (ccl::make-fd-stream read-fd :direction :input)
            (ccl::make-fd-stream write-fd :direction :output)))
  #+(and sbcl os-unix)
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream read-fd :input t)
            (sb-sys:make-fd-stream write-fd :output t)))
  #-(and os-unix (or allegro clisp clozure sbcl))
  (not-implemented-error 'posix-pipe))

