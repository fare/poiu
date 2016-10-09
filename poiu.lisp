(uiop:define-package :poiu
  (:use :uiop/common-lisp :uiop
        :poiu/queue :poiu/fork :poiu/background-process
        :asdf/upgrade
        :asdf/component :asdf/system :asdf/find-component :asdf/operation
        :asdf/action :asdf/plan :asdf/operate
        :asdf/output-translations)
  (:use-reexport :poiu/action-graph))

(in-package :poiu)

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
  (unless (can-fork-or-warn)
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
               (push (getf result :deferred-warnings) all-deferred-warnings)
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
                 (asdf-message "~&[~vd to go] Done ~A~%"
                               ltogo planned-output-action-count (action-description o c))
                 (finish-outputs))
               (mark-as-done plan o c)
               (categorize-starting-points)))
          ;; What we do in each forked process
          (destructuring-bind (o . c) action
            (cond
              (backgroundp
               (perform-with-restarts o c)
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

(setf *default-plan-class* 'parallel-plan)

