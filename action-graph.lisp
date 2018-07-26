(uiop:define-package :poiu/action-graph
  (:use :uiop/common-lisp :uiop :poiu/queue
        :asdf/upgrade :asdf/session
        :asdf/component :asdf/system :asdf/find-system :asdf/find-component
        :asdf/operation :asdf/action :asdf/plan)
  (:export #:parallel-plan #:*parallel-plan-deterministic-p*
           #:summarize-plan #:serialize-plan
           #:starting-points #:children #:parents ;; slot names -- FIXME, have clients use accessors
           #:plan-starting-points #:plan-children #:plan-parents #:plan-action-index
           #:plan-deterministic-p
           #:action-map #:action-map-keys #:action-map-values))
(in-package :poiu/action-graph)

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
   (action-indices
    :initform (make-hash-table :test #'equal) :reader plan-action-indices
    :documentation "inverting the all-actions table")
   (deterministic-p
    :initform *parallel-plan-deterministic-p* :initarg :deterministic-p
    :type boolean :reader plan-deterministic-p
    :documentation "is this plan supposed to be executed in deterministic way?")))

(defgeneric plan-action-index (plan action))
(defmethod plan-action-index ((plan parallel-plan) action)
  (gethash action (plan-action-indices plan)))

#| ;; We can't do that if we want to trace action-already-done-p
(defmethod print-object ((plan parallel-plan) stream)
  (print-unreadable-object (plan stream :type t :identity t)
    (with-safe-io-syntax (:package :asdf)
      (pprint (summarize-plan plan) stream))))
|#

(defun make-action-map ()
  (make-hash-table :test 'equal))
(defun action-map (map action)
  (gethash action map))
(defun action-unmap (map action)
  (remhash action map))
(defun (setf action-map) (value map action)
  (setf (gethash action map) value))
(defun action-map-values (map)
  (table-values map))
(defun action-map-keys (map)
  (table-keys map))

(defun record-action-dependency (parent child parents children)
  (unless (action-map parents child)
    (setf (action-map parents child) (make-action-map)))
  (when parent
    (unless (action-map children parent)
      (setf (action-map children parent) (make-action-map)))
    (setf (action-map (action-map children parent) child) t)
    (setf (action-map (action-map parents child) parent) t)))

(defun parallel-plan-mark-as-done (plan operation component)
  ;; marks the action of operation on component as done in the deps hash-tables,
  ;; returns a list of new actions that are enabled by it being done.
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

(defmethod mark-as-done :after ((plan parallel-plan) (operation operation) (component component))
  (parallel-plan-mark-as-done plan operation component))

(defmethod record-dependency ((plan parallel-plan) (o operation) (c component))
  (let ((action (make-action o c))
        (parent (first (visiting-action-list *asdf-session*))))
    ;; Only record the dependency if it points to a parent in the current plan.
    (when (and parent (plan-action-index plan action))
      (record-action-dependency parent action (plan-parents plan) (plan-children plan)))))

(defmethod (setf action-status) :before
    (new-status (p parallel-plan) (o operation) (c component))
  (let ((action (make-action o c)))
    (unless (gethash action (visited-actions *asdf-session*)) ; already visited?
      (setf (gethash action (plan-action-indices p)) (fill-pointer (plan-all-actions p)))
      (vector-push-extend action (plan-all-actions p))
      (when (empty-p (action-map (plan-children p) action))
        (enqueue (plan-starting-points p) action)))))

(defun summarize-plan (plan)
  (with-slots (starting-points children) plan
    `((:starting-points
       ,(mapcar 'action-path (queue-contents starting-points)))
      (:dependencies
       ,(mapcar #'rest
                  (sort
                   (loop :for parent-node :being :the :hash-keys :in children
                         :using (:hash-value progeny)
                         :for parent = parent-node
                         :for (o . c) = parent
                         :collect `(,(plan-action-index plan parent)
                                    ,(action-path parent)
                                    ,(if (action-already-done-p plan o c) :- :+)
                                    ,@(loop :for child-node :being :the :hash-keys :in progeny
                                            :using (:hash-value v)
                                            :for child = child-node
                                            :when v :collect (action-path child))))
                   #'< :key #'first))))))

(defgeneric serialize-plan (plan))
(defmethod serialize-plan ((plan list)) plan)
(defmethod serialize-plan ((plan parallel-plan))
  (with-slots (all-actions) plan
    (loop :for action :in (plan-actions plan)
          :for (o . c) = action
          :when (status-need-p (action-status plan o c)) :collect action)))

(defgeneric check-invariants (object))

(defmethod check-invariants ((plan parallel-plan))
  ;; This *destructively* checks that the dependency tree model is coherent.
  (while-collecting (collect)
    (with-slots (starting-points parents children) plan
      (with-queue (action action-queue starting-points)
        (collect action)
        (destructuring-bind (operation . component) action
          (mark-as-done plan operation component)))
      (unless (empty-p children)
        (error "Cycle detected in the dependency graph:~%~S"
               (summarize-plan plan))))))

(defmethod make-plan :around (plan-class (o operation) (c component) &key &allow-other-keys)
  (let ((plan (call-next-method)))
    (when (typep plan 'parallel-plan)
      (warn "~S" (summarize-plan plan))
      ;; make a second plan and destructively check it
      (check-invariants (call-next-method)))
    plan))

(defmethod plan-actions ((plan parallel-plan))
  (coerce (plan-all-actions plan) 'list))

