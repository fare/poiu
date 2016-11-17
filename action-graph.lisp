(uiop:define-package :poiu/action-graph
  (:use :uiop/common-lisp :uiop :poiu/queue
        :asdf/upgrade
        :asdf/component :asdf/system :asdf/find-system :asdf/find-component
        :asdf/operation :asdf/action :asdf/plan)
  (:export #:parallel-plan #:*parallel-plan-deterministic-p*
           #:summarize-plan #:serialize-plan
           #:starting-points #:children #:parents ;; slot names -- FIXME, have clients use accessors
           #:plan-starting-points #:plan-children #:plan-parents
           #:reify-action #:mark-as-done #:plan-deterministic-p
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
   (deterministic-p
    :initform *parallel-plan-deterministic-p* :initarg :deterministic-p
    :type boolean :reader plan-deterministic-p
    :documentation "is this plan supposed to be executed in deterministic way?")))

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
  ;; This *destructively* checks that the dependency tree model is coherent.
  (while-collecting (collect)
    (with-slots (starting-points parents children) plan
      (with-queue (action action-queue starting-points)
        (collect action)
        (destructuring-bind (operation . component) action
          (mark-as-done plan operation component)))
      (unless (empty-p children)
        (error "Cycle detected in the dependency graph:~%~S"
               plan)))))

(defmethod make-plan :around (plan-class (o operation) (c component) &key &allow-other-keys)
  (let ((plan (call-next-method)))
    (when (typep plan 'parallel-plan)
      ;; make a plan once already and destructively check it
      (check-invariants (call-next-method)))
    plan))

(defmethod plan-actions ((plan parallel-plan))
  (coerce (plan-all-actions plan) 'list))

