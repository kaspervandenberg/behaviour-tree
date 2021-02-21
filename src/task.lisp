;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass task ()
  ((id
    :initarg :id
    :initform (uuid:make-v4-uuid)
    :reader id
    :documentation
    "UUID identifier.
Can be used to reference this TASK.  The TASK can bve-found via FIND-TASK-BY-ID.")
   (behaviour
    :initarg :behaviour
    :reader behaviour
    :documentation
    "The BEHAVIOUR of which this TASK is an execution instance."))
  (:documentation "Per execution state of a BEHAVIOUR."))

(defun find-task-by-id (id task-list)
  (car
   (member id task-list
	   :key #'id
	   :test #'uuid:uuid=)))

(defun remove-tasks-not-of-behaviour (behaviour behaviour-list)
  (let ((id-to-find (id behaviour)))
    (remove-if-not
     #'(lambda (x)
	 (uuid:uuid=
	  id-to-find
	  (id x)))
     behaviour-list
     :key #'behaviour)))

(defmethod make-load-form ((obj task) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defmethod print-object ((obj task) stream)
  (print-behaviour-tree obj stream 0))

(defmethod print-behaviour-tree ((obj task) stream indent)
  (with-slots ((task-id id) behaviour) obj
    (with-slots ((behaviour-id id) name display-symbol) behaviour
      (format stream
	      "~&~v,0T*~4A ~A (~A ~a) (~A ~a)"
	      (* 4 indent)
	      display-symbol
	      name
	      (type-of behaviour) behaviour-id
	      (type-of obj) task-id))))
