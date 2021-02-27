;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass action (behaviour)
  ((display-symbol
    :initform "!"))
  (:documentation "Leaf BEHAVIOUR for executing an action and affecting the world."))

(defun make-class-slots (slot-list)
  (mapcar
   #'(lambda (x)
       (cond
	 ((symbolp x)
	  x)
	 ((listp x)
	  (list (car x)
		:initform (cadr x)))))
   slot-list))

(defmacro defaction (name task-slot-list &body update-body)
  "Create a simple ACTION named NAME and which executes UPDATE-BODY.
UPDATE-BODY should return one of :SUCCES, :FAILURE, or :RUNNING"
  (let ((task-class-sym (gensym (format nil "~A-TASK-" name))))
    `(progn
      (defclass ,name (action)
	((name
	  :initform ,(format nil "~A" name)))
	(:documentation ,(format nil "~A BEHAVIOUR (automatically generated)" name)))
      ,(if task-slot-list
	   `(defclass ,task-class-sym (task)
	     ,(make-class-slots task-slot-list)
	     (:documentation ,(format nil "Execution instance specific data for BEHAVIOUR ~a (automatically generated)" name))))
      ,(if task-slot-list
	   `(defmethod initialise (system (behaviour ,name) scheduler data-context)
	     (make-instance (quote ,task-class-sym) :behaviour behaviour)))
      (defmethod update (system ,(if task-slot-list `(task ,task-class-sym) '(task task)) (behaviour ,name) scheduler data-context)
	,@update-body))))

