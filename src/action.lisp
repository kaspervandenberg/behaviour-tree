;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass action (behaviour)
  ((display-symbol
    :initform "!"))
  (:documentation "Leaf BEHAVIOUR for executing an action and affecting the world."))

(defclass behaviour-condition (behaviour)
  ((display-symbol
    :initform "?"))
  (:documentation "Leaf BEHAVIOUR for testing a specific situation/predicate/condition."))

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

(defun to-task-status (f)
  (let ((result (funcall f)))
    (cond
      ((eq result :success) result)
      ((eq result :failure) result)
      ((eq result :running) result)
      (result :success)
      (t :failure))))

(defun define-behaviour-leaf (parent-class name task-slot-list update-body)
  "Create a PARENT-CLASS BEHAVIOUR leaf which executes UPDATE-BODY"
  (let ((task-class-sym (gensym (format nil "~A-TASK-" name)))
	(class-name-string (format nil "~A" name))
	(class-doc (format nil "~A ~A (automatically generated)" parent-class name))
	(task-doc (format nil "Execution instance specific data for ~A ~A (automatically generated)" parent-class name)))
    `(progn
      (defclass ,name (,parent-class)
	((name
	  :initform ,class-name-string))
	(:documentation ,class-doc))
      ,(if task-slot-list
	   `(defclass ,task-class-sym (task)
	     ,(make-class-slots task-slot-list)
	     (:documentation ,task-doc)))
      ,(if task-slot-list
	   `(defmethod initialise (system (behaviour ,name) scheduler data-context)
	     (make-instance (quote ,task-class-sym) :behaviour behaviour)))
      (defmethod update (system (task ,(if task-slot-list task-class-sym 'task)) (behaviour ,name) scheduler data-context)
	(to-task-status (function (lambda () ,@update-body)))))))

(defmacro defaction (name task-slot-list &body update-body)
  "Create a simple ACTION named NAME and which executes UPDATE-BODY."
  (define-behaviour-leaf 'action name task-slot-list update-body))

(defmacro defbehaviour-condition (name task-slot-list &body update-body)
  "Create a simple BEHAVIOUR-CONDITION named NAME and which executes UPDATE-BODY."
  (define-behaviour-leaf 'behaviour-condition name task-slot-list update-body))


