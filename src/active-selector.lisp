;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass active-selector (composite)
  ((name
    :initform "active-selector")
   (display-symbol
    :initform "⎇↻"))
  (:documentation "Select the first :SUCCESSful SUB-BEHAVIOUR.

Actively checking each condition every TICK."))

(defclass active-selector-task (task)
  ((sub-tasks
    :initarg :sub-tasks
    :reader sub-tasks
    :documentation "List of TASKs to execute.  If an Element is NIL, the corresponding SUB-BEHAVIOUR should be INITIALISEd."))
  (:documentation "Per execution state of running ACTIVE-SELECTOR."))

(defmethod list-tasks-or-behaviours ((obj active-selector-task))
  (mapcar #'(lambda (x y) (or x y))
	  (sub-tasks obj)
	  (sub-behaviours (behaviour obj))))

(defmethod print-behaviour-tree :after ((obj active-selector-task) stream indent)
  (mapc #'(lambda (x) (print-behaviour-tree x stream (1+ indent)))
	(list-tasks-or-behaviours obj)))

(defmethod initialise (system (behaviour active-selector) scheduler data-context)
  (make-instance 'active-selector-task
		 :behaviour behaviour
		 :sub-tasks (mapcar #'(lambda (x) (declare (ignore x)) nil) (sub-behaviours behaviour))))

(defun tick-sub-behaviour (system remaining-sub-tasks remaining-sub-behaviours scheduler data-context)
  (if (not (car remaining-sub-tasks))
      (setf (car remaining-sub-tasks)
	    (initialise system (car remaining-sub-behaviours) scheduler data-context)))
  (let ((result (tick system (car remaining-sub-tasks) scheduler data-context)))
    (if (or (eq result :success) (eq result :failure))
	(progn
	  (terminate system (car remaining-sub-tasks) scheduler data-context)
	  (setf (car remaining-sub-tasks) nil)))
    (if (and (eq result :failure)
	     (cdr remaining-sub-tasks))
	(tick-sub-behaviour system (cdr remaining-sub-tasks) (cdr remaining-sub-behaviours) scheduler data-context)
	result)))

(defmethod update (system (task active-selector-task) (behaviour active-selector) scheduler data-context)
  (tick-sub-behaviour system (sub-tasks task) (sub-behaviours behaviour) scheduler data-context))

