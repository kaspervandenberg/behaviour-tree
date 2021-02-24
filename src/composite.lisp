;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass composite (behaviour)
  ((sub-behaviours
    :initarg :sub-behaviours
    :reader sub-behaviours
    :documentation "List of SUB-BEHAVIOURS that this COMPOSITE contains."))
  (:documentation "Compose a BEHAVIOUR from SUB-BEHAVIOURS."))

(defclass composite-task (task)
  ((remaining-behaviours
    :initarg :remaining-behaviours
    :documentation "List of sub-BEHAVIOURs that remain to be executed.
When a sub-BEHAVIOUR is INITIALSEd to a TASK, the BEHAVIOUR is discarded from REMAINING-BEHAVIOURS.")
   (current-sub-task
    :initform nil
    :reader current-sub-task
    :documentation "TASK of the SUB-BEHAVIOUR that is currently running."))
  (:documentation "Per execution status of a COMPOSITE-BEHAVIOUR"))

(defmethod initialize-instance :after ((obj composite-task) &key)
  (unless (slot-boundp obj 'remaining-behaviours)
    (with-slots (behaviour remaining-behaviours) obj
      (setf remaining-behaviours (sub-behaviours behaviour)))))

(defmethod print-behaviour-tree :after ((obj composite) stream indent)
  (mapc #'(lambda (x) (print-behaviour-tree x stream (1+ indent)))
	(sub-behaviours obj)))

(defmethod print-behaviour-tree :after ((obj composite-task) stream indent)
  (with-slots (current-sub-task remaining-behaviours) obj
    (if current-sub-task
	(print-behaviour-tree current-sub-task stream (1+ indent)))
    (mapc #'(lambda (x) (print-behaviour-tree x stream (1+ indent)))
	  remaining-behaviours)))

(defmethod initialise (system (behaviour composite) scheduler data-context)
  (make-instance 'composite-task
		 :behaviour behaviour
		 :remaining-behaviours (sub-behaviours behaviour)))

(defmethod terminate :before (system (task composite-task) scheduler data-context)
  (terminate-sub-task system task scheduler data-context)
  (setf (slot-value task 'remaining-behaviours) nil))

(defun terminate-sub-task (system task scheduler data-context)
  (if (current-sub-task task)
      (progn
	(terminate system (current-sub-task task) scheduler data-context)
	(setf (slot-value task 'current-sub-task) nil))))

(defun next-sub-task (system task scheduler data-context)
  (terminate-sub-task system task scheduler data-context)
  (with-slots (remaining-behaviours current-sub-task) task
    (let ((behaviour (car remaining-behaviours)))
      (when behaviour
	(setf remaining-behaviours (cdr remaining-behaviours))
	(setf current-sub-task (initialise system behaviour scheduler data-context)))
      current-sub-task)))
