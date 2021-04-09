;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass selector (composite)
  ((name
    :initform "selector")
   (display-symbol
    :initform "⎇"))
  (:documentation "Select the first :SUCCESSful SUB-BEHAVIOUR."))

(defun selector-update (task-selector system task behaviour scheduler data-context)
  (declare (ignore behaviour))
  (with-slots (current-sub-task) task
    (if (and (not current-sub-task)
	     (not (select-next-sub-task task-selector system task scheduler data-context)))
	:failure
	(let ((sub-task-return-status (tick system current-sub-task scheduler data-context)))
	  (cond
	    ((eq sub-task-return-status :success)
	     (terminate-sub-task system task scheduler data-context)
	     sub-task-return-status)
	    ((eq sub-task-return-status :failure)
	     (terminate-sub-task system task scheduler data-context)
	     (tick system task scheduler data-context))
	    ((eq sub-task-return-status :running)
	     sub-task-return-status))))))

(defmethod update (system (task composite-task) (behaviour selector) scheduler data-context)
  (selector-update #'car system task behaviour scheduler data-context))
