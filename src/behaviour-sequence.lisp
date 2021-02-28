;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass behaviour-sequence (composite)
  ((name
    :initform "sequence")
   (display-symbol
    :initform ">>"))
  (:documentation "Execute a sequences of BEHAVIOURs.
Return :SUCCESS when all SUB-BEHAVIOUR return :SUCCESS; abort when the first SUB_BEHAVIOUR returns :FAILURE."))

(defmethod update (system (task composite-task) (behaviour behaviour-sequence) scheduler data-context)
  (with-slots (current-sub-task) task
    (if (and (not current-sub-task)
	     (not (next-sub-task system task scheduler data-context))) 
	:success
	(let ((sub-task-return-status (tick system current-sub-task scheduler data-context)))
	  (cond
	    ((eq sub-task-return-status :success)
	     (terminate-sub-task system task scheduler data-context)
	     (tick system task scheduler data-context))
	    ((eq sub-task-return-status :failure)
	     (terminate-sub-task system task scheduler data-context)
	     sub-task-return-status)
	    ((eq sub-task-return-status :running)
	     sub-task-return-status))))))

