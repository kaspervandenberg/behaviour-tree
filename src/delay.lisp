;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass post-delay (behaviour)
  ((inner-behaviour
    :initarg :inner-behaviour
    :reader inner-behaviour
    :documentation "BEHAVIOUR of ehich to delay its :SUCCES or :FAILURE.")
   (n-ticks-delay
    :initarg :delay
    :initform 1
    :reader get-delay
    :documentation "Number of TICKs to return :RUNNING instead of the result of INNER-BEHAVIOUR.")
   (name
    :initform "delay (post)"))
  (:documentation "Delay returning the result of INNER-BEHAVIOUR; INNER-BEHAVIOUR is executed immediatly.
POST-DELAY gives the player the opportiunity to react to the effect of INNER-BEHAVIOUR."))

(defclass post-delay-task (task)
  ((inner-task
    :initarg :inner-task
    :reader inner-task
    :documentation "TASK that is being executed.")
   (inner-task-result
    :initform nil
    :reader inner-task-result
    :documentation "What should POST-DELAY-TASK return after delaying?")
   (n-ticks-delay-remaining
    :initarg :delay
    :reader n-ticks-delay-remaining
    :documentation "For how many TICKs should this return :RUNNING?"))
  (:documentation "Per execution state of POST-DELAY."))

(defmethod initialise (system (behaviour post-delay) scheduler data-context)
  (make-instance 'post-delay-task
		 :behaviour behaviour
		 :inner-task (initialise system (inner-behaviour behaviour) system data-context)
		 :delay (get-delay behaviour)))

(defun terminate-inner-task (system task scheduler data-context)
  (with-slots (inner-task) task
    (if inner-task
	(progn
	  (terminate system inner-task scheduler data-context)
	  (setf inner-task nil)))))

(defmethod terminate (system (task post-delay-task) scheduler data-context)
  (terminate-inner-task system task scheduler data-context))

(defmethod update (system (task post-delay-task) (behaviour post-delay) scheduler data-context)
  (with-slots (inner-task inner-task-result n-ticks-delay-remaining) task
    (cond
      ((and (not inner-task) (<= n-ticks-delay-remaining 0))
       inner-task-result)
      ((and (not inner-task) (> n-ticks-delay-remaining ))
       (decf n-ticks-delay-remaining)
       :running)
      (inner-task
       (setf inner-task-result (tick system inner-task scheduler data-context))
       (when (or (eq inner-task-result :success) (eq inner-task-result :failure))
	 (terminate-inner-task system task scheduler data-context)
	 (tick system task scheduler data-context)
	 :running)))))
