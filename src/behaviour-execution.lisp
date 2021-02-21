;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defgeneric tick (system task scheduler data-context)
  (:documentation "Let TASK do one time-step of work.
TICK should return one of :SUCCESS, :FAILURE, or :RUNNING."))

(defgeneric update (system task behaviour scheduler data-context)
  (:documentation "Implementation of TICK; do one time-step of work.
UPDATE should return one of :SUCCES, :FAILURE, or :RUNNING."))

(defgeneric initialise (system behaviour scheduler data-context)
  (:documentation "Possibility to run code when the BEHAVIOUR starts executing.
INITIALISE should return a TASK to run for this activation instance of the BEHAVIOUR."))

(defgeneric terminate (system task scheduler data-context)
  (:documentation "Possibility to run code when the TASK stops executing."))

(defmethod tick (system (task task) scheduler data-context)
  (update system task (behaviour task) scheduler data-context))

(defmethod update (system (task task) (behaviour behaviour) scheduler data-context)
  :failure)

(defmethod initialise (system (behaviour behaviour) scheduler data-context)
  (make-instance 'task :behaviour behaviour))

(defmethod terminate (system (task task) scheduler data-context)
  nil)
