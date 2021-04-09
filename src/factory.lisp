;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defun to-behaviour-instance (behaviour-object-or-class)
  "Transform BEHAVIOUR-OBJECT-OR-CLASS to an instance of the given behaviour."
  (cond
    ((typep behaviour-object-or-class 'behaviour)
     behaviour-object-or-class)
    ((find-class behaviour-object-or-class nil)
     (make-instance behaviour-object-or-class))))

(defun make-composite (composite-sub-class &rest sub-behaviours)
  "Make COMPOSITE-SUB-CLASS.
SUB-BEHAVIOURS is a list containing class symbols or instances of sub-classes of BEHAVIOUR."
  (make-instance composite-sub-class
		 :sub-behaviours (mapcar #'to-behaviour-instance sub-behaviours)))

(defun make-behaviour-sequence (&rest sub-behaviours)
  "Make a BEHAVIOUR-SEQUENCE.
SUB-BEHAVIOURS is a list containing class symbols or instances of sub-classes of BEHAVIOUR."
  (apply #'make-composite 'behaviour-sequence sub-behaviours))

(defun make-selector (&rest sub-behaviours)
  "Make a SELECTOR.
SUB-BEHAVIOURS is a list containing class symbols or instances of sub-classes of BEHAVIOUR."
  (apply #'make-composite 'selector sub-behaviours))

(defun make-active-selector (&rest sub-behaviours)
  "Make an ACTIVE-SELECTOR.
SUB-BEHAVIOURS is a list containing class symbols or instances of sub-classes of BEHAVIOUR."
  (apply #'make-composite 'active-selector sub-behaviours))

(defun make-random-selector (&rest sub-behaviours)
  "Make a RANDOM-SELECTOR.
SUB-BEHAVIOURS is a list containing class symbols or instances of sub-classes of BEHAVIOUR."
  (apply #'make-composite 'random-selector sub-behaviours))

(defbehaviour-condition failure nil
  "BEHAVIOUR that always returns :FAILURE."
  :failure)

(defun make-checked-action (precondition behaviour &optional condition-fail-behaviour)
  "TICK BEHAVIOUR's TASK only when PRECONDITION succeeds; optionally TICK CONDITION-FAIL-BEHAVIOUR when CONDITION fails."
  (make-behaviour-sequence
   (if condition-fail-behaviour
       (make-selector precondition
		      (make-behaviour-sequence condition-fail-behaviour
					       'failure))
       precondition)
   behaviour))

(defun make-goal (goal achieve-goal)
  "Continuously check whether GOAL has been reached, if it has not been reached TICK ACHIEVE_GOAL."
  (make-active-selector goal achieve-goal))
