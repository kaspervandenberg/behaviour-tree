;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass behaviour ()
  ((id
    :initarg :id
    :initform (uuid:make-v4-uuid)
    :reader id
    :documentation
    "UUID identifier.
Can be used to reference this BEHAVIOUR.  The BEHAVIOUR can be found via FIND-BEHAVIOUR-BY-ID.")
   (name
    :initarg :name
    :reader name
    :documentation
    "Human readable short (single word) description of the BEHAVIOUR.")
   (display-symbol
    :initarg :sym
    :initform "λ"
    :documentation
    "Short (few characters) symbol to indicate the type of node to a human."))
  (:documentation
   "Behaviours from the nodes in a behaviour-tree, which is a control structure to script game-AI."))

(defun find-behaviour-by-id (id behaviour-list)
  (car
   (member id behaviour-list
	   :key #'id
	   :test #'uuid:uuid=)))

(defmethod make-load-form ((obj behaviour) &optional environment)
  (make-load-form-saving-slots obj :environment environment))

(defmethod print-object ((obj behaviour) stream)
  (print-behaviour-tree obj stream 0))

(defmethod print-behaviour-tree ((obj behaviour) stream indent)
  (with-slots (id name display-symbol) obj
    (format stream
	    "~&~v,0T~5A ~A (~A ~a)"
	    (* 4 indent)
	    display-symbol
	    name
	    (type-of obj)
	    id)))
