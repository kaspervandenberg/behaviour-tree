;; © Kasper van den Berg, 2021
(in-package net.kaspervandenberg.behaviour-tree)

(defclass random-selector (composite)
  ((name
    :initform "random-selector")
   (display-symbol
    :initform "⎇🎲")))

(defun random-element-from-list (elements)
  "Select an element from ELEMENTS at random."
  (nth (random (length elements))
       elements))

(defmethod update (system (task composite-task) (behaviour random-selector) scheduler data-context)
  (selector-update #'random-element-from-list system task behaviour scheduler data-context))
