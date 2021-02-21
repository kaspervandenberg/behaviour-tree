;; © Kasper van den Berg, 2021
(defpackage net.kaspervandenberg.behaviour-tree
  (:use :cl)
  (:export behaviour
	   task
	   initialise
	   tick
	   terminate))
