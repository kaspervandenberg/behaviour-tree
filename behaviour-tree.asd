;; © Kasper van den Berg, 2021

(defsystem "behaviour-tree"
  :version "0.1.0"
  :author "Kasper van den Berg <kasper@kaspervandenberg.net>"
  :licence "MIT"
  :depends-on (:uuid)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "behaviour")
			     (:file "task")
			     (:file "behaviour-execution"))))
  :description "Event-driven behaviour tree extended with probabilistic action selection.")
