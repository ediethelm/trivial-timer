;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-timer
  :name "trivial-timer"
  :description "Easy scheduling of tasks (functions)."
  :version "0.3.5"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-utilities
	       :log4cl
	       :bordeaux-threads
	       :chanl
	       :iterate
	       :mgl-pax)
  :in-order-to ((test-op (test-op :trivial-timer/test)))
  :components ((:file "package")
	       (:file "trivial-timer")
	       (:file "documentation")))

(defsystem :trivial-timer/test
  :name "trivial-timer/test"
  :description "Unit Tests for the trivial-timer project."
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-timer fiveam)
  :perform (test-op (o s) (uiop:symbol-call :fiveam  '#:run! :trivial-timer-tests))
  :components ((:file "test-trivial-timer")))
