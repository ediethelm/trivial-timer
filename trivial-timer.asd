;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(ql:quickload :trivial-asdf-extensions :silent t)

(defsystem :trivial-timer
  :name "trivial-timer"
  :description "Easy scheduling of tasks (functions)."
  :version "0.3.6"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-utilities
	       :log4cl
	       :bordeaux-threads
	       :chanl
	       :iterate)
  :in-order-to ((test-op (test-op :trivial-timer/test))
		(trivial-asdf-extensions:document-op (load-op :trivial-timer/document)))
  :components ((:file "package")
	       (:file "trivial-timer")))

(defsystem :trivial-timer/test
  :name "trivial-timer/test"
  :description "Unit Tests for the trivial-timer project."
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-timer
	       fiveam)
  :perform (test-op (o s) (uiop:symbol-call :fiveam  '#:run! :trivial-timer-tests))
  :components ((:file "test-trivial-timer")))

(defsystem :trivial-timer/document
  :name "trivial-timer/document"
  :description ""
  :version "0.3.5"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-timer
	       :mgl-pax)
  :perform (load-op (o s) (uiop:symbol-call :trivial-utilities '#:update-doc :trivial-timer "README.md"))
  :components ((:file "documentation")))

