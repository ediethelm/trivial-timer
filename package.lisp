(uiop:define-package #:trivial-timer
  (:documentation "Trivial Timer allows for easy scheduling of tasks (functions). The default tolerance is +- 10ms as defined by *\*ms-tolerance\**.")
  (:use #:common-lisp)
  (:export #:initialize-timer
	   #:shutdown-timer
	   #:register-timer-call
	   #:register-timer-recurring-call
	   #:cancel-timer-call
	   #:*ms-tolerance*))

