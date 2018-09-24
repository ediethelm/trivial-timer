(mgl-pax:define-package #:trivial-timer
  (:documentation "")
  (:use #:common-lisp #:mgl-pax)
  (:export #:initialize-timer
	   #:stop-timer
	   #:register-timer-call
	   #:register-timer-recurring-call))

