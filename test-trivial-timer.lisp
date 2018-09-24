(in-package :trivial-timer)

(5am:def-suite :trivial-timer-tests :description "Trivial Timer tests")
(5am:in-suite :trivial-timer-tests)

(defparameter *messured-delay* nil)
(defun timer-callback (registered-time delay &aux (diff (- (get-internal-real-time) registered-time)))
  (log:info "I was called by the timer ~a ms after registration. Expected was ~a ms." diff delay)
  (setf *messured-delay* diff))

(5am:test register-timer-call
  (log:config :sane2 :this-console)
  (setf *messured-delay* nil)
  (let ((now (get-internal-real-time))
	(delay 1000))
    (register-timer-call delay #'(lambda () (timer-callback now delay)))

    (sleep 2)

    (log:info *messured-delay*)
    (5am:is (< (- delay *ms-tolerance*) *messured-delay* (+ delay *ms-tolerance*)))
  )
