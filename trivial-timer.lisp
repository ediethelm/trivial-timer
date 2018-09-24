(in-package :trivial-timer)

(defclass threadpool-thread ()
  ((queue-size :accessor queue-size
	       :initarg :queue-size
	       :type fixnum)
   (thread :accessor thread
	   :initarg :thread
	   :type bordeaux-threads:thread)
   (queue :accessor queue
	  :initarg :queue
	  :type chanl:bounded-channel)
   (queue-lock :accessor queue-lock
	       :initarg :queue-lock
	       :type bordeaux-threads:lock)))

(proclaim '(type (or bordeaux-threads:thread null) *timer-thread*))
(proclaim '(type (unsigned-byte 8) *thread-pool-size*))
(proclaim '(type list *thread-pool* *call-queue*))
(proclaim '(type bordeaux-threads:lock *call-queue-lock*))
(proclaim '(type (unsigned-byte 62) *ms-to-ticks* *ms-tolerance* *ticks-tolerance*))

(proclaim '(ftype (function (threadpool-thread) boolean) thread-free?))
(proclaim '(ftype (function () threadpool-thread) make-thread-pool-thread))
(proclaim '(ftype (function (threadpool-thread t) null) enqueue))
(proclaim '(ftype (function () null) timer-process))

(proclaim '(ftype (function () null) initialize-timer))
(proclaim '(ftype (function ((unsigned-byte 62) function) null) register-timer-call))
(proclaim '(ftype (function ((unsigned-byte 62) function) null) register-timer-recurring-call))

(defvar *timer-thread* nil)
(defvar *thread-pool-size* 4)
(defvar *thread-pool* nil)
(defvar *call-queue* nil)
(defvar *call-queue-lock* (bordeaux-threads:make-lock "call-queue"))
(defvar *ms-to-ticks* (/ 1000 internal-time-units-per-second))
(defvar *ms-tolerance* 10 "Time tolerance (+/- 10ms)")
(defvar *ticks-tolerance* (* *ms-to-ticks* *ms-tolerance*))

(defun thread-free? (thread)
  (declare (type ThreadPool-Thread thread))
  (the boolean (zerop (the fixnum (queue-size thread)))))


(defun make-thread-pool-thread ()
  (return-from make-thread-pool-thread
    (the threadpool-thread
	 (trivial-utilities:aprog1 
	     (make-instance 'threadpool-thread
			    :queue-size 0
			    :queue (make-instance 'chanl:bounded-channel :size 5)
			    :queue-lock (bordeaux-threads:make-lock))
	   (setf (thread it)
		 (bordeaux-threads:make-thread
		  #'(lambda ()
		      (loop
			 do
			   (let ((call (chanl:recv (queue it))))
			     (declare (type function call))
			     (unwind-protect (funcall call)
			       (let ((lock (queue-lock it)))
				 (bordeaux-threads:with-lock-held (lock)
				   (decf (the fixnum (queue-size it)))))))))
		  
		  :name "Thread pool thread"))))))


(defun enqueue (thread obj)
  (declare (type threadpool-thread thread))
  (bordeaux-threads:with-lock-held  ((queue-lock thread))
    (chanl:send (queue thread) obj :blockp nil)
    (incf (the fixnum (queue-size thread))))
  nil)


(defun timer-process ()
  (loop
     ;; Identify calls within +/- *ticks-tolerance* from now
     ;; @TODO QoS: Could use a sorted list to prioritize handling of calls by time
     do (let* ((upper (+ (get-internal-real-time) *ticks-tolerance*))
	       (calls
		;; @TODO What would be the consequence of NOT locking??
		(bordeaux-threads:with-lock-held (*call-queue-lock*)
		  (loop for call of-type list in *call-queue*
		     when (<= (the (unsigned-byte 62) (first call)) (the (unsigned-byte 62) upper))
		     collect call))))

	  (declare (type list calls))
	  (dolist (call calls)      
	    (let ((thread (loop
			     with thread-to-use
			     do (setf thread-to-use
				      (loop for th in *thread-pool*
					 if (thread-free? th)
					 do (return th)
					 else
					 do (bordeaux-threads:thread-yield)))
			     when thread-to-use
			     do (return thread-to-use))))

	      (if (null thread)
		  (log:warn "No thread in the threadpool is free to handle timer calls.")
		  (progn
		    (bordeaux-threads:with-lock-held (*call-queue-lock*)
		      (setf *call-queue* (delete call *call-queue*)))
		    
		    (enqueue thread (second call))

		    (let ((execution-time (get-internal-real-time)))
		      (declare (type (unsigned-byte 62) execution-time))
		      
		      (when (> 0 (- (coerce (+ (the (unsigned-byte 62) (first call)) *ticks-tolerance*) 'fixnum) execution-time))
			(log:warn "A call was started too late (~a ms)."
				  (/ (- (first call) execution-time)
				     *ms-to-ticks*)))))))))
       
       (sleep (/ *ms-tolerance* 1000))))


(defun initialize-timer ()
  ""
  (dotimes (n *thread-pool-size*)
    (push (make-thread-pool-thread) *thread-pool*))

  (setf *timer-thread* (bordeaux-threads:make-thread #'timer-process :name "Timer thread"))
  nil)

(defun stop-timer ()
  ""
  )
  
(defun register-timer-call (offset call)
  (declare (type (unsigned-byte 62) offset)
	   (type function call))
  
  (bordeaux-threads:with-lock-held (*call-queue-lock*)
    (push (list (+ (the (unsigned-byte 64) (* *ms-to-ticks* offset)) (get-internal-real-time)) call) *call-queue*))
  nil)

;; How to implement a recurring timer call?
;; Get an call ID to stop it later.
;; (cancel-timer-recurring-call (register-timer-recurring-call ...)) 

(defun register-timer-recurring-call (offset call)
  (declare (type (unsigned-byte 62) offset)
	   (type function call))
  (bordeaux-threads:with-lock-held (*call-queue-lock*)
    (push (list (+ (* *ms-to-ticks* offset) (get-internal-real-time))
		(lambda () (progn
			     ;;(register-timer-recurring-call offset call)
			     (funcall call))))
	  *call-queue*))
  nil)

