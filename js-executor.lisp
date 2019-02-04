(cl:in-package :hhgbot-augmented-assistant)

(defclass js-executor ()
  ((%inp :accessor work-queue :initform (make-instance 'chanl:unbounded-channel))))

(defparameter *js-executor* (make-instance 'js-executor))

(defun submit-js (executor js)
  (with-accessors ((work-queue work-queue)) executor
    (let ((promise (blackbird-base:make-promise :name "js-execution")))
      (chanl:send work-queue
<<<<<<< HEAD
                  (list promise js))
=======
		  (list promise js))
>>>>>>> github/master
      promise)))

(defmethod slacker:start-module ((event-pump event-pump) (exe js-executor))
  (declare (ignorable event-pump))
  (values exe
<<<<<<< HEAD
          (bt:make-thread
           (lambda ()
             (loop
               (multiple-value-bind (message message-p) (chanl:recv (work-queue exe))
                 (when message-p
                   (destructuring-bind (promise script) message
                     (handler-case
                         (blackbird-base:finish promise (cl-js:run-js script))
                       (t (c) (blackbird:signal-error promise c)))))
		             (sleep 0.4))))
	         :name "js-executor")))
=======
	  (bt:make-thread
	   (lambda ()
	     (loop
		(multiple-value-bind (message message-p) (chanl:recv (work-queue exe))
		  (when message-p
		    (destructuring-bind (promise script) message
		      (handler-case
			  (blackbird-base:finish promise (cl-js:run-js script))
			(t (c) (blackbird:signal-error promise c)))))
		  (sleep 0.4))))
	   :name "js-executor")))
>>>>>>> github/master
