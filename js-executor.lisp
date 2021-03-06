(cl:in-package :hhgbot-augmented-assistant)

(defclass js-executor ()
  ((%thread :accessor thread :initform nil)
   (%inp :accessor work-queue :initform (make-instance 'chanl:unbounded-channel))))

(defparameter *js-executor* (make-instance 'js-executor))

(defun submit-js (executor js)
  (with-accessors ((work-queue work-queue)) executor
    (let ((promise (blackbird-base:make-promise :name "js-execution")))
      (chanl:send work-queue
		              (list promise js))
      promise)))

(defmethod slacker:start-module ((event-pump event-pump) (exe js-executor))
  (declare (ignorable event-pump))
  
  (values exe
          (setf (thread exe)
                (bt:make-thread
                 (lambda ()
                   (loop
                     (multiple-value-bind (message message-p) (chanl:recv (work-queue exe))
                       (when message-p
                         (destructuring-bind (promise script) message
                           (handler-case
                               (blackbird-base:finish promise
                                                      (cl-js:run-js script))
                             (serious-condition (c) (blackbird:signal-error promise c)))))
		                   (sleep 0.4))))
	               :name "js-executor"))))

(defmethod slacker:stop-module ((event-pump event-pump) (exe js-executor))
  (declare (ignorable event-pump))
  (with-accessors ((thread thread)) exe
    (format t "~&executor: ~s~&" thread)
    (when thread
      (bt:destroy-thread thread))))
