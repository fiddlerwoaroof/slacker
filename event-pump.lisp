(in-package :slacker)

(defclass queue-pair ()
  ((%work-queue :initarg :work-queue :reader work-queue :initform (make-instance 'chanl:unbounded-channel))
   (%result-queue :initarg :result-queue :reader result-queue :initform (make-instance 'chanl:unbounded-channel))))

(defclass event-pump ()
  ((%tick-pause :initform 0.01)
   (%running :accessor running :initform nil)
   (%finish-cb :reader finish-cb :writer fwoar.event-loop:register-finish-cb :initform nil)
   (%ws-client :accessor ws-client :initarg :ws-client :initform nil)
   (%waiting-pings :accessor waiting-pings :initform 0)
   (%modules :accessor modules :initform (make-hash-table))
   (%client-factory :reader client-factory :initarg :client-factory :initform (error "must provide a client factory"))
   (%latest-id :accessor latest-id :initform 0)
   (%queue-pair :accessor queue-pair :initarg :queue-pair :initform (make-instance 'queue-pair))))

(defgeneric start-module (client module)
  (:documentation "start a module"))
(defgeneric stop-module (client module)
  (:documentation "stop a module"))

(defmethod result-queue ((obj event-pump))
  (result-queue (queue-pair obj)))

(defmethod work-queue ((obj event-pump))
  (work-queue (queue-pair obj)))

(defun clear-waiting-pings (event-pump)
  (setf (waiting-pings event-pump) 0))

(defmethod fwoar.event-loop:prepare-loop ((event-pump event-pump))
  (declare (optimize (debug 3)))
  (let ((client (funcall (client-factory event-pump) event-pump)))
    #+nil (websocket-driver:start-connection client)))

(defmethod fwoar.event-loop:cleanup ((event-pump event-pump))
  (setf (running event-pump) nil)
  (do-hash-table (_ v (modules event-pump))
    (declare (ignore _))
    (stop-module event-pump v))
  (when (ws-client event-pump)
    (wsd:close-connection (ws-client event-pump))))

(let ((last-ping nil))
  (defun maybe-ping (event-pump)
    (let ((current-time  (get-universal-time)))
      (when (or (null last-ping)
                (< 15 (- current-time last-ping)))
        (setf last-ping current-time)
        (websocket-driver:send-ping (ws-client event-pump))))))


(defun handle-work-queue (event-pump)
  (multiple-value-bind (message message-p)
      (chanl:recv (work-queue event-pump)
                  :blockp nil)
    (when message-p
      (funcall message
               event-pump))))

(defmethod fwoar.event-loop:tick ((task event-pump))
  (handle-work-queue task)
  (maybe-ping task)
  (sleep (slot-value task '%tick-pause)))

(defmethod attach-module ((event-pump event-pump) module &rest args &key)
  (setf (gethash (make-keyword module)
                 (modules event-pump))
        (apply #'make-instance
               module
               args)))

(defgeneric get-module (module event-pump)
  (:documentation "Get one of the activated modules")
  (:method (module (event-pump event-pump))
    (gethash (make-keyword module)
             (modules event-pump))))

(defun stop-slacker (event-pump)
  (funcall (finish-cb event-pump)))

(defun throttle-continue (num)
  (let ((continue-count 0)
        (now (local-time:now)))
    (lambda (&optional condition)
      (format t "~&~s ~s~%" now continue-count)
      (when (< continue-count num)
        (let ((new-now (local-time:now)))
          (if (< (* 1000 (local-time:timestamp-difference new-now now))
                 5)
              (incf continue-count)
              (setf now new-now
                    continue-count 0))
          (continue condition))))))

(defun test-event-pump ()
  (let ((the-event-pump (make-instance 'event-pump :client-factory 'identity)))
    (chanl:send (work-queue the-event-pump)
                (lambda (a)
                  (error "test")))
    (chanl:send (work-queue the-event-pump)
                (lambda (a)
                  (format t "This should happen ~s~%" a)))
    (bt:make-thread (lambda ()
                      (sleep 5)
                      (funcall (finish-cb the-event-pump))))
    (handler-bind ((serious-condition #'continue))
      (fwoar.event-loop:run-loop the-event-pump))))
