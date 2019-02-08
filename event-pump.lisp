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

(defmethod result-queue ((obj event-pump))
  (result-queue (queue-pair obj)))

(defmethod work-queue ((obj event-pump))
  (work-queue (queue-pair obj)))

(defun clear-waiting-pings (event-pump)
  (setf (waiting-pings event-pump) 0))
