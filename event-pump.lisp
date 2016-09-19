(in-package :slacker)

(defclass event-pump ()
  ((%ws-client :accessor ws-client :initarg :ws-client)
   (%waiting-pings :accessor waiting-pings :initform 0)
   (%modules :accessor modules :initform (make-hash-table))
   (%latest-id :accessor latest-id :initform 0)
   (%work-queue :accessor work-queue :initform (make-instance 'chanl:unbounded-channel))
   (%result-queue :accessor result-queue :initform (make-instance 'chanl:unbounded-channel))))

