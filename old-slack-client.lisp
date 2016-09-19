;; Classes
(defclass slack-client ()
  ((self :reader self :initarg :self)
   (url :reader url :initarg :url)
   (users :accessor users :initarg :users :initform (make-hash-table :test 'equal))
   (slack-info :reader slack-info :initarg :slack-info)
   (work-mailbox :reader work-mailbox :initform (make-instance 'chanl:bounded-channel :size 10))
   (name :reader name)
   (waiting-pings :accessor waiting-pings :initform 0)
   (latest-id :accessor latest-id :initform 0)
   (message-id :accessor message-id :initform 0)
   (ws-client :reader ws-client :initarg :ws-client)))

(defmethod initialize-instance :after ((client slack-client) &rest r)
  (declare (ignore r))
  (let ((self (self client)))
    (setf (slot-value client 'name)
          (gethash "name" self))))

(defmethod handle-message ((type (eql :error)) data)
  (format t "~&~s~%" (hash-table-alist (gethash "error" data))))

