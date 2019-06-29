(defpackage :slacker.postmodern-store
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export #:postmodern-store))
(in-package :slacker.postmodern-store)

(defclass postmodern-store ()
  ((%connection :reader connection)
   (%connection-spec :reader connection-spec :initarg :postgres-connection-spec)))

(defmethod shared-initialize :after ((instance postmodern-store) slot-names &key postgres-connection-spec)
  (setf (slot-value instance '%connection)
	(apply 'postmodern:connect postgres-connection-spec)))

(defun save-slack-message (value)
  (pomo:query "INSERT INTO slack_messages_raw (message) VALUES ($1) RETURNING id,slack_ts,message_text;"
              value :rows))

(defmethod slacker:handle-message :before
    (type (event-pump postmodern-store) ts channel message)
  (declare (ignore type ts channel))
  (let ((pomo:*database* (connection event-pump)))
    (save-slack-message
     (with-output-to-string (s)
       (yason:encode message s))))
  (values))
