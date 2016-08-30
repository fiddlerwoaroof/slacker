(in-package :slacker)

(defclass event-pump ()
  ((%ws-client :accessor ws-client :initarg :ws-client)
   (%waiting-pings :accessor waiting-pings :initform 0)
   (%modules :accessor modules :initform (make-hash-table))
   (%latest-id :accessor latest-id :initform 0)
   (%work-queue :accessor work-queue :initform (make-instance 'chanl:unbounded-channel))
   (%result-queue :accessor result-queue :initform (make-instance 'chanl:unbounded-channel))))

(defmethod attach-module ((event-pump event-pump) module &rest args &key)
  (setf (gethash (make-keyword module)
		 (modules event-pump))
	(apply #'make-instance
	       module
	       args)))

(defvar *api-token*) 

(defun make-client (event-pump)
  (flet ((get-ws-url (slack-response)
	   (gethash "url" slack-response)))
    (fw.lu:let-each (:be slack-data)
      (format nil "https://slack.com/api/rtm.start?token=~a" *api-token*)
      (drakma:http-request slack-data :want-stream t)
      (yason:parse slack-data)

      (let* ((url (get-ws-url slack-data))
	     (self (gethash "self" slack-data))
	     (users (gethash "users" slack-data))
	     (client (wsd:make-client url)))

	(declare (ignorable self users))
	(wsd:on :message client
		(lambda (message)
		  (chanl:send (result-queue event-pump)
			      message)))
	client))))

(defgeneric send-message (client type &key)
  (:documentation "Send a slack message")
  (:method :around ((client event-pump) _type &key)
    (declare (ignorable client _type))
    (let ((result (call-next-method)))
	(values result
		(wsd:send (ws-client client)
			(with-output-to-string (s)
			    (yason:encode result s)))))))


(defgeneric start-module (client module)
  (:documentation "start a module"))

(defun start-client (&key modules)
 (let* ((event-pump (make-instance 'event-pump))
	(client (make-client event-pump)))
   (setf (ws-client event-pump)
	 client)
   (values event-pump
	   (bt:make-thread 
	    (lambda ()
	      (loop for (module . args) in modules
		 do (start-module event-pump
				  (apply #'attach-module
					 event-pump module args)))
	      (as:with-event-loop () 
		(websocket-driver:start-connection client)

		(as:with-interval (15)
		  (send-message event-pump :ping))

		(as:with-interval (0.01)
		  (multiple-value-bind (message message-p)
		      (chanl:recv (work-queue event-pump)
				  :blockp nil)
		    (when message-p
		      (format t "Got a message")
		      (funcall message
			       event-pump)))
		  :event-cb (lambda (ev)
			      (format t "~&EVENT: ~a~%" ev)))))
	    :name "Event Server"))))

(defmethod get-event-nonblocking ((event-pump event-pump) &key (object-as :hash-table))
  (multiple-value-bind (message message-p) (chanl:recv (result-queue event-pump) :blockp nil)
    (values (when message-p
	      (yason:parse message :object-as object-as))
	    message-p)))

(defmethod get-event ((event-pump event-pump) &key (object-as :hash-table))
  (multiple-value-bind (message message-p) (chanl:recv (result-queue event-pump))
    (values (when message-p
	      (yason:parse message :object-as object-as))
	    message-p)))

(defun event-loop (event-pump)
  (loop with message with message-p
     do (multiple-value-setq (message message-p) (get-event event-pump))
     when message-p do
       (let ((type (gethash "type" message))
	     (reply (gethash "reply_to" message)))
	 (cond (type (string-case (gethash "type" message)
		       ("message" (format t "~&MSG: <~a/~a> ~a~%"
					  (gethash "channel" message)
					  (gethash "user" message)
					  (gethash "text" message))
				  (when-let* ((msg (gethash "text" message))
					      (parsed-message (tokens msg))
					      (ts (gethash "ts" message))
					      (channel (gethash "channel" message))) 
				    (when (eql #\; (elt msg 0))
				      (handle-command event-pump ts channel
						      (car parsed-message)
						      (cdr parsed-message)))))
		       (t (fresh-line)
			  (yason:encode message)
			  (terpri))))
	       (reply )))
       do (sleep 0.01)))

(defun coordinate-threads ()
  (let* ((event-pump (start-client :modules '((hhgbot-augmented-assistant::js-executor)))))
    (bt:make-thread (lambda ()  (event-loop event-pump))
		    :name "Event Loop") 
    event-pump))

(defparameter *command-table* (make-hash-table :test 'equal))

(defun quote-output (str)
  (with-output-to-string (s)
    (format s "```~%~a```~%" str)))

(defmacro in-wq ((client-sym) &body body)
  `(let ((promise (blackbird-base:make-promise)))
     (values promise
	     (chanl:send (work-queue ,client-sym)
			 (lambda (,client-sym)
			   (declare (ignorable ,client-sym))
			   (let ((result (progn ,@body)))
			     (blackbird-base:finish promise result)
			     result))))))

(define-modify-macro aconsf (key datum)
  (lambda (alist key datum)
    (acons key datum alist)))

(defmethod send-message ((client event-pump) (type (eql :ping)) &key data)
  (let* ((id (incf (latest-id client)))
         (message `(("id" . ,id)
                    ("type" . "ping"))))

    (when data
      (aconsf message "data" data))

    (incf (waiting-pings client))
    (alist-hash-table message
                      :test 'equal)))

(defmethod send-message ((client event-pump) (type (eql :message)) &key channel data)
  (let* ((id (incf (latest-id client)))
	 (message `(("id" . ,id)
		    ("type" . "message")
		    ("channel" . ,channel)
		    ("text" . ,data))))
    (alist-hash-table message
		      :test 'equal)))

(defun queue-message (event-pump channel message &key quote)
  (let ((message (if quote (quote-output message)
		     message)))
    (in-wq (event-pump)
      (send-message event-pump :message
		    :channel channel
		    :data message))))

(define-condition command-error () ())
(define-condition unsupported-args (command-error) ())

(defmacro define-command (name (event-pump ts channel &rest args) &body body)
  (let* ((command-sym (intern (string-upcase name)))
	 (has-rest (position '&rest args))
	 (rest-sym (gensym "rest"))
	 (args (if has-rest
		   args
		   (append args `(&rest ,rest-sym)))))
    `(progn
       (defun ,command-sym (,event-pump ,ts ,channel ,@args)
	 (declare (ignorable event-pump ts ,@(when (not has-rest) `(,rest-sym))))
	   ,@body)
       (setf (gethash ,name *command-table*) (function ,command-sym)))))

(defun handle-command (event-pump ts channel command args)
  (declare (ignorable args))
  (let* ((command (subseq (plump:decode-entities command) 1))
	 (handler (gethash command *command-table*)))
    (print (hash-table-alist *command-table*))
    (terpri)
    (print command)
    (if handler
	(apply handler event-pump ts channel args)
	(queue-message event-pump channel (concat "I don't underand the command `" command "`.")))))

(defun edit-message (ts channel text)
  (babel:octets-to-string
   (drakma:http-request "https://slack.com/api/chat.update"
			:method :post
			:content (concat "token=" *api-token*
					 "&channel=" channel
					 "&ts=" ts
					 "&text=" text))))

(defmacro with-output-to-message ((stream event-pump channel &key quote) &body body)
  (once-only (event-pump channel quote)
    `(queue-message ,event-pump ,channel
		    (with-output-to-string (,stream)
		      ,@body)
		    :quote ,quote)))

(define-command "help" (event-pump ts channel)
  (let ((*print-right-margin* (max (or *print-right-margin* 0)
				   80)))
    (with-output-to-message (s event-pump channel)
      (format s "I understand these commands:~%~{`~a`~^ ~}"
	      (hash-table-keys *command-table*))
      :quote t)))


(defparameter *id* 0)
(defun make-message (data channel)
  (incf *id*)
  (with-output-to-string (s)
    (yason:encode
     (alist-hash-table
      `(("id" . ,*id*)
	("type" . "message")
	("channel" . ,channel)
	("text" . ,data)))
     s)))

