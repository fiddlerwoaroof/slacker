(in-package :slacker)

(defvar *api-token*) 

(defun make-client (event-pump)
  (flet ((get-ws-url (slack-response)
           (gethash "url" slack-response)))
    (fw.lu:let-each (:be slack-data)
      (format nil "https://slack.com/api/rtm.connect?token=~a" *api-token*)
      (drakma:http-request slack-data :want-stream t)
      (yason:parse slack-data)

      (let* ((url (get-ws-url slack-data))
             (client (wsd:make-client url)))
        (setf (ws-client event-pump)
              client)
        (wsd:on :message client
                (lambda (message)
                  (chanl:send (result-queue event-pump)
                              message)))
        (wsd:on :close client
                (lambda (&key code reason)
                  (if (running event-pump)
                      (progn
                        (format t "~&Closed with code: ~a for reason ~a. Restarting in 2 seconds~%"
                                code reason)
                        (sleep 2)
                        (format t "~& ... restarting~%")
                        (make-client event-pump))
                      (format t "~&exiting~%"))))
        (wsd:start-connection client)))))

(defgeneric send-message (client type &key)
  (:documentation "Send a slack message")
  (:method :around ((client event-pump) _type &key)
    (declare (ignorable client _type))
    (let ((result (call-next-method)))
      (values result
              (wsd:send (ws-client client)
                        (with-output-to-string (s)
                          (yason:encode result s)))))))

(defmethod send-message ((client event-pump) (type (eql :ping)) &key data)
  (let* ((id (incf (latest-id client)))
         (message `(("id" . ,id)
                    ("type" . "ping"))))

    (when data
      (aconsf message "data" data))

    (format t "~&pinging with id: ~a at time: ~a~%" id (local-time:now))
    (incf (waiting-pings client))
    (alist-hash-table message
                      :test 'equal)))

(defmethod send-message ((client event-pump) (type (eql :message)) &key channel data thread)
  (let* ((id (incf (latest-id client)))
         (message `(("id" . ,id)
                    ("type" . "message")
                    ("channel" . ,channel)
                    ("text" . ,data)
                    ,@(unsplice
                       (when thread
                         `("thread_ts" . ,thread))))))
    (alist-hash-table message
                      :test 'equal)))


(define-condition connection-lost (error)
  ())

(defclass slack-client ()
  ((%commands :accessor commands :initform (make-hash-table))
   (%modules :accessor modules :initform '())))

(defgeneric start (event-pump client &key queue-pair))

(defgeneric bind (what where))
(defmethod bind ((queue-pair queue-pair) (event-pump event-pump))
  (setf (queue-pair event-pump) queue-pair))

(defun handle-work-queue (event-pump)
  (multiple-value-bind (message message-p)
      (chanl:recv (work-queue event-pump)
                  :blockp nil)
    (when message-p
      (funcall message
               event-pump))))

(defun send-pings (event-pump client)
  "Ping slack for connectivity, error if we have too many waiting pings."
  ;; Eventually drop client
  (declare (ignorable client))
  (if (> 100 (waiting-pings event-pump))
      (send-message event-pump :ping)
      (error 'connection-lost)))

(defun network-loop (event-pump modules)
  (declare (optimize (debug 3)))
  (loop for (module . args) in modules
        while (running event-pump) do
          (start-module event-pump
                        (apply #'attach-module
                               event-pump module args)))

  (fwoar.event-loop:run-loop event-pump))

(defun start-client (implementation &key (queue-pair (make-instance 'queue-pair)) modules impl-args)
  (let* ((event-pump (apply #'make-instance implementation
			    :queue-pair queue-pair
			    :client-factory (op (make-client _))
			    impl-args)))
    (setf (running event-pump) t)
    (values event-pump
            (bt:make-thread (lambda ()
                              (network-loop event-pump
                                            modules))
                            :name "Event Server"
                            :initial-bindings `((*api-token* . ,*api-token*))))))

(defmethod get-event-nonblocking ((event-pump event-pump) &key (object-as :hash-table))
  (multiple-value-bind (message message-p) (chanl:recv (result-queue event-pump) :blockp nil)
    (values (when message-p
              (yason:parse message :object-as object-as))
            message-p)))

(defmethod get-event ((queue-pair queue-pair) &key (object-as :hash-table))
  (multiple-value-bind (message message-p) (chanl:recv (result-queue queue-pair))
    (values (when message-p
              (yason:parse message :object-as object-as))
            message-p)))

(defparameter *ignored-messages* '(:pong :user_typing :desktop_notification))

(defgeneric handle-message (type event-pump ts channel message)
  (:method :around (type event-pump ts channel message)
    (flet ((report-continue (stream)
             (format stream "Skip message of type ~s" type))
           (report-retry (stream)
             (format stream "Retry message of type ~s" type)))
      (tagbody start
         (restart-case (call-next-method)
           (continue () :report report-continue)
           (skip-message () :report report-continue)
           (retry () :report report-retry
             (go start))))))
  (:method :before (type event-pump ts channel message)
    (declare (ignore event-pump ts channel))
    (unless (member type *ignored-messages*)
      (fresh-line)
      (yason:encode message)
      (terpri)))
  (:method (type (event-pump event-pump) ts channel message)
    (declare (ignore type event-pump ts channel message))
    nil)
  (:method ((type (eql :pong)) (event-pump event-pump) ts channel message)
    (declare (ignore ts type channel))
    (format t "~&Was waiting on ~a pings," (waiting-pings event-pump))
    (decf (waiting-pings event-pump))
    (format t "after pong received for ~a, now waiting on ~a~%"
            (gethash "reply_to" message)
            (waiting-pings event-pump))))

(defmethod handle-message ((type (eql :message)) (event-pump event-pump) ts channel message)
  (when-let* ((msg (gethash "text" message))
              (parsed-message (tokens msg))) 
    (when (eql #\; (elt msg 0))
      (handle-command event-pump message channel
                      (plump:decode-entities
                       (car parsed-message))
                      (cdr parsed-message))))) 

(defun event-loop (event-pump)
  (loop
    with message with message-p
    while (running event-pump) do
      (multiple-value-setq (message message-p)
        (get-event (queue-pair event-pump)))
    when message-p do
      (let ((type (gethash "type" message))
            (reply (gethash "reply_to" message))
            (ts (gethash "ts" message))
            (channel (gethash "channel" message)))
        (cond (type
               (handle-message (make-keyword (string-upcase type))
                               event-pump ts channel message))
              (reply )))
    do (sleep 0.01)))

(defun coordinate-threads (&optional queue-pair (implementation 'event-pump) args)
  (let* ((event-pump (start-client implementation
                                   :queue-pair queue-pair
                                   :modules '((hhgbot-augmented-assistant::js-executor))
                                   :impl-args args)))
    (bt:make-thread (lambda ()
                      (loop until (running event-pump)
                            finally
                               (unwind-protect (event-loop event-pump)
                                 (stop-slacker event-pump))))
                    :name "Event Loop") 
    event-pump))

(defvar *command-table* (make-hash-table :test 'equal))

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

(defun queue-message (event-pump channel message &key quote thread)
  (let ((message (if quote (quote-output message)
                     message)))
    (in-wq (event-pump)
      (send-message event-pump :message
                    :channel channel
                    :data message
                    :thread thread))))

(define-condition command-error () ())
(define-condition unsupported-args (command-error) ())

(defgeneric add-command ())
(defmacro define-command (name (event-pump ts channel &rest args) &body body)
  (let* ((command-sym (intern (string-upcase name)))
         (has-rest (position '&rest args))
         (rest-sym (gensym "rest"))
         (args (if has-rest
                   args
                   (append args `(&rest ,rest-sym)))))
    `(progn
       (defun ,command-sym (,event-pump ,ts ,channel ,@args)
         (declare (ignorable ,event-pump ,ts ,@(when (not has-rest) `(,rest-sym))))
         ,@body)
       (setf (gethash ,name *command-table*) ',command-sym))))

(defun safe-apply (func event-pump message channel args)
  (with-simple-restart (continue "Skip command")
    (apply func event-pump message channel args)))

(defun handle-command (event-pump message channel command args)
  (declare (ignorable args))
  (let* ((command (subseq command 1))
         (handler (gethash command *command-table*)))
    (print (hash-table-alist *command-table*))
    (terpri)
    (print command)
    (if handler
        (safe-apply handler event-pump message channel args)
        (queue-message event-pump channel
                       (concat "I don't understand the command `" command "`.")
                       :thread (ensure-thread message)))))

(defun edit-message (ts channel text)
  (babel:octets-to-string
   (drakma:http-request "https://slack.com/api/chat.update"
                        :method :post
                        :content (concat "token=" *api-token*
                                         "&channel=" channel
                                         "&ts=" ts
                                         "&text=" text))))

(defmacro with-output-to-message ((stream event-pump channel &key quote thread) &body body)
  (once-only (event-pump channel quote)
    `(queue-message ,event-pump ,channel
                    (with-output-to-string (,stream)
                      ,@body)
                    :quote ,quote
                    :thread ,thread)))

(defmacro with-thread-info ((ts thread-ts in-thread is-reply) message &body body)
  (once-only (message)
    `(let* ((,ts (gethash "ts" ,message))
            (,thread-ts (gethash "thread_ts" ,message))
            (,in-thread (not (null ,thread-ts)))
            (,is-reply (and ,in-thread (string/= ,ts ,thread-ts))))
       ,@body)))

(defun ensure-thread (message)
  "Continue thread or else start a new one"
  (with-thread-info (ts thread-ts in-thread is-reply) message
    (declare (ignore is-reply))
    (if in-thread thread-ts ts)))

(defun keep-in-thread (message)
  "Continue thread or continue in main thread"
  (with-thread-info (ts thread-ts in-thread is-reply) message
    (declare (ignore is-reply))
    (if in-thread thread-ts nil)))

(define-command "help" (event-pump message channel)
  (let ((*print-right-margin* (max (or *print-right-margin* 0)
                                   80)))
    (with-thread-info (ts thread-ts in-thread is-reply) message
      (format t "~&THREAD INFO: (ts ~s) (thread-ts ~s) (in-thread ~s) (is-reply ~s)~%" ts thread-ts in-thread is-reply)
      (with-output-to-message (s event-pump channel :thread (ensure-thread message))
        (format s "I understand these commands:~%~{`~a`~^ ~}"
                (hash-table-keys *command-table*))
        :quote t))))


(defvar *id* 0)
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

