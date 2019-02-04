(in-package :slacker)

(defmethod attach-module ((event-pump event-pump) module &rest args &key)
  (setf (gethash (make-keyword module)
<<<<<<< HEAD
                 (modules event-pump))
        (apply #'make-instance
               module
               args)))
=======
		 (modules event-pump))
	(apply #'make-instance
	       module
	       args)))
>>>>>>> github/master

(defgeneric get-module (module event-pump)
  (:documentation "Get one of the activated modules")
  (:method (module (event-pump event-pump))
    (gethash (make-keyword module)
<<<<<<< HEAD
             (modules event-pump))))
=======
	     (modules event-pump))))
>>>>>>> github/master

(defvar *api-token*) 

(defun make-client (event-pump)
  (flet ((get-ws-url (slack-response)
<<<<<<< HEAD
           (gethash "url" slack-response)))
=======
	   (gethash "url" slack-response)))
>>>>>>> github/master
    (fw.lu:let-each (:be slack-data)
      (format nil "https://slack.com/api/rtm.start?token=~a" *api-token*)
      (drakma:http-request slack-data :want-stream t)
      (yason:parse slack-data)

      (let* ((url (get-ws-url slack-data))
<<<<<<< HEAD
             (client (wsd:make-client url)))
        (setf (ws-client event-pump)
              client)
        (wsd:on :message client
                (lambda (message)
                  #+null
                  (format t "~&Got a message ~a~%" message)
                  (chanl:send (result-queue event-pump)
                              message)))
        client))))
=======
	     (client (wsd:make-client url)))
	(setf (ws-client event-pump)
	      client)
	(wsd:on :message client
		(lambda (message)
		  #+null
		  (format t "~&Got a message ~a~%" message)
		  (chanl:send (result-queue event-pump)
			      message)))
	client))))
>>>>>>> github/master

(defgeneric send-message (client type &key)
  (:documentation "Send a slack message")
  (:method :around ((client event-pump) _type &key)
    (declare (ignorable client _type))
    (let ((result (call-next-method)))
<<<<<<< HEAD
      (values result
              (wsd:send (ws-client client)
                        (with-output-to-string (s)
                          (yason:encode result s)))))))
=======
	(values result
		(wsd:send (ws-client client)
			(with-output-to-string (s)
			    (yason:encode result s)))))))
>>>>>>> github/master

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
<<<<<<< HEAD
         (message `(("id" . ,id)
                    ("type" . "message")
                    ("channel" . ,channel)
                    ("text" . ,data)
                    ,@(unsplice
                       (when thread
                         `("thread_ts" . ,thread))))))
    (alist-hash-table message
                      :test 'equal)))
=======
	 (message `(("id" . ,id)
		    ("type" . "message")
		    ("channel" . ,channel)
		    ("text" . ,data)
		    ,@(unsplice
		       (when thread
			 `("thread_ts" . ,thread))))))
    (alist-hash-table message
		      :test 'equal)))
>>>>>>> github/master


(defgeneric start-module (client module)
  (:documentation "start a module"))

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
<<<<<<< HEAD
                  :blockp nil)
    (when message-p
      (format t "Got a message")
      (funcall message
               event-pump))))

(defun send-pings (event-pump client)
  "Ping slack for connectivity, error if we have too many waiting pings."
=======
		  :blockp nil)
    (when message-p
      (format t "Got a message")
      (funcall message
	       event-pump))))

(defun send-pings (event-pump client)
  "Ping slack for connectivity, error if we have more than 100 waiting pings."
>>>>>>> github/master
  ;; Eventually drop client
  (declare (ignorable client))
  (if (> 100 (waiting-pings event-pump))
      (send-message event-pump :ping)
      (error 'connection-lost)))

(defun network-loop (event-pump client-factory modules)
  (declare (optimize (debug 3)))
  (loop for (module . args) in modules
<<<<<<< HEAD
        do (start-module event-pump
                         (apply #'attach-module
                                event-pump module args)))
  (let ((client (funcall client-factory))
        (connected nil))
=======
     do (start-module event-pump
		      (apply #'attach-module
			     event-pump module args)))
  (let ((client (funcall client-factory))
	(connected nil))
>>>>>>> github/master
    (as:with-event-loop () 
      (websocket-driver:start-connection client)
      (setf connected t)
      (as:with-interval (15)
<<<<<<< HEAD
        (when connected
          (restart-case (send-pings event-pump client)
            (restart-server ()
              (websocket-driver:close-connection client)
              (setf connected nil)
              (clear-waiting-pings event-pump)
              (as:with-delay (10)
                (cl+ssl:reset-library)
                (websocket-driver:start-connection
                 (setf client (funcall client-factory)))
                (setf connected t))))))
      (as:with-interval (0.01)
        (when connected
          (handle-work-queue event-pump))
        :event-cb (lambda (ev)
                    (format t "~&EVENT: ~a~%" ev))))))

(defun start-client (&key (queue-pair (make-instance 'queue-pair)) modules)
  (let* ((event-pump (make-instance 'event-pump :queue-pair queue-pair))
         (client-factory (op (make-client event-pump))))
    (values event-pump
            (bt:make-thread (lambda ()
                              (network-loop event-pump
                                            client-factory
                                            modules))
                            :name "Event Server"
                            :initial-bindings `((*api-token* . ,*api-token*))))))
=======
	(when connected
	  (restart-case (send-pings event-pump client)
	    (restart-server ()
	      (websocket-driver:close-connection client)
	      (setf connected nil)
	      (clear-waiting-pings event-pump)
	      (as:with-delay (10)
		(websocket-driver:start-connection (setf client (funcall client-factory)))
		(setf connected t))))))
      (as:with-interval (0.01)
	(when connected
	 (handle-work-queue event-pump))
	:event-cb (lambda (ev)
		    (format t "~&EVENT: ~a~%" ev))))))

(defun start-client (&key (queue-pair (make-instance 'queue-pair)) modules)
  (let* ((event-pump (make-instance 'event-pump :queue-pair queue-pair))
	 (client-factory (op (make-client event-pump))))
    (values event-pump
	    (bt:make-thread (lambda ()
			      (network-loop event-pump client-factory modules))
			    :name "Event Server"
			    :initial-bindings `((*api-token* . ,*api-token*))))))
>>>>>>> github/master

(defmethod get-event-nonblocking ((event-pump event-pump) &key (object-as :hash-table))
  (multiple-value-bind (message message-p) (chanl:recv (result-queue event-pump) :blockp nil)
    (values (when message-p
<<<<<<< HEAD
              (yason:parse message :object-as object-as))
            message-p)))
=======
	      (yason:parse message :object-as object-as))
	    message-p)))
>>>>>>> github/master

(defmethod get-event ((queue-pair queue-pair) &key (object-as :hash-table))
  (multiple-value-bind (message message-p) (chanl:recv (result-queue queue-pair))
    (values (when message-p
<<<<<<< HEAD
              (yason:parse message :object-as object-as))
            message-p)))
=======
	      (yason:parse message :object-as object-as))
	    message-p)))
>>>>>>> github/master

(defparameter *ignored-messages* '(:pong))
(defgeneric handle-message (type event-pump ts channel message)
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
<<<<<<< HEAD
            (gethash "reply_to" message)
            (waiting-pings event-pump))))
=======
	    (gethash "reply_to" message)
	    (waiting-pings event-pump))))
>>>>>>> github/master

(defmethod handle-message ((type (eql :message)) (event-pump event-pump) ts channel message)
  (format t "~&Received message ~s~%" message)
  (when-let* ((msg (gethash "text" message))
<<<<<<< HEAD
              (parsed-message (tokens msg))) 
    (when (eql #\; (elt msg 0))
      (handle-command event-pump message channel
                      (plump:decode-entities
                       (car parsed-message))
                      (cdr parsed-message))))) 

(defun event-loop (event-pump)
  (loop with message with message-p
        do (multiple-value-setq (message message-p) (get-event (queue-pair event-pump)))
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

(defun coordinate-threads (&optional queue-pair)
  (let* ((event-pump (start-client :queue-pair queue-pair
                                   :modules '((hhgbot-augmented-assistant::js-executor)))))
    (bt:make-thread (lambda () (event-loop event-pump))
                    :name "Event Loop") 
=======
	      (parsed-message (tokens msg))) 
    (when (eql #\; (elt msg 0))
      (handle-command event-pump message channel
		      (car parsed-message)
		      (cdr parsed-message))))) 

(defun event-loop (event-pump)
  (loop with message with message-p
     do (multiple-value-setq (message message-p) (get-event (queue-pair event-pump)))
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

(defun coordinate-threads (&optional queue-pair)
  (let* ((event-pump (start-client :queue-pair queue-pair :modules '((hhgbot-augmented-assistant::js-executor)))))
    (bt:make-thread (lambda ()  (event-loop event-pump))
		    :name "Event Loop") 
>>>>>>> github/master
    event-pump))

(defparameter *command-table* (make-hash-table :test 'equal))

(defun quote-output (str)
  (with-output-to-string (s)
    (format s "```~%~a```~%" str)))

(defmacro in-wq ((client-sym) &body body)
  `(let ((promise (blackbird-base:make-promise)))
     (values promise
<<<<<<< HEAD
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
=======
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
>>>>>>> github/master

(define-condition command-error () ())
(define-condition unsupported-args (command-error) ())

(defgeneric add-command ())
(defmacro define-command (name (event-pump ts channel &rest args) &body body)
  (let* ((command-sym (intern (string-upcase name)))
<<<<<<< HEAD
         (has-rest (position '&rest args))
         (rest-sym (gensym "rest"))
         (args (if has-rest
                   args
                   (append args `(&rest ,rest-sym)))))
    `(progn
       (defun ,command-sym (,event-pump ,ts ,channel ,@args)
         (declare (ignorable ,event-pump ,ts ,@(when (not has-rest) `(,rest-sym))))
         ,@body)
=======
	 (has-rest (position '&rest args))
	 (rest-sym (gensym "rest"))
	 (args (if has-rest
		   args
		   (append args `(&rest ,rest-sym)))))
    `(progn
       (defun ,command-sym (,event-pump ,ts ,channel ,@args)
	 (declare (ignorable ,event-pump ,ts ,@(when (not has-rest) `(,rest-sym))))
	   ,@body)
>>>>>>> github/master
       (setf (gethash ,name *command-table*) ',command-sym))))

(defun safe-apply (func event-pump message channel args)
  (with-simple-restart (continue "Skip command")
    (apply func event-pump message channel args)))

(defun handle-command (event-pump message channel command args)
  (declare (ignorable args))
<<<<<<< HEAD
  (let* ((command (subseq command 1))
         (handler (gethash command *command-table*)))
=======
  (let* ((command (subseq (plump:decode-entities command) 1))
	 (handler (gethash command *command-table*)))
>>>>>>> github/master
    (print (hash-table-alist *command-table*))
    (terpri)
    (print command)
    (if handler
<<<<<<< HEAD
        (safe-apply handler event-pump message channel args)
        (queue-message event-pump channel
                       (concat "I don't understand the command `" command "`.")
                       :thread (ensure-thread message)))))
=======
	(safe-apply handler event-pump message channel args)
	(queue-message event-pump channel (concat "I don't understand the command `" command "`.")))))
>>>>>>> github/master

(defun slack-api-call (method &rest args)
  (bb:with-promise (resolve reject)
    (bt:make-thread
     (lambda ()
       (handler-case
<<<<<<< HEAD
           (let ((api-result (yason:parse
                              (babel:octets-to-string 
                               (drakma:http-request (concat "https://slack.com/api/" method "?token=" *api-token*)
                                                    :method :post
                                                    :content (quri:url-encode-params
                                                              (loop for (key value) on args by #'cddr
                                                                    collect (cons (string-downcase key) value)))
                                                    )))))
                                        ;todo error handling . . .
             (resolve api-result)) 
         (t (c)
           (format t "~&Received condition ~s~%" c)
           (reject c)))))))
=======
	   (let ((api-result (yason:parse
			      (babel:octets-to-string 
			       (drakma:http-request (concat "https://slack.com/api/" method "?token=" *api-token*)
						    :method :post
						    :content (quri:url-encode-params
							      (loop for (key value) on args by #'cddr
								 collect (cons (string-downcase key) value)))
						    )))))
	     ;todo error handling . . .
	     (resolve api-result)) 
	 (t (c)
	   (format t "~&Received condition ~s~%" c)
	   (reject c)))))))
>>>>>>> github/master

;; (defgeneric api-call (name args)
;;   (:method ((name symbol) (args list))
;;     (slack-api-call name)))

(defmacro define-api-wrapper (name required-args &rest args)
  (flet ((name-case (string)
<<<<<<< HEAD
           (let ((parts (split-sequence #\- (string-downcase string))))
             (apply #'concatenate 'string
                    (car parts)
                    (mapcar #'string-capitalize (cdr parts))))))
    (let* ((api-method-name (name-case name)))
      `(progn (defun ,name (,@required-args &rest r &key ,@args)
                (apply #'slack-api-call ,api-method-name
                       ,@(loop for req-arg in required-args
                               append (list (make-keyword req-arg) req-arg))
                       r))
              (eval-when (:compile-toplevel :load-toplevel :execute)
                (let ((*package* (find-package 'slacker.api)))
                  (import ',name)
                  (export ',name)))))))
=======
	   (let ((parts (split-sequence #\- (string-downcase string))))
	     (apply #'concatenate 'string
		    (car parts)
		    (mapcar #'string-capitalize (cdr parts))))))
    (let* ((api-method-name (name-case name)))
      `(progn (defun ,name (,@required-args &rest r &key ,@args)
		(apply #'slack-api-call ,api-method-name
		       ,@(loop for req-arg in required-args
			    append (list (make-keyword req-arg) req-arg))
		       r))
	      (eval-when (:compile-toplevel :load-toplevel :execute)
		(let ((*package* (find-package 'slacker.api)))
		  (import ',name)
		  (export ',name)))))))
>>>>>>> github/master


(defmacro define-api-wrappers (&body body)
  `(progn ,@(loop for (name required-args . rest) in body
<<<<<<< HEAD
                  collect `(define-api-wrapper ,name ,required-args ,@rest))))
=======
		 collect `(define-api-wrapper ,name ,required-args ,@rest))))
>>>>>>> github/master

(defun edit-message (ts channel text)
  (babel:octets-to-string
   (drakma:http-request "https://slack.com/api/chat.update"
<<<<<<< HEAD
                        :method :post
                        :content (concat "token=" *api-token*
                                         "&channel=" channel
                                         "&ts=" ts
                                         "&text=" text))))
=======
			:method :post
			:content (concat "token=" *api-token*
					 "&channel=" channel
					 "&ts=" ts
					 "&text=" text))))
>>>>>>> github/master

(defmacro with-output-to-message ((stream event-pump channel &key quote thread) &body body)
  (once-only (event-pump channel quote)
    `(queue-message ,event-pump ,channel
<<<<<<< HEAD
                    (with-output-to-string (,stream)
                      ,@body)
                    :quote ,quote
                    :thread ,thread)))
=======
		    (with-output-to-string (,stream)
		      ,@body)
		    :quote ,quote
		    :thread ,thread)))
>>>>>>> github/master

(defmacro with-thread-info ((ts thread-ts in-thread is-reply) message &body body)
  (once-only (message)
    `(let* ((,ts (gethash "ts" ,message))
<<<<<<< HEAD
            (,thread-ts (gethash "thread_ts" ,message))
            (,in-thread (not (null ,thread-ts)))
            (,is-reply (and ,in-thread (string/= ,ts ,thread-ts))))
=======
	    (,thread-ts (gethash "thread_ts" ,message))
	    (,in-thread (not (null ,thread-ts)))
	    (,is-reply (and ,in-thread (string/= ,ts ,thread-ts))))
>>>>>>> github/master
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
<<<<<<< HEAD
                                   80)))
    (with-thread-info (ts thread-ts in-thread is-reply) message
      (format t "~&THREAD INFO: (ts ~s) (thread-ts ~s) (in-thread ~s) (is-reply ~s)~%" ts thread-ts in-thread is-reply)
      (with-output-to-message (s event-pump channel :thread (ensure-thread message))
        (format s "I understand these commands:~%~{`~a`~^ ~}"
                (hash-table-keys *command-table*))
        :quote t))))
=======
				   80)))
    (with-thread-info (ts thread-ts in-thread is-reply) message
      (format t "~&THREAD INFO: (ts ~s) (thread-ts ~s) (in-thread ~s) (is-reply ~s)~%" ts thread-ts in-thread is-reply)
      (with-output-to-message (s event-pump channel :thread (ensure-thread message))
	(format s "I understand these commands:~%~{`~a`~^ ~}"
		(hash-table-keys *command-table*))
	:quote t))))
>>>>>>> github/master


(defparameter *id* 0)
(defun make-message (data channel)
  (incf *id*)
  (with-output-to-string (s)
    (yason:encode
     (alist-hash-table
      `(("id" . ,*id*)
<<<<<<< HEAD
        ("type" . "message")
        ("channel" . ,channel)
        ("text" . ,data)))
=======
	("type" . "message")
	("channel" . ,channel)
	("text" . ,data)))
>>>>>>> github/master
     s)))


(in-package :slacker.api)

(slacker::define-api-wrappers
  (channels.list () exclude_archived)
  (chat.delete (ts channel) as_user)
  (chat.me-message (channel text))
<<<<<<< HEAD
  (chat.post-message (channel text)
                     parse link_name attachments unfurl_links unfurl_media username as_user icon_uri icon_emoji)
=======
  (chat.post-message (channel text) parse link_name attachments unfurl_links unfurl_media username as_user icon_uri icon_emoji)
>>>>>>> github/master
  (chat.update (ts channel text) attachments parse link_names as_user))

