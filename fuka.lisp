(defpackage :hhgbot-fukaws
  (:use :cl :alexandria :serapeum))

(in-package :hhgbot-fukaws)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ubiquitous:restore 'hhgbot)
  (require 'sb-concurrency))

(defvar *client*)
(defclass slack-client ()
  ((self :reader self :initarg :self)
   (url :reader url :initarg :url)
   (users :accessor users :initarg :users :initform (make-hash-table :test 'equal))
   (slack-info :reader slack-info :initarg :slack-info)
   (work-mailbox :reader work-mailbox :initform (sb-concurrency:make-mailbox :name "work"))
   (name :reader name)
   (waiting-pings :accessor waiting-pings :initform 0)
   (latest-id :accessor latest-id :initform 0)
   (ws-client :reader ws-client :initarg :ws-client)))

(defmethod initialize-instance :after ((client slack-client) &rest r)
  (declare (ignore r))
  (let ((self (self client)))
    (setf (slot-value client 'name)
          (gethash "name" self))))

(defclass user ()
  ((id :reader id :initarg :id)
   (name :reader name :initarg :name)
   (presence :accessor presence :initarg :presence)
   (deleted :reader deleted :initarg :deleted)
   (color :reader color :initarg :color)
   (profile :reader profile :initarg :profile)
   (is_admin :reader is_admin :initarg :is_admin)
   (is_owner :reader is_owner :initarg :is_owner)
   (is_primary_owner :reader is_primary_owner :initarg :is_primary_owner)
   (is_restricted :reader is_restricted :initarg :is_restricted)
   (is_ultra_restricted :reader is_ultra_restricted :initarg :is_ultra_restricted)
   (has_2fa :reader has_2fa :initarg :has_2fa)
   (two_factor_type :reader two_factor_type :initarg :two_factor_type)
   (has_files :reader has_files :initarg :has_files)))

(defmethod print-object ((o user) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~a: ~a" (id o) (name o))))

(defmacro define-constructor (name (class &rest args))
  `(defun ,name (source-hash-table)
     (make-instance ',class
                    ,@(mapcan (lambda (arg) (list (make-keyword arg)
                                                  `(gethash ,(symbol-name arg)
                                                            source-hash-table)))
                              args))))

(define-constructor make-user (user id name deleted color profile is_admin is_owner is_primary_owner is_restricted is_ultra_restricted has_2fa two_factor_type has_files presence))


(defparameter *api-token*
  "DEL")

(defgeneric send-message (client type &optional data))

(defgeneric handle-message (type message)
  (:documentation "Handle a websocket message")
  (:method (_type message)
   (format t "~& Ok? ~s "(gethash "ok" message))
   (when (eq 'yason:false (gethash "ok" message))
     (format t "~&Problem: ~s~%" (hash-table-alist (gethash "error" message))))
   (format t "Received a packet of type: ~a~%with data: ~s~%" _type
           (hash-table-alist message))))

(defgeneric handle-mention (client event-data id channel message mentioned-pos)
  (:method (client event-data id channel message mentioned-pos)))

(defgeneric bot-command (command &rest args)
  (:method (c &rest r)
    (format t "Received command ~a with args ~s" c r)))

(defun make-attachment (title pretext text)
  (alist-hash-table
    `(("title" . ,title)
      ("pretext" . ,pretext)
      ("text" . ,text))
    :test 'equal))

(defun build-message (id channel text &rest attachments)
 (alist-hash-table 
   `(("id" . ,id)
     ("type" . "message")
     ("channel" . ,channel)
     ("text" . ,text)
     ,@(when attachments
         (cons "attachments"
               (list attachments))))
   :test 'equal))

(let ((id 0))
  (defun make-message (data channel)
    (incf id)
    (with-output-to-string (s)
      (yason:encode
        (alist-hash-table
          `(("id" . ,id)
            ("type" . "message")
            ("channel" . ,channel)
            ("text" . ,data)))
        s))))

(defmethod send-message :around ((client slack-client) _type &optional data)
  (declare (ignorable client _type data))
  (wsd:send (ws-client client)
            (with-output-to-string (s)
              (yason:encode
                (call-next-method)
                s))))

(defmethod send-message ((client slack-client) (type (eql :ping)) &optional data)
  (let* ((id (incf (latest-id client)))
         (message `(("id" . ,id)
                    ("type" . "ping"))))
    (when data
      (push (cons "data" data)
            message))
    (incf (waiting-pings client))
    (alist-hash-table message
                      :test 'equal)))

(defun pick (keys h-t)
  (mapcar (plambda:plambda (gethash :1 h-t))
          keys))

(defun quote-output (str)
  (with-output-to-string (s)
    (format s "```~%~a```~%" str)))

(defvar *memory* '())

(defvar *feeds* '("https://thejosias.com/feed"
                  "https://sancrucensis.wordpress.com/feed"
                  "https://thomism.wordpress.com/feed"))

(defvar *books* (ubiquitous:defaulted-value '() :lists :books))

(defclass list-manager ()
  ())

(defgeneric add-to-list (list-name item))

(defmethod add-to-list ((list-name (eql :books)) item)
  (push item
        (ubiquitous:value :lists :books)))

(defmethod get-list ((list-name (eql :books)))
  (ubiquitous:value :lists :books))

(defun get-random-article ()
  (let* ((feed-url (elt *feeds*
                        (funcall (compose #'random #'length)
                                 *feeds*)))
         (feed (alimenta.pull-feed:pull-feed feed-url)))
    (alimenta::get-random-item feed)))

(defmacro if-let* ((&rest bindings) &body (then-form &optional else-form))
  "Like if-let, but sets bindings sequentially.  Doesn't short-circuit."
  `(let* ,bindings
     (if (and ,@(mapcar #'car bindings))
       ,then-form
       ,else-form)))

(defmethod handle-mention ((client slack-client) (event-data hash-table) (id string) (channel string) (message string) (mentioned-pos (eql 0)))
  (declare (optimize (debug 3)))
  (if-let ((message (if (starts-with #\D) (cdr (tokens message)))))
    (let* ((the-user (gethash (gethash "user" event-data)
                              (users client)))
           (msg-text (string-case:string-case ((car message) :default "Not Recognized")
                       ("users"
                        (if (is_admin the-user)
                          (quote-output
                            (with-output-to-string (s)
                              (format-users client s)))
                          "Can't help you"))
                       ("josias"
                        (in-eventloop (cl)
                          (let* ((feed (alimenta.pull-feed:pull-feed "http://thejosias.com/feed"))
                                 (item (alimenta::get-random-item feed)))
                            (wsd:send (ws-client cl)
                                      (make-message (format nil "~a ( ~a )"
                                                            (alimenta:title item)
                                                            (alimenta:link item))
                                                    channel)))))

                       ("recommend"
                        (string-case:string-case ((cadr message)
                                                  :default (format nil
                                                                   "I don't know about ~a~p"
                                                                   (cadr message)
                                                                   2))
                          ("book"
                           (wsd:send (ws-client client) 
                                     (make-message
                                       (if-let* ((title (string-join (cddr message) #\space))
                                                 (message (format nil "I'll remember ~a" title)))
                                         (prog1 message
                                           (add-to-list :books title))
                                         "No book suggested???")
                                       channel)))
                          ("feed" (let ((feed (caddr message)))
                                    (push (subseq feed
                                                  1
                                                  (1- (length feed)))
                                        *feeds*)))))
                       ("suggest"
                        (string-case:string-case ((cadr message) :default (format nil "I don't know about ~a~p"
                                                                                  (cadr message) 2))
                          ("book" (let ((*books* (get-list :books)))
                                    (wsd:send (ws-client client)
                                              (make-message (elt *books*
                                                                 (random (length *books*)))
                                                            channel))))
                          ("article" (in-eventloop (cl)
                                       (let ((item (get-random-article)))
                                         (wsd:send (ws-client cl)
                                                   (make-message (format nil "~a ( ~a )"
                                                                         (alimenta:title item)
                                                                         (alimenta:link item))
                                                                 channel)))))))
                       ("list"
                        (string-case:string-case ((cadr message) :default "No such list")
                          ("feeds"
                           (wsd:send (ws-client *client*)
                                     (make-message (format nil "```~%~{~a~^~%~}~%```"
                                                           *feeds*)
                                                   channel)))  
                          ("books"
                           (let ((*books* (ubiquitous:value :lists :books)))
                             (wsd:send (ws-client *client*)
                                       (make-message (format nil "```~%~{~a~^~%~}~%```"
                                                             *books*)
                                                     channel))))))
                       ("remember"
                        (wsd:send (ws-client client)
                                  (make-message (car (push (string-join (cdr message)
                                                                        #\space)
                                                           *memory*))
                                                channel)))
                       ("recall"
                        (let ((mem-length (length *memory*)))
                          (wsd:send (ws-client client)
                                    (make-message (elt *memory*
                                                       (random mem-length))
                                                  channel)))))))
      (wsd:send (ws-client client)
                (make-message msg-text channel)))))

(defmethod handle-message ((type (eql :pong)) data)
  (with-accessors ((waiting-pings waiting-pings)) *client*
    (decf waiting-pings)
    (when (> waiting-pings 0)
      (format t "Something wrong? ~a waiting pings" waiting-pings)
      (when (> waiting-pings 5)
        (setf waiting-pings 0)))))

(defmethod handle-message ((type (eql :error)) data)
  (format t "~&~s~%" (hash-table-alist (gethash "error" data))))

(defmethod handle-message ((type (eql :message)) data)
    (format t "~&~s~%" (hash-table-alist data))
  (let* ((message (gethash "text" data))
         (id (gethash "id" (self *client*)))
         (name (name *client*))
         (channel (gethash "channel" data))
         (mentioned (or (search (format nil "<@~a>" id)
                                message)
                        (search (format nil "~a " name) message)   
                        (search id message))))
    (format t "~&Received a message with text: ~a~&"
            message)
    (format t "~&My id is: ~a~%"
            id)
    (format t "~&The message mentions me? ~a~%"
            mentioned)
    (when mentioned
      (handle-mention *client* data id channel message mentioned))))

(defmethod handle-message ((type (eql :presence_change)) data)
  (let ((id (gethash "user" data))
        (presence (gethash "presence" data)))
    (when-let* ((user (gethash id (users *client*)))
                (old-presence (presence user))
                (user-name (name user)))
      (setf (presence user)
            presence)
      (format t "~&Presence change: ~a is now ~a (~a -> ~a)~%"
              user-name
              presence
              old-presence
              (presence user)))))

(defmethod handle-message ((type (eql :team_join)) data)
  (let ((user (gethash "user" data)))
    (when user
      (setf (gethash (gethash "id" user)
                     (users *client*))
            (make-user user))
      (format t "~&Added user: ~a~%" (gethash "id" user)))))

(defun get-ws-url (slack-response)
  (gethash "url" slack-response))

(defun make-client ()
  (fw.lu:let-each (:be slack-data)
    (format nil "https://slack.com/api/rtm.start?token=~a" *api-token*)
    (drakma:http-request slack-data :want-stream t)
    (yason:parse slack-data)

    (let* ((url (get-ws-url slack-data))
           (self (gethash "self" slack-data))
           (users (gethash "users" slack-data))
           (client (wsd:make-client url)))

      (wsd:on :message client
              (lambda (message)
                (let* ((message (yason:parse message
                                             :object-as :hash-table
                                             :json-booleans-as-symbols t))
                       (type (funcall (compose #'make-keyword #'string-upcase)
                                      (gethash "type" message "DEFAULT-TYPE"))))
                  (handle-message type message))))

      (make-instance 'slack-client
                     :self self
                     :url url
                     :slack-info slack-data
                     :ws-client client
                     :users (alist-hash-table
                              (loop for user in users
                                    collect (cons (gethash "id" user)
                                                  (make-user (copy-hash-table user :test 'equalp))))
                              :test 'equal)))))

(defun start-heartbeat (client &optional (interval 5))
  (bordeaux-threads:make-thread
    (lambda ()
      (let ((*client* client))
        (loop
          (in-eventloop (*client*)
            (send-message *client* :ping))
          (sleep interval))))
    :name "Heartbeat"))

(defun start-client ()
  (let ((slack-client (make-client)))
    (values
      slack-client
      (bordeaux-threads:make-thread
        (lambda ()
          (let ((*client* slack-client))
            (as:with-event-loop ()
              (websocket-driver.ws.base:start-connection (ws-client slack-client))
              (format t "... after start-connection ...")
              (as:idle
                (lambda ()
                  (multiple-value-bind (message message-p) (sb-concurrency:receive-message-no-hang (work-mailbox *client*))
                    (when message-p
                      (format t "~&got message~&")
                      (funcall message *client*))))))))
        :name "Server"))))

(defun call-in-eventloop (client cb)
  (sb-concurrency:send-message (work-mailbox client)
                               cb))

(defmacro in-eventloop ((client) &body body)
  `(call-in-eventloop *client*
                      (lambda (,client)
                        (declare (ignorable ,client))
                        ,@body)))

(defun format-users (client &optional (stream t))
  (format stream "~&~:{~a: ~{~19<~a~>~^ ~}~%~}"
          (stable-sort
            (sort
              (loop for id being the hash-keys of (users client) using (hash-value user)
                    collect (list id (list (name user) (presence user))))
              #'string-lessp
              :key #'caadr)
            #'string-lessp
            :key #'cadadr)))
