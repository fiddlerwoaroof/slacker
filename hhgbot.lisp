;;;; hhgbot.lisp

(in-package #:hhgbot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\{
                                (lambda (stream char param)
                                  (declare (ignore char param))
                                  (let ((elems (read-delimited-list #\} stream t))
                                        (rest-sym (gensym "rest")))
                                    `(lambda (&rest ,rest-sym)
                                       (apply ',(car elems) ,@(cdr elems) ,rest-sym)))))
  (set-macro-character #\} (get-macro-character #\) nil)))

(defparameter *api-token*
  "xoxb-21694007908-XtlBghcRjIZkTZCIbXQgdMIf")


(defun write-crlf (stream)
  (format stream "~c"
          #\linefeed))


(defun write-crlf (stream)
  (format stream "~c~c"
          #\return
          #\linefeed))

;  GET /websocket/lEEwhrr2xA4rxHXxY0bPY7Ir06hXul4yhvYfIN2iU8-zBRIHrDIMfTDhIzbs67fAKy0Iw2wrLY1mggMTkO1xAq8WtGTVYMtdE4HhP7jWQrLJNxfSsneGAuvAN_wGWvW9cPxv6hLBjFfA_QQl3FFwHnspEQelNdKGj8ISdIsYYLI= HTTP/1.1
;  
;  Host: mpmulti-y1d8.slack-msgs.com:443
;  Upgrade: WebSocket
;  Connection: Upgrade
;  Pragma: no-cache
;  Cache-Control: no-cache
;  Sec-WebSocket-Key: fcat0W4ssKWT29LZoAKgaw==
;  Sec-WebSocket-Version: 13


(defun format-with-crlf (s control &rest args)
  (apply #'format s control args)
  (write-crlf s))

(defun make-connection-string (puri &optional s)
  ;(let ((s (make-broadcast-stream *standard-output* s)))
    (fresh-line s)
    (format-with-crlf s "GET ~a HTTP/1.1" (puri:uri-path puri))
    (format-with-crlf s "Host: ~a:443" (puri:uri-host puri))
    (format-with-crlf s "User-Agent: hhgbot")
    (format-with-crlf s "Upgrade: websocket")
    (format-with-crlf s "Connection: Upgrade")
    (format-with-crlf s "Pragma: no-cache")
    (format-with-crlf s "Cache-Control: no-cache")
    (format-with-crlf s "Sec-WebSocket-Key: fcat0W4ssKWT29LZoAKgaw==")
    (format-with-crlf s "Sec-WebSocket-Version: 13")
    (format-with-crlf s "")
    (finish-output s)
    );)

(defclass content-type ()
  ((%genus :initarg :genus :initform (error "need a genus") :reader genus)
   (%species :initarg :species :initform (error "need a species") :reader species)
   (%metadata :initarg :metadata :initform '() :reader metadata)))

(defmethod print-object ((object content-type) s)
  (print-unreadable-object (object s :type t :identity t)
    (format s "~a/~a ~s"
            (genus object)
            (species object)
            (metadata object)
            )))

(defgeneric parse-header-value (key value)
  (:method (key value)
    value))

(defmethod parse-header-value ((key (eql :content-length)) value)
  (parse-integer value))

(defmethod parse-header-value ((key (eql :expires)) value)
  (parse-integer value))

(defmethod parse-header-value ((key (eql :content-type)) value)
  (let ((parts (mapcar (plambda (string-trim '(#\space #\tab) :1))
                       (split-sequence:split-sequence #\; value))))
    (destructuring-bind (content-type . parameters) parts
      (destructuring-bind (type subtype) (split-sequence:split-sequence #\/ content-type)
        (make-instance 'content-type
                       :genus type
                       :species subtype
                       :metadata (mapcar (plambda
                                           (funcall (alexandria:compose #'alexandria:make-keyword #'string-upcase)
                                                    (split-sequence:split-sequence #\= :1)))
                                         parameters))))))

(defun parse-header (header-string)
  (declare (optimize (debug 3)))
  (let ((keywords-to-remove '()))
    (flet ((temp-keyword (name)
             (declare (optimize (debug 3)))
             (multiple-value-bind (keyword status) (funcall (alexandria:compose #'alexandria:make-keyword #'string-upcase)
                                                            name))))
      (let* ((sep-position (position #\: header-string))
             (name (alexandria:make-keyword
                     (string-upcase 
                       (subseq header-string 0 sep-position))))

             (value (subseq header-string (+ 2 sep-position))))
        (cons name
          (parse-header-value name value))))))

(defun get-google (puri char-stream &optional (ostream char-stream))
  (make-connection-string puri ostream)
  (loop with buf = (make-string 1)
        for q = (read-sequence buf char-stream)
        when (> q 0)
          do (princ buf)))

(defun ssl-connect (puri port continuation)
  (let ((hn (puri:uri-host puri)))
    (usocket:with-client-socket (socket stream hn port
                                        :element-type '(unsigned-byte 8))
      (let* ((ssl-stream (cl+ssl:make-ssl-client-stream stream :hostname hn))
             (char-stream (flexi-streams:make-flexi-stream ssl-stream
                                                           :external-format '(:utf-8))))
        (unwind-protect
          (progn (format t "~a~%" puri)
                 (funcall continuation puri char-stream
                          char-stream))
          (close ssl-stream))))))

(progn (defparameter *ws-url*
         (puri:parse-uri
           (funcall (alexandria:compose #'cdr
                                        (plambda (assoc "url" :1
                                                        :test #'string-equal)))
                    (yason:parse
                      (drakma:http-request
                        (format nil "https://slack.com/api/rtm.start?token=~a"
                                *api-token*)
                        :want-stream t)
                      :object-as :alist)))
         )
       (make-connection-string *ws-url* t)
       (format t "'~a~%" *ws-url*)
       )

(let ((headers '())
      (body '())
      (body-count 0)
      (tmp-header-string nil)
      (mode :header)
      (*ws-url*
        (puri:parse-uri
          (funcall (alexandria:compose #'cdr
                                       (plambda (assoc "url" :1
                                                       :test #'string-equal)))
                   (yason:parse
                     (drakma:http-request
                       (format nil "https://slack.com/api/rtm.start?token=~a"
                               *api-token*)
                       :want-stream t)
                     :object-as :alist)))))
  (as:start-event-loop
    (lambda ()

      (declare (optimize (debug 3)))
      (cl-async-ssl:tcp-ssl-connect
        "slack.com" 443
        (lambda (socket data)
          (let* ((data (babel:octets-to-string data)))
            (when tmp-header-string
              (psetf data (concatenate 'string
                                       tmp-header-string
                                       data)
                     tmp-header-string nil))

            (case mode
              (:header
                (loop for next-divide = (position #\return data)
                      while next-divide
                      for next-header = (subseq data 0 next-divide)
                      until (string= next-header "")

                      when (alexandria:starts-with-subseq "HTTP" next-header) do
                      (format t "Initial line: ~a~%" next-header)

                      unless (alexandria:starts-with-subseq "HTTP" next-header) do
                      (push (parse-header next-header)
                            headers)
                      (format t "GOT: ~s~%" (car headers)) 

                      when (< next-divide (1- (length data))) do
                      (setf data (subseq data (+ 2 next-divide)))

                      finally
                      (when (> (length data) 0)
                        (setf tmp-header-string data))
                      (when (string= next-header "")
                        (setf mode :body)
                        (push data body)
                        (incf body-count (length data))
                        (setf tmp-header-string ""))))
              (:body
                (push data body)
                (incf body-count (length data))))

            (format t "loop done, body count: ~d, content-length ~d ~%"
                    body-count
                    (cdr (assoc :content-length headers))) 
            (when (>= body-count
                      (or (cdr (assoc :content-length headers)) 0))
              (as:close-socket socket))))
        :event-cb (lambda (ev)
                    (format t "EV: ~a~%" ev))
        :read-timeout 3
        :data (with-output-to-string (s)
                (make-connection-string *ws-url* s)))))
  (values headers (apply #'concatenate 'string body)))
