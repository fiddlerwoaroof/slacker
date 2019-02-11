(in-package :slacker)

(defun slack-api-call (method &rest args)
  (bb:with-promise (resolve reject)
    (bt:make-thread
     (lambda ()
       (handler-case
           (let ((api-result (yason:parse
                              (babel:octets-to-string 
                               (drakma:http-request (concat "https://slack.com/api/" method "?token="
                                                            *api-token*)
                                                    :method :post
                                                    :content (quri:url-encode-params
                                                              (loop for (key value) on args by #'cddr
                                                                    collect (cons
                                                                             (string-downcase key)
                                                                             value))))))))
             ;; todo error handling . . .
             (resolve api-result)) 
         (serious-condition (c)
           (format t "~&Received condition ~s~%" c)
           (reject c)))))))

(defmacro define-api-wrapper (name required-args &rest args)
  (flet ((name-case (string)
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

(defmacro define-api-wrappers (&body body)
  `(progn ,@(loop for (name required-args . rest) in body
                  collect `(define-api-wrapper ,name ,required-args ,@rest))))

(in-package :slacker.api)

(slacker::define-api-wrappers
  (auth.test ())
  (users.list () include_locale limit cursor)
  (channels.list () exclude_archived)
  (conversations.list () exclude_archived types)
  (conversations.join (channel)) 
  (chat.delete (ts channel) as_user)
  (chat.me-message (channel text))
  (chat.post-message (channel text)
                     parse link_name attachments unfurl_links unfurl_media username as_user icon_uri icon_emoji)
  (chat.update (ts channel text) attachments parse link_names as_user))
