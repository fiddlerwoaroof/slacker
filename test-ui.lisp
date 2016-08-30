(cl:in-package :hhgbot-augmented-assistant)

;; Special Variables

(defvar *client*)
;; Macros

(defun start-with-apitoken ()
  (ubiquitous:restore :hhgbot-augmented-assistant)
  (let ((slacker::*api-token* (ubiquitous:restore :api-token :***REMOVED***)))
    (unless slacker::*api-token*
      (format *terminal-io* "~&API Token? ")
      (finish-output *terminal-io*)
      (setf slacker::*api-token* (read-line)))
    (slacker:coordinate-threads)))

(defmacro if-let* ((&rest bindings) &body (then-form &optional else-form))
  "Like if-let, but sets bindings sequentially.  Doesn't short-circuit."
  `(let* ,bindings
     (if (and ,@(mapcar #'car bindings))
       ,then-form
       ,else-form)))
;; Utility functions

(defun pick (keys h-t)
  (mapcar (plambda:plambda (gethash :1 h-t))
          keys))

(define-command "myip" (event-pump ts channel)
  (in-wq (event-pump)
    (blackbird:alet ((ip (carrier:request "http://api.ipify.org/" :return-body t)))
      (format *zxcv* "~&IP: ~a~%" ip)
      (edit-message ts channel
		    (concat "My ip is: "
			    (babel:octets-to-string ip))))))

(define-command "jira" (event-pump ts channel project &optional issue-number &rest rest)
  (apply #'edit-message ts channel
	 (fw.lu:ensure-list
	  (cond (rest
		 "I don't understand . . .")
		(issue-number
		 (format nil "https://***REMOVED***.atlassian.net/browse/~A-~A"
			 project issue-number))
		(t
		 (format nil "https://***REMOVED***.atlassian.net/browse/ATOMOS-~a"
			 project))))))

(define-command "js>" (event-pump ts channel &rest args)
  (declare (ignorable ts))
  (let ((*js-executor* (gethash :js-executor (modules event-pump))))
    (blackbird:alet ((result (submit-js *js-executor*
					(string-join args " "))))
      (let ((result (cl-js:to-string result)))
	(queue-message event-pump channel
		       (subseq result 0
			       (min 1000
				    (length result)))
		       :quote t)))))

(define-command "my-rsa-key" (event-pump ts channel &optional (cipher "rsa"))
  (with-output-to-message (s event-pump  channel :quote t)
    (with-open-file (f (truename (concat "~/.ssh/id_" cipher ".pub"))) 
      (let ((seq (make-string (file-length f))))
	(read-sequence seq f)
	(format s seq)))))

(define-command "paste" (event-pump ts channel)
  (ubiquitous:restore :***REMOVED***-slack)
  (edit-message ts channel
		(format nil "```~%~a~%```"
			(pop (ubiquitous:value :clip)))))
