(cl:in-package :hhgbot-augmented-assistant)

;; Special Variables

(defvar *client*)
;; Macros

(defun start-in-repl (&optional (start-bot t))
  (ubiquitous:restore :hhgbot-augmented-assistant)
  (setf slacker::*api-token* (ubiquitous:value :api-token :atomampd))
  (if start-bot
      (start-with-apitoken)
      slacker::*api-token*))

(defun start-with-apitoken ()
  (ubiquitous:restore :hhgbot-augmented-assistant)
  (let ((slacker::*api-token* (ubiquitous:value :api-token :atomampd)))
    (unless slacker::*api-token*
      (format *terminal-io* "~&API Token? ")
      (finish-output *terminal-io*)
      (setf slacker::*api-token* (read-line)))
    (values (slacker:coordinate-threads)
	    slacker::*api-token*)))

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
      (format *zxcv* "~&IP: ~a~%TS: ~a~%CHANNEL: ~a~%CLIENT: ~A~%" (babel:octets-to-string ip) ts channel event-pump)
      (edit-message ts channel
		    (concat "My ip is: "
			    (babel:octets-to-string ip))))))

(define-command "jira" (event-pump ts channel project &optional issue-number &rest rest)
  (let ((drakma:*drakma-default-external-format* :utf-8))
    (apply #'edit-message ts channel
	   (fw.lu:ensure-list
	    (cond (rest
		   "I don't understand . . .")
		  (issue-number
		   (format nil "https://atomampd.atlassian.net/browse/~A-~A"
			   project issue-number))
		  (t
		   (format nil "https://atomampd.atlassian.net/browse/ATOMOS-~a"
			   project)))))))

(define-command "pr" (event-pump ts channel num)
  (let ((num (parse-integer num)))
    (edit-message ts channel (format nil "https://bitbucket.org/atomampd/atomos/pull-requests/~d?w=1" num))))

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
  (with-simple-restart (abort "Stop command")
    (ubiquitous:restore :atomampd-slack)
    (format t "foo")
    (let ((drakma:*drakma-default-external-format* :utf-8))
      (edit-message ts channel
		    (format nil "```~%~a~%```"
			    (pop (ubiquitous:value :clip)))))))

(define-command "id" (event-pump ts channel &optional (for "channel"))
  (edit-message ts channel
		(string-case for
		  ("channel" channel)
		  (t (concat "don't know the id for " for)))))
    
(defmacro with ((var val) &body body)
  `(let ((,var ,val))
     ,@body))

(ql:quickload '(:vecto :ironclad))

(defun hash-of-vector (vec)
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 vec)))

(defpackage #:vecto-example
  (:use :cl :alexandria :serapeum :slacker :vecto)
  (:shadowing-import-from :alexandria :rotate))

(defparameter *zxcv* *standard-output*)

(define-command "lss" (event-pump ts channel)
  (fw.lu:let-each (:be *)
    #p"~/public_html/Screenshots/"
    (uiop:directory-files *)
    (sort * #'> :key #'file-write-date)
    (car *)
    (uiop/common-lisp:enough-namestring * #p"~/public_html/")
    (concat "https://srv2.elangley.org/~edwlan/" *)
    (slacker.api:chat.post-message channel * :as_user t :icon_emoji "camera_with_flash")))

(in-package :vecto-example)
(defmacro with ((var val) &body body)
  `(let ((,var ,val))
     ,@body))


(defun octets-to-36string (octets)
  (string-join (map 'list (lambda (it)
			    (format nil "~2,1,0,'0@a"
				    (with (*print-base* 36)
				      (write-to-string it ))))
		    octets)))

(defmacro with-digested-output ((stream (data-sym digest-sym)) data-form &body body)
  (with-gensyms (digest-stream data-stream)
    `(let* ((,digest-stream (ironclad:make-digesting-stream :sha256))
	    (,data-stream (ironclad:make-octet-output-stream))
	    (,stream (make-broadcast-stream ,data-stream ,digest-stream)))
       ,data-form
       (let ((,data-sym (ironclad:get-output-stream-octets ,data-stream))
	     (,digest-sym (ironclad:produce-digest ,digest-stream)))
	 ,@body))))

(define-command "ring-word" (event-pump ts channel &optional (content #(#x3BB)))
  (bt:make-thread
   (lambda ()
     (declare (optimize (debug 3)))
     (let ((*default-pathname-defaults* "/home/edwlan/github_repos/hhgbot/"))
       (with-canvas (:width 90 :height 90)
	 (let ((font (get-font "times.ttf"))
	       (step (/ pi 7)))
	   (set-font font 40)
	   (translate 45 45)
	   (format hhgbot-augmented-assistant::*zxcv* "~&content: ~s~%" content)
	   (draw-centered-string 0 -10 (map 'vector #'char-code content))
	   (set-rgb-stroke 1 0 0)
	   (centered-circle-path 0 0 35)
	   (stroke)
	   (set-rgba-stroke 0 0 1.0 0.5)
	   (set-line-width 4)
	   (dotimes (i 14)
	     (with-graphics-state
	       (vecto:rotate (* i step))
	       (move-to 30 0)
	       (line-to 40 0)
	       (stroke))))
	 (let* ((image-directory (ensure-directories-exist "/home/edwlan/public_html/slack_images/"))
		(image-basename (concat channel "-" ts ".png"))
		(image-filename (merge-pathnames image-basename image-directory))
		(payload
		 (concat "token=" *api-token*
			 "&channel=" channel
			 "&text=..." 
			 "&as_user=true"
			 "&attachments="
			 (quri:url-encode
			  (concat
			   "[{\"fallback\": \"foo\", \"ts\":" ts ",\"image_url\": \"https://srv2.elangley.org/~edwlan/slack_images/"
			   image-basename "\"}]")))))
	   (vecto:save-png image-filename)
	   (babel:octets-to-string
	    (drakma:http-request "https://slack.com/api/chat.postMessage"
				 :method :post
				 :content payload))))))
     :name "Image Maker"))
