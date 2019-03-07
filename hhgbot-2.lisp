(defpackage :hhgbot-2
  (:use :cl :alexandria :serapeum :slacker :fw.lu))

(defpackage :quote-server
  (:use :cl :ningle :araneus :serapeum :alexandria :fw.lu)
   (:export #:start))

(cl:in-package :hhgbot-2)

;; Special Variables

(defvar *xxx* (make-synonym-stream '*standard-output*))
(defvar *client*)
(defvar *queue-pair* nil)
(defvar *slack-url* "https://~a.slack.com")
(defparameter *refs* (make-hash-table :test 'equalp))
;; Macros

(defclass logging-slackbot ()
  ((%users :initform (make-hash-table :test 'equal))
   (%conversations :initform (make-hash-table :test 'equal))))


(defclass hhgbot-event-pump (slacker:event-pump slacker.montezuma-store:montezuma-store logging-slackbot)
  ()
  (:default-initargs :index-path "/tmp/slack-idx/"))

(defun start-in-repl (&optional (start-bot t) (team-id :atomampd))
  (ubiquitous:restore :hhgbot-augmented-assistant)
  (setf slacker::*api-token* (ubiquitous:value :api-token :atomampd))
  (if start-bot
      (start-with-apitoken team-id)
      slacker::*api-token*))

(defun start-with-apitoken (&optional (team-id :atomampd))
  (unless *queue-pair*
    (setf *queue-pair* (make-instance 'slacker::queue-pair)))

  (ubiquitous:restore :hhgbot-augmented-assistant)
  (let ((slacker::*api-token* (ubiquitous:value :api-token team-id)))
    (unless slacker::*api-token*
      (format *terminal-io* "~&API Token? ")
      (finish-output *terminal-io*)
      (let ((value (read-line)))
        (setf (ubiquitous:value :api-token team-id) value
              slacker::*api-token* value)))
    (values (slacker:coordinate-threads *queue-pair* 'hhgbot-event-pump)
            slacker::*api-token*)))

(defun ensure-unescaped (src)
  (if (and (starts-with #\` src)
           (ends-with #\` src))
      (cond
        ((and (>= (length src) 6)
              (starts-with-subseq "```" src)
              (ends-with-subseq "```" src))
         (subseq src 3 (- (length src) 3)))
        ((>= (length src) 2)
         (subseq src 1 (- (length src) 1)))
        (t src))
      src))

(define-command "js>" (event-pump message channel &rest args)
  (declare (ignorable message))
  (let ((hhgbot-augmented-assistant::*js-executor* (get-module :js-executor event-pump))
        (js-src (ensure-unescaped (string-join args " "))))
    (blackbird:alet ((result (hhgbot-augmented-assistant::submit-js hhgbot-augmented-assistant::*js-executor*
                                                                    js-src)))
      (let ((result (cl-js:to-string result)))
        (queue-message event-pump channel
                       (subseq result 0
                               (min 1000
                                    (length result)))
                       :quote t
                       :thread (ensure-thread message))))))

(define-command "id" (event-pump message channel &optional (for "channel"))
  (queue-message event-pump channel
                 (string-case for
                   ("channel" channel)
                   (t (concat "don't know the id for " for)))
                 :quote t
                 :thread (ensure-thread message)))

(define-command "sources" (event-pump message channel &rest args)
  (declare (ignore args))
  (queue-message event-pump channel
                 (format nil "~{~a~^, ~}" (hash-table-keys *refs*))
                 :thread (ensure-thread message)))

(define-command "ref>" (event-pump message channel &optional source ref &rest args)
  (declare (ignore args))
  (if (and source ref)
      (when-let ((source-h (gethash source *refs*)))
        (queue-message event-pump channel
                       (gethash ref source-h (concat "Can't find " source " " ref))
                       :thread (keep-in-thread message)))
      (queue-message event-pump channel (concat "Must provide both a source and a reference. See ;sources")
                     :thread (keep-in-thread message))))

(defmacro define-message-command (name (&rest args) &body body)
  (let ((message-sym (cadr args)))
    `(define-command ,name (,@args)
       (queue-message event-pump channel
                      (progn ,@body)
                      :thread (keep-in-thread ,message-sym)))))

(define-command "latina" (event-pump message channel word &rest a)
  (declare (ignore a))
  (let* ((results (words-coprocess::get-word-results word))
	 (json-objs (split-sequence #\newline results :remove-empty-subseqs t))
	 (parsed (mapcar 'yason:parse json-objs)))
    (queue-message event-pump channel
		   (format nil "~{> ~a~2%~}"
			   (remove-if-not #'identity
					  (mapcan (op (gethash "meanings" _))
						  parsed))))))

(define-message-command "random-quote" (event-pump message channel &rest args)
  args
  (let-each (:be *)
    (hash-table-keys *refs*)
    (random-elt *)
    (gethash * *refs*)
    (let ((keys (hash-table-keys *)))
      (gethash (random-elt keys) *))))

(defun trace-value (&rest args)
  (format *trace-output* "===>>> ~{~s~^ ~}" args)
  (values-list args))

(define-message-command "arc" (event-pump message channel &rest args)
  (let ((r (with-output-to-string (s)
	     (multiple-value-bind (results idx)
		 (slacker.montezuma-store:search-index *client* "message" (string-join args " "))
	       (montezuma:each results
			       (lambda (h)
				 (format s "> ~a: ~a~%"
					 (local-time:format-timestring
					  nil
					  (local-time:unix-to-timestamp
					   (floor 
					    (parse-number
					     (montezuma:document-value (montezuma:get-document idx (montezuma:doc h))
								       "ts")))
					   )
					  :format local-time:+rfc3339-format+)
					 
					 (montezuma:document-value (montezuma:get-document idx (montezuma:doc h))
								   "text"))))))))
    (if (= 0 (length r))
        (format nil "No results found for: `~a`" (string-join args " "))
	r)))

(defun extract-channel-info (channels)
  (funcall (data-lens:pick
            (compose
             (data-lens:applying (lambda (name id is-member is-private is-mpim)
                                   (list name id
                                         (plist-hash-table
                                          (list "is-member" is-member
                                                "is-private" is-private
                                                "is-mpim" is-mpim)
                                          :test 'equal))))
             (data-lens:juxt (op (gethash "name" _))
                             (op (gethash "id" _))
                             (op (gethash "is_member" _))
                             (op (gethash "is_private" _))
                             (op (gethash "is_mpim" _)))))
           (gethash "channels" channels)))

(defun find-channel (name)
  (bb:alet* ((r (slacker.api:channels.list))
             (channels (funcall (data-lens:pick
                                 (data-lens:juxt (op (gethash "name" _))
                                                 (op (gethash "id" _))))
                                (gethash "channels" r))))
#+nil    (fw.su:log-json channels)
    (assoc name channels :test 'equal)))

(defmacro with-output-to-json-string ((s &rest args &key indent) &body body)
  "Set up a JSON streaming encoder context, then evaluate BODY.
Return a string with the generated JSON output."
  (declare (ignore indent))
  `(with-output-to-string (,s)
     (with-open-stream (,s (yason:make-json-output-stream s ,@args))
       ,@body)))

(defmacro dbind* (destructuring-expression promise-gen &body body)
  `(bb:attach ,promise-gen
              (fw.lu:destructuring-lambda (,destructuring-expression)
                ,@body)))

(defmethod slacker:handle-message :before (type (event-pump hhgbot-event-pump) ts channel message)
  (declare (ignore type ts channel))
#+nil
  (index-message message)
  (values))

(define-command "notify-channel" (event-pump message channel
                                             &optional target
                                             &rest args)
  (format *xxx* "~&~a: ~{~a~^ ~}~%" target args)
  (dbind* (&optional target-name target-id) (find-channel target)
          (if target-name
              (progn
                (with (message (string-join args #\space))
                  (when-let* ((start-link (position #\< message))
                              (stop-link (position #\> message :start start-link))
                              (_ (> stop-link (+ 4 start-link))))
                    (setf message (concat (subseq message 0 start-link)
                                          (subseq message (1+ start-link) stop-link)
                                          (subseq message (1+ stop-link)))))
                  (queue-message event-pump target-id
                                 message))
                (queue-message event-pump target-id
                               (format nil "Notifying channel ~a" target-name)
                               :thread (ensure-thread message)))
              (queue-message event-pump target-id (format nil "Can't find channel `~a`" target)
                             :thread (ensure-thread message)))))

(defparameter *reaction-store* (make-hash-table :test 'equalp :synchronized t))

(defun format-reaction-store ()
  (format nil "~{~a~^~%~}"
          (loop for reaction in (sort (hash-table-keys *reaction-store*) #'string-lessp)
                for (count users) = (gethash reaction *reaction-store*)
                collect (format nil ":~a: `~a` ~{<@~a>~^ ~}" reaction count (remove-duplicates users :test 'equalp)))))

(define-message-command "show-reactions" (event-pump message channel &rest args)
  args
  (format-reaction-store))

(defmethod slacker:handle-message ((type (eql :reaction_added)) event-pump ts channel message)
  ;; {"type":"reaction_added","user":"U0CSPP3SB","item":{"type":"message","channel":"G1YA9SR0S","ts":"1489124136.780019"},"reaction":"+1","item_user":"U0CSPP3SB","event_ts":"1489124534.806294","ts":"1489124534.806294"}
  (let ((reaction-info (ensure-gethash (gethash "reaction" message) *reaction-store* (list 0 nil))))
    (incf (car reaction-info))
    (push (gethash "user" message) (cadr reaction-info))))

(defun read-refs (stream)
  (loop for x = (read stream nil)
        while x
        for (type source ref text) = x
        do
           (setf (gethash ref
                          (ensure-gethash (string source)
                                          *refs*
                                          (make-hash-table :test 'equalp)))
                 text)))
(defun initialize-quotes ()
  (loop for filename in '("qda.sexp" "refs.sexp" "scg.sexp" "ss.sexp")
        do
           (with-input-from-file (s filename :external-format :utf-8)
             (read-refs s))))

(defun main ()
  (initialize-quotes)
  (quote-server:start)
  (setf (values *client* slacker::*api-token*)
	(start-in-repl)))

(in-package :quote-server)

(defparameter *app* (make-instance '<app>))

(define-controller quote (params)
  (let* ((text (cdr (assoc :text params)))
         (ref (cdr (assoc :ref params))))
    (cons (cons text ref) (gethash ref (gethash text hhgbot-2::*refs*)))))

(define-controller random-quote (params)
  (declare (ignore params))
  (let-each (:be *)
    (hash-table-keys hhgbot-2::*refs*)
    (random-elt *)
    (let* ((text (gethash * hhgbot-2::*refs*))
           (keys (hash-table-keys text))
           (selected-elt (random-elt keys)))
      (cons (cons * selected-elt) (gethash (random-elt keys) text)))))

(define-view random-quote (model)
  (destructuring-bind ((book . ref) . text) model
    `(302
      ("Location"  ,(format nil "http://hhgbot.edw.ai:5000/q/~a/~a"
                            book ref))
      (,text))))

(define-spinneret-view quote (quote)
  (let ((title (format nil "Quote: ~a, ~a" (caar quote) (cdar quote)))
        (permalink (format nil "http://hhgbot.edw.ai:5000/q/~a/~a"
                           (caar quote)
                           (cdar quote))))
    (:html
     (:head (:title title)
            (:link :href "https://fonts.googleapis.com/css?family=Lato:400&subset=latin,latin-ext" :rel "stylesheet" :type "text/css")
            (:style "p { font-family: 'Lato', sans-serif; width: 50vw; margin-left: 25vw; } p::first-letter {color:red} p:first-child { margin-top: 10vh; }")
            (:meta :property "og:title" :content title)
            (:meta :property "fb:app_id" :content "521205154682685")
            (:meta :property "og:url" :content permalink)
            (:meta :property "og:description" :content (cdr quote)))
     (:body
      (:p (cdr quote))
      (:p (:a :href permalink permalink))
      (:p (:a :href "/" "Random quote"))))))

(defroutes *app*
  (("/" :GET) (as-route 'random-quote))
  (("/q/:text/:ref" :GET) (as-route 'quote))
  )

(defparameter *word-index* (make-array 100 :adjustable t :fill-pointer 0))

(defun index-quote (text ref)
  (let ((quote (split-sequence-if-not (op (alphanumericp _))
                                      (pick (list text ref) hhgbot-2::*refs*)
                                      :remove-empty-subseqs t)))
    quote))

(let ((handler '()))
  (defun start ()
    (push (clack:clackup *app*) handler))
  (defun stop ()
    (clack:stop (pop handler))))
