;;;; package.lisp
(defpackage :slacker
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export #:coordinate-threads #:define-command #:start-module
	   #:event-pump #:attach-module #:*api-token* #:send-message
	   #:get-event-nonblocking #:get-event #:event-loop #:quote-output
	   #:in-wq #:aconsf #:queue-message #:command-error #:unsupported-args
	   #:define-command #:edit-message #:with-output-to-message
	   #:help #:make-message #:modules #:latest-id #:work-queue #:result-queue
	   #:ws-client #:waiting-pings #:ts #:channel))

(defpackage slacker.api
  (:use))

(defpackage :hhgbot-augmented-assistant
  (:use :cl :alexandria :serapeum :slacker))
;; (defpackage #:hhgbot
;;   (:use #:cl #:positional-lambda))

