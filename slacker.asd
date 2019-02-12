;;;; hhgbot.asd
(in-package :asdf-user)

(asdf:defsystem #:slacker
  :description "Describe hhgbot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:blackbird
               #:chanl
               #:cl+ssl
               #:cl-js
               #:drakma
               #:fast-http
               #:flexi-streams
               #:fwoar-lisputils
               #:hunchensocket
               #:positional-lambda
               #:plump
               #:puri
               #:serapeum
               #:ubiquitous
               #:vecto
               #:websocket-driver
               #:yason
               #:vecto
               #:fwoar-event-loop)
  :serial t
  :components ((:file "package")
               (:file "slack-api")
               (:file "event-pump")
               (:file "js-executor")
               (:file "slack-client")))

(defsystem :slacker/montezuma-store 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:slacker
               #:montezuma)
  :serial t
  :components ((:file "montezuma-store")))
