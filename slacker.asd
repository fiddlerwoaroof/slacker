;;;; hhgbot.asd
(in-package :asdf-user)

(asdf:defsystem #:slacker
  :description #.(concatenate 'string
			      "A Client for Slack's RTM API"
			      " and wrappers for its REST API")
  :author "Ed L <edward@elangley.org>"
  :license "Apache/v2"
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

(defsystem :slacker/postmodern-store 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:slacker
               #:postmodern)
  :serial t
  :components ((:file "postmodern-store")))
