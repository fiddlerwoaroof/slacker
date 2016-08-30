;;;; hhgbot.asd

(asdf:defsystem #:hhgbot
  :description "Describe hhgbot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (
          #:alexandria
          #:blackbird
          #:carrier
          #:chanl
          #:cl+ssl
          #:cl-js
          #:drakma
          #:fast-http
          #:flexi-streams
          #:fwoar.lisputils
          #:hunchensocket
          #:positional-lambda
          #:plump
          #:puri
          #:serapeum
          #:ubiquitous
          #:websocket-driver
          #:yason
          )
  :serial t
  :components ((:file "package")
               (:file "js-executor")
               (:file "slack-client")
               (:file "test-ui")))

