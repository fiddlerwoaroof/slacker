(in-package :cl-user)
(defpackage :foobar-baz (:use :cl))
(in-package :foobar-baz)

(defmacro eval-always (() &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


(eval-always ()
  (ql:quickload :slacker)
  (ql:quickload :slacker/montezuma-store)
  (ql:quickload :slacker/postmodern-store)
  (ql:quickload :data-lens)
  (ql:quickload :osicat)
  (ql:quickload :trivial-shell)
  (ql:quickload :clack)
  (ql:quickload :ningle)
  (ql:quickload :araneus))

(eval-always ()
  (load (compile-file "/home/edwlan/words_coprocess-work/words-coprocess.lisp")))

(eval-always ()
  (load (compile-file (asdf:system-relative-pathname :slacker "hhgbot-2.lisp"))))

#+(or)(eval-always ()
  (load (compile-file "/home/edwlan/words_coprocess-work/words-coprocess.lisp")))
  


