(defpackage fwoar.safe-reader
  (:use :cl))

(in-package :fwoar.safe-reader)

(defun safe-read (stream)
  )

(with-input-from-string (s "(a b (c d (e f) (g h) e) a s)")
  )
