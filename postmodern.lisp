(defpackage :slacker.postgres-store
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export #:postgres-store))
(in-package :slacker.postgres-store)

(defclass postgres-store ()
  ((%connection-string :reader connection-string :initarg :postgres-connection-string))
  (:default-initargs :index-path nil))
