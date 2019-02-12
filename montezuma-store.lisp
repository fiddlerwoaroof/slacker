(defpackage :slacker.montezuma-store
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export ))
(in-package :slacker.montezuma-store)

(defclass montezuma-store ()
  ((%indexes :reader indexes :initform (make-hash-table :test #'equal))))

(defun ensure-index-for-type (store type)
  (ensure-gethash type (indexes store)
                  (make-instance 'montezuma:index)))

(defgeneric combine-child (parent key value)
  (:method (parent k child)
    (setf (gethash k parent) child))
  (:method (parent k (child string))
    (setf (gethash k parent) child))
  (:method (parent k (children list))
    (combine-child parent k (coerce children 'vector)))
  (:method (parent k (children vector))
    (map nil
         (lambda (idx child)
           (combine-child parent
                          (concat k "::" (princ-to-string idx))
                          child))
         (iota (length children))
         children))
  (:method ((parent hash-table) k (child hash-table))
    (do-hash-table (sk sv child)
      (combine-child parent (concat k "::" sk) sv))))

(defun flatten-hash-table (hash-table)
  (let ((new (fw.lu:empty-hash-table-like hash-table)))
    (do-hash-table (k v hash-table new)
      (combine-child new k v))))


(defgeneric store-message (store message)
  (:method ((store montezuma-store) message)
    (let ((type (gethash "type" message)))
      (montezuma:add-document-to-index (ensure-index-for-type store type)
                                       (flatten-hash-table message)))))

(defmethod slacker:handle-message :before (type (event-pump montezuma-store) ts channel message)
  (declare (ignore type ts channel))
  (store-message event-pump message)
  (values))
