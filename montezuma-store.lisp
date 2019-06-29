(defpackage :slacker.montezuma-store
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export
   #:montezuma-store
   #:search-index
   #:retire-open-indices))
(in-package :slacker.montezuma-store)

(defclass montezuma-store ()
  ((%indexes :reader indexes
	     :initform (make-hash-table :test #'equal :synchronized t))
   (%montezuma-index-path :reader index-path
			  :initarg :index-path))
  (:default-initargs :index-path nil))

(defun retire-open-indices (client)
  (sb-ext:with-locked-hash-table ((indexes client))
    (let ((old-indexes (indexes client)))
      (setf (slot-value client '%indexes) 
	    (make-hash-table :test #'equal :synchronized t))
      (alexandria:maphash-values (lambda (v) (montezuma:close v)) 
				 old-indexes))))

(defun search-index (store index text)
  (let* ((index (ensure-index-for-type store index)))
    (values
     (montezuma:search index
		       (format nil "!text:\";arc\" text:~a" text)
		       :num-docs 3)
     index)))

(defun ensure-index-for-type (store type)
  (sb-ext:with-locked-hash-table ((indexes store))
    (ensure-gethash type (indexes store)
		   (if (index-path store)
		       (make-instance 'montezuma:index
				      :path (ensure-directories-exist
					     (format nil "~a/~a/"
						     (index-path store)
						     type))
				      :create-p nil
				      :create-if-missing-p t)
		       (make-instance 'montezuma:index)))))

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
    (let* ((type (gethash "type" message))
           (index (ensure-index-for-type store type)))
      (montezuma:add-document-to-index index
				       (flatten-hash-table message))
      (montezuma:flush index))))

(defmethod slacker:handle-message :before
    (type (event-pump montezuma-store) ts channel message)
  (declare (ignore type ts channel))
  (store-message event-pump message)
  (values))
