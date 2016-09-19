(in-package :hhgbot-augmented-assistant)

(defmacro define-constructor (name (class &rest args))
  `(defun ,name (source-hash-table)
     (make-instance ',class
		    ,@(mapcan (lambda (arg) (list (make-keyword arg)
						  `(gethash ,(symbol-name arg)
							    source-hash-table)))
			      args))))

(defclass user ()
  ((id :reader id :initarg :id)
   (name :reader name :initarg :name)
   (presence :accessor presence :initarg :presence)
   (deleted :reader deleted :initarg :deleted)
   (color :reader color :initarg :color)
   (profile :reader profile :initarg :profile)
   (is_admin :reader is_admin :initarg :is_admin)
   (is_owner :reader is_owner :initarg :is_owner)
   (is_primary_owner :reader is_primary_owner :initarg :is_primary_owner)
   (is_restricted :reader is_restricted :initarg :is_restricted)
   (is_ultra_restricted :reader is_ultra_restricted :initarg :is_ultra_restricted)
   (has_2fa :reader has_2fa :initarg :has_2fa)
   (two_factor_type :reader two_factor_type :initarg :two_factor_type)
   (has_files :reader has_files :initarg :has_files)))

(define-constructor make-user
    (user id name deleted color profile
	  is_admin is_owner is_primary_owner is_restricted is_ultra_restricted
	  has_2fa two_factor_type has_files presence))

(defmethod print-object ((o user) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~a: ~a" (id o) (name o))))

(defun format-users (client &optional (stream t))
  (format stream "~&~:{~a: ~{~19<~a~>~^ ~}~%~}"
          (stable-sort
            (sort
              (loop for id being the hash-keys of (users client) using (hash-value user)
                    collect (list id (list (name user) (presence user))))
              #'string-lessp
              :key #'caadr)
            #'string-lessp
            :key #'cadadr)))

