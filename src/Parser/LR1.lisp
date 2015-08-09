(defclass action ()
  ((type :accessor action-type :initform NIL :initarg :type)
   (value :accessor action-value :initform NIL :initarg :value)))

(defparameter *production-hash* (make-hash-table))
(defparameter *action-hash* (make-hash-table :test #'equal))

;TODO: Initialize production-hash and action-hash

(defun get-lr-action (state tok)
  (gethash (cons state (symbol-name tok)) *action-hash*))

(defun get-lr-production (n)
  (gethash n *production-hash*))
