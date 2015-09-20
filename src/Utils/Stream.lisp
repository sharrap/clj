(in-package :clj.utils)

(defclass istream ()
  ((stream :accessor istream-stream :initform NIL :initarg :stream)
   (top :accessor istream-top :initform NIL :initarg :top)))

(defun istream-read (is)
  (assert (typep is 'istream))
  (when (not (istream-top is))
    (setf (istream-top is) (list (read-char (istream-stream is)))))
    (istream-top is))

(defun istream-next (is)
   (assert (typep is 'istream))
   (assert (istream-top is))
   (setf (istream-top is) (cdr (istream-top is)))
   is)

(defun (setf istream-next) (val is)
  (assert (typep is 'istream))
  (setf (istream-top is) (cons val (istream-top is)))
  is)
