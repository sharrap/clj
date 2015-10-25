(in-package :clj.utils)

;;;A utility stream class, which allows reading of the same
;;;element twice, as well as placing elements on top of the stream

(defclass istream ()
  ((stream :accessor istream-stream :initform NIL :initarg :stream)
   (top :accessor istream-top :initform NIL :initarg :top)))

;;Check the top of the stream
(defun istream-read (is)
  (assert (typep is 'istream))
  (when (not (istream-top is))
    (setf (istream-top is) (list (read-char (istream-stream is)))))
    (car (istream-top is)))

;;Discard the top of the stream
(defun istream-next (is)
   (assert (typep is 'istream))
   (assert (istream-top is))
   (setf (istream-top is) (cdr (istream-top is)))
   is)

;;Push on top of the stream
(defun (setf istream-next) (val is)
  (assert (typep is 'istream))
  (setf (istream-top is) (cons val (istream-top is)))
  is)
