(in-package :clj.lexer)

(defclass Token ()
  ((type :accessor token-type :initform NIL :initarg :type)
   (value :accessor token-value :initform NIL :initarg :value)))

(defun split-rshift (tok)
  (cons (make-instance 'Token :type (reintern '|gt|))
        (make-instance 'Token :type (reintern '|gt|))))
