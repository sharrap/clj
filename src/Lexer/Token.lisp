(in-package :clj.lexer)

;;A lexed token.
;;Type:  Describes the token,
;;       should be lower case
;;Value: Contains the value of
;;       the token, for applicable tokens
;;       (strings, characters, numbers, identifiers etc)
(defclass Token ()
  ((type :accessor token-type :initform NIL :initarg :type)
   (value :accessor token-value :initform NIL :initarg :value)))

;;Generate two |gt| tokens (when hacking the parser)
;;Intended for use later, when line number information
;;is tracked.
(defun split-rshift (tok)
  (cons (make-instance 'Token :type (reintern '|gt|))
        (make-instance 'Token :type (reintern '|gt|))))
