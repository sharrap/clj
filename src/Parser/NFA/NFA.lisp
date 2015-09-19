(in-package :clj.parser.nfa)

(defparameter *lrproductions* (make-hash-table))
(defparameter *lrstates* (make-hash-table))

(defclass lrproduction ()
  ((index :accessor lrproduction-index :initform NIL :initarg :index)
   (lhs   :accessor lrproduction-lhs   :initform NIL :initarg :lhs)
   (rhs   :accessor lrproduction-rhs   :initform NIL :initarg :rhs)))

(defclass lrnfastate ()
  ((index  :accessor lrnfastate-index  :initform NIL :initarg :index)
   (shift  :accessor lrnfastate-shift  :initform NIL :initarg :shift)
   (reduce :accessor lrnfastate-reduce :initform NIL :initarg :reduce)))

(defmacro defproduction (idx lhs rhs)
  `(setf (gethash *lrproductions* idx)
         (make-instance 'lrproduction :index ,idx :lhs ,lhs :rhs ,rhs)))

(defun fold-into-hashtable (item hash)
  (setf (gethash (car item) hash) (cdr item))
  hash)

(defmacro deflrstate (idx &key (shift NIL) (reduce NIL))
  `(setf (gethash *lrstates* idx)
         (make-instance 'lrnfastate
                        :index ,idx
                        :shift ,(reduce #'fold-into-hashtable shift
                                        :initial-value (make-hash-table))
                        :reduce ,reduce)))
