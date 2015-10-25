(in-package :clj.parser.nfa)

(defparameter *lrproductions* (make-hash-table))
(defparameter *lrstates* (make-hash-table))
(defparameter *lrterminals* (make-hash-table))

(defclass lrproduction ()
  ((index :accessor lrproduction-index :initform NIL :initarg :index)
   (lhs   :accessor lrproduction-lhs   :initform NIL :initarg :lhs)
   (rhs   :accessor lrproduction-rhs   :initform NIL :initarg :rhs)
   (len   :accessor lrproduction-len   :initform NIL :initarg :len)))

(defclass lrnfastate ()
  ((index  :accessor lrnfastate-index  :initform NIL :initarg :index)
   (shift  :accessor lrnfastate-shift  :initform NIL :initarg :shift)
   (reduce :accessor lrnfastate-reduce :initform NIL :initarg :reduce)))

(defmacro defproduction (idx lhs rhs)
  `(setf (gethash ,idx *lrproductions*)
         (make-instance 'lrproduction :index ,idx :lhs (reintern (quote ,lhs))
                                      :rhs (mapcar #'reintern (quote ,rhs))
                                      :len ,(length rhs))))

(defun deflrterminals (&rest terminals)
  (loop for terminal in terminals do
    (setf (gethash (reintern terminal) *lrterminals*) T)))

(defun deflrterminal (terminal)
  (setf (gethash (reintern terminal) *lrterminals*) T))

(defmacro deflrstate (idx &key (shift NIL) (reduce NIL))
  `(setf (gethash ,idx *lrstates*)
         (make-instance 'lrnfastate
                        :index ,idx
                        :shift (hash-from-list
                                  (mapcar (lambda (x)
                                            (cons (reintern (car x)) (cdr x)))
                                          (quote ,shift)))
                        :reduce (quote ,reduce))))
