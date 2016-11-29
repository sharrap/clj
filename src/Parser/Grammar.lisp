(in-package :clj.parser)

(defclass Grammar ()
  ((start        :accessor grammar-start        :initform NIL :initarg :start)
   (terminals    :accessor grammar-terminals    :initform NIL :initarg :terminals)
   (nonterminals :accessor grammar-nonterminals :initform NIL :initarg :nonterminals)
   (symbols      :accessor grammar-symbols      :initform NIL :initarg :symbols)
   (productions  :accessor grammar-productions  :initform NIL :initarg :productions)))

(defclass Production ()
  ((lhs :accessor production-lhs :initform NIL :initarg :lhs)
   (rhs :accessor production-rhs :initform NIL :initarg :rhs)))

(defun production-equals (p1 p2)
  (and (eq (production-lhs p1) (production-lhs p2))
       (equal (production-rhs p1) (production-rhs p2))))

(defun read-productions (prodstring)
  (let ((words (mapcar (lambda (x) (mapcar intern (split-sequence #\ x) :remove-empty T))
                       (split-sequence #\newline prodstring :remove-empty T))))
    (loop :for prod :in words
          :unless (not prod)
          :collect (make-instance 'Production :lhs (car prod) :rhs (cdr prod)))))

(defun make-grammar (productions start)
  (let ((symbols (list-to-clsset (apply #'append
                                        (mapcar (lambda (x)
                                                 (cons (production-lhs x) (production-rhs x)))
                                                productions))))
        (nonterminals (list-to-clsset (mapcar production-lhs productions) #'sxhash #'eq))
        (terminals (clsset-difference symbols nonterminals)))
    (make-instance 'Gramar :start        start
                           :terminals    terminals
                           :nonterminals nonterminals
                           :symbols      symbols
                           :productions  productions)))

(defun read-grammar (prodstring start)
  (make-grammar (read-productions prodstring) start))
