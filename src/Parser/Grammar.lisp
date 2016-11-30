(in-package :clj.parser)

(defclass Grammar ()
  ((start        :accessor grammar-start        :initform NIL :initarg :start)
   (terminals    :accessor grammar-terminals    :initform NIL :initarg :terminals)
   (nonterminals :accessor grammar-nonterminals :initform NIL :initarg :nonterminals)
   (symbols      :accessor grammar-symbols      :initform NIL :initarg :symbols)
   (productions  :accessor grammar-productions  :initform NIL :initarg :productions)
   (prodmap      :accessor grammar-prodmap      :initform NIL :initarg :prodmap)))

(defclass Production ()
  ((lhs :accessor production-lhs :initform NIL :initarg :lhs)
   (rhs :accessor production-rhs :initform NIL :initarg :rhs)))

(defun print-grammar (grammar)
  (loop :for production :in (grammar-productions grammar)
        :do (format t "~a ~a~%" (production-lhs production) (production-rhs production))))

(defun productions-expanding (grammar symbol)
  (gethash symbol (grammar-prodmap grammar)))

(defun production-equals (p1 p2)
  (and (eq (production-lhs p1) (production-lhs p2))
       (equal (production-rhs p1) (production-rhs p2))))

(defun intern-grammar-symbol (s)
  (if (gethash s *operator-hash*)
      (gethash s *operator-hash*)
      (intern s :clj.lexer)))

(defun read-productions (prodstring)
  (let ((words (mapcar (lambda (x) (mapcar #'intern-grammar-symbol (split-sequence #\  x :remove-empty T)))
                       (split-sequence #\newline prodstring :remove-empty T))))
    (loop :for prod :in words
          :unless (not prod)
          :collect (make-instance 'Production :lhs (car prod) :rhs (cdr prod)))))

(defun make-grammar (productions start)
  (let* ((symbols (list-to-clsset (apply #'append
                                         (mapcar (lambda (x)
                                                  (cons (production-lhs x) (production-rhs x)))
                                                 productions))
                                  #'sxhash #'eq))
         (nonterminals (list-to-clsset (mapcar #'production-lhs productions) #'sxhash #'eq))
         (terminals (clsset-difference symbols nonterminals))
         (prodmap (make-hash-table :test #'equal)))
    (loop :for production :in productions
          :do (setf (gethash (production-lhs production) prodmap) (cons production (gethash (production-lhs production) prodmap))))
    (make-instance 'Grammar :start        start
                            :terminals    terminals
                            :nonterminals nonterminals
                            :symbols      symbols
                            :productions  productions
                            :prodmap      prodmap)))

(defun read-grammar (prodstring start)
  (make-grammar (read-productions prodstring) start))
