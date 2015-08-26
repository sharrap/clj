;A nondeterministic parser based on LR(0).

;Rule forms:
;(a b c)        Accept a b c
;(a <b c> d)    Accept either a b d or a c d
;(a [b c] d)    Accept either a d or a b c d
;(a {b c} d)    Accept a d, a b c d, a b c b c d etc
;These can be nested arbitrarily

(defparameter *rulehash* (make-hash-table))
(defparameter *termhash* (make-hash-set))

;Register a new rule with name and a list of possible forms
(defun register-rule (name forms)
  (setf (gethash name *rulehash*) forms))

;This unbinds {}[] from surrounding letters
(defun comprehend-subexprs (form)
  (labels ((split-brackets (x) (split-when (lambda (y) (findchr y "{}[]")) x T))
           (flatten (l)
             (apply #'append
                    (mapcar (lambda (x)
                              (if (listp x)
                                  (cons '< (append (flatten x)
                                                      (list '>)))
                                  (list x))) l))))
    (mapcar #'intern
            (remove-if (lambda (x) (eql x ""))
                       (apply #'append
                              (mapcar (compose #'split-brackets #'string)
                                      (flatten form)))))))

;Group subexpressions from [], (), {} and rename them to something
;more meaningful (any, optional, repeat)
(defun group-subexprs (frm)
  (labels ((closesym (sym)
             (case sym
                   ('< '>)
                   ('{ '})
                   ('[ '])))
           (identsym (sym)
             (case sym
                   ('> 'any)
                   ('] 'optional)
                   ('} 'repeat)))
           (group (form acc sym)
             (cond ((not form)
                    (if (eql sym NIL)
                        (reverse acc)
                        (error "Unexpected end of input")))
                   ((eql (car form) sym)
                    (list (cons (identsym sym)
                                (reverse acc))
                          (cdr form)))
                   ((or (eql (car form) '<)
                        (eql (car form) '{)
                        (eql (car form) '[))
                    (let ((ans (group (cdr form) NIL (closesym (car form)))))
                      (group (cadr ans) (cons (car ans) acc) sym)))
                   (T (group (cdr form) (cons (car form) acc) sym)))))
    (group frm NIL NIL)))

;Syntax described above, similar to BNF but lisp-y
(defmacro defrule (name &rest forms)
  `(register-rule (quote ,name)
                  (quote ,(mapcar (compose #'group-subexprs
                                           #'comprehend-subexprs)
                                  forms))))

(defmacro defterminals (&rest terminals)
  `(progn ,(mapcar (lambda (x) `(set-add ,x *termhash*)) terminals)))
