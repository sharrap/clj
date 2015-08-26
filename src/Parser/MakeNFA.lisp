;A nondeterministic parser based on LR(0).

;Rule forms:
;(a b c)          Accept a b c
;(a (b c) d)   Accept either a b d or a c d
;(a [b c] d)    Accept either a d or a b c d
;(a {b c} d)    Accept a d, a b c d, a b c b c d etc
;These can be nested arbitrarily

;Register a new rule with name and a list of possible forms
(defun register-rule (name forms)
  (format T "~a: ~a~%" name forms))

;Using '|(| etc messes with vim's bracket matching and I can't
;be bothered to fix it.
(defconstant +lparensym+ (intern "("))
(defconstant +rparensym+ (intern ")"))

;This unbinds {}[] from surrounding letters
(defun comprehend-subexprs (form)
  (labels ((split-brackets (x) (split-when (lambda (y) (findchr y "{}[]")) x T))
           (flatten (l)
             (apply #'append
                    (mapcar (lambda (x)
                              (if (listp x)
                                  (cons +lparensym+ (append (flatten x)
                                                      (list +rparensym+)))
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
                   (+lparensym+ +rparensym+)
                   ('{ '})
                   ('[ '])))
           (identsym (sym)
             (case sym
                   (+rparensym+ 'any)
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
                   ((or (eql (car form) +lparensym+)
                        (eql (car form) '{)
                        (eql (car form) '[))
                    (let ((ans (group (cdr form) NIL (closesym (car form)))))
                      (group (cadr ans) (cons (car ans) acc) sym)))
                   (T (group (cdr form) (cons (car form) acc) sym)))))
    (group frm NIL NIL)))

;Written as a macro to allow rules to be written in a natural, unquoted manner
(defmacro defrule (name &rest forms)
  `(register-rule (quote ,name)
                  (quote ,(mapcar (compose #'group-subexprs
                                           #'comprehend-subexprs)
                                  forms))))
