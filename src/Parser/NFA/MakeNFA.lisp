;A nondeterministic parser based on LR(0).
;Grammar is written in a LISP file and macroexpands into the appropriate
;functions to generate the grammar

(in-package :clj.parser.nfa)

;Rule forms:
;(a b c)        Accept a b c
;(a <b c> d)    Accept either a b d or a c d
;(a [b c] d)    Accept either a d or a b c d
;(a {b c} d)    Accept a d, a b c d, a b c b c d etc
;These can be nested arbitrarily

(defparameter *rulehash* (make-hash-table))
(defparameter *termhash* (make-hash-set))
(defparameter *start-nonterminal* NIL)

;for debugging/testing
(defun print-hash (firsthash)
  (format t "----------~%")
  (with-hash-table-iterator (it firsthash)
    (loop
      (multiple-value-bind (entryp k v) (it)
        (if entryp
          (format t "Key: ~a Value: ~a~%" k v)
          (return))))))


(defun check-validity (form)
  (labels ((check-validity2 (f)
             (when (not f) (error "Cannot have empty subexpressions"))
             (mapcar (compose #'check-validity2 #'cdr)
                     (remove-if-not #'listp f))))
    (mapcar #'check-validity2 (remove-if-not #'identity form))))

;Register a new rule with name and a list of possible forms
(defun register-rule (name forms)
  (loop for form in forms do (check-validity forms))
  (setf (gethash name *rulehash*) (append forms (gethash name *rulehash*))))

;This unbinds {}[]<> from surrounding letters
(defun comprehend-subexprs (form)
  (labels ((split-brackets (x)
             (split-when (lambda (y) (findchr y "{}[]<>")) x T)))
    (mapcar #'intern
            (remove-if (lambda (x) (eql x ""))
                       (apply #'append
                              (mapcar (compose #'split-brackets #'string)
                                      form))))))

;Group subexpressions from [], (), {} and rename them to something
;more meaningful (any, optional, repeat)
(defun group-subexprs (frm)
  (labels ((closesym (sym)
             (ecase sym
                   ('< '>)
                   ('{ '})
                   ('[ '])))
           (identsym (sym)
             (ecase sym
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

(defmacro defterminal (terminal)
  `(set-add (quote ,terminal) *termhash*))

(defmacro defterminals (&rest terminals)
  `(progn
     ,@(mapcar (lambda (x) `(defterminal ,x)) terminals)))

(defun set-start-nonterminal (nonterminal)
  (setf *start-nonterminal* nonterminal))

;Generate the NFA now that all the grammar setup is done
;Just use LR(0) for (relative) simplicity since it's nondeterministic anyways
(defun compute-first-sets (nonterms prodhash)
  (let* ((firsthash (make-hash-table))
         (nullablehash (make-hash-table))
         (referredhash (make-hash-table))
         (worklist nonterms)
         (worklist2 (copylst worklist)))
    (loop for nonterm in nonterms do
     (progn
       (setf (gethash nonterm firsthash) NIL)
       (setf (gethash nonterm nullablehash) NIL)
       (loop for prod in (gethash nonterm prodhash) do
        (loop for term in prod do
         (when (gethash term prodhash)
               (setf (gethash term referredhash)
                     (cons nonterm (gethash term referredhash))))))))
    (labels ((nullablep (term)
               (cond ((not (listp term)) (gethash term nullablehash))
                     ((eql (car term) 'any) (some #'nullablep (cdr term)))
                     (T T))))
      (loop while worklist do
       (let* ((nonterm (car worklist))
              (prods (gethash nonterm prodhash)))
         (if (and (not (gethash nonterm nullablehash))
                  (some (lambda (x) (every #'nullablep x)) prods))
             (progn
               (setf (gethash nonterm nullablehash) T)
               (setf worklist (append (gethash nonterm referredhash)
                                      (cdr worklist))))
             (setf worklist (cdr worklist)))))
      (loop while worklist2 do
       (let* ((nonterm (car worklist2))
              (prods (gethash nonterm prodhash)))
         (labels ((get-first (prod)
                    (when prod
                          (let ((cp (car prod)))
                            (cond ((and (listp cp) (nullablep cp))
                                   (apply #'append (get-first (cdr prod))
                                          (mapcar #'get-first (cdr cp))))
                                  ((listp cp) (mapcar #'get-first (cdr cp))) 
                                  ((not (gethash cp prodhash)) (list cp))
                                  ((not (nullablep cp)) (gethash cp firsthash))
                                  (T (append (gethash cp firsthash)
                                             (get-first (cdr prod)))))))))
           (let ((firsts (uniq (apply #'append (mapcar #'get-first prods)))))
             (if (not (equal firsts (gethash nonterm firsthash)))
                 (progn
                   (setf (gethash nonterm firsthash) firsts)
                   (setf worklist2 (append (gethash nonterm referredhash)
                                           (cdr worklist2))))
                 (setf worklist2 (cdr worklist2)))))))
      firsthash)))

(defun generate-nfa ()
  (let ((nonterms NIL))
    (with-hash-table-iterator (it *rulehash*)
      (loop
        (multiple-value-bind (entryp k v) (it)
          (if entryp
              (setf nonterms (cons k nonterms))
              (return)))))
  ;TODO
    (print-hash (compute-first-sets nonterms *rulehash*))))
