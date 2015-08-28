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
(defparameter *termhash* (make-hash-table))
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
                       (reduce #'nconc
                              (mapcar (compose #'split-brackets #'string)
                                      form)
                              :from-end t)))))

;Group subexpressions from [], (), {} and rename them to something
;more meaningful (any, optional, repeat)
(defun group-subexprs (frm)
  (labels ((closesym (sym)
             (ecase sym
                   (< '>)
                   ({ '})
                   ([ '])))
           (identsym (sym)
             (ecase sym
                   (> 'any)
                   (] 'optional)
                   (} 'repeat)))
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
  `(setf (gethash (quote ,terminal) *termhash*) T))

(defmacro defterminals (&rest terminals)
  `(progn
     ,@(mapcar (lambda (x) `(defterminal ,x)) terminals)))

(defun set-start-nonterminal (nonterminal)
  (setf *start-nonterminal* (intern (string nonterminal))))

;Generate the NFA now that all the grammar setup is done
;Just use LR(0) for (relative) simplicity since it's nondeterministic anyways
;(Unused) This was originally written by accident. It isn't necessary,
;but is left around in the event that I suddenly want to start doing LR(1) or something
(defun compute-first-sets (nonterms prodhash)
  (let* ((firsthash (make-hash-table))
         (nullablehash (make-hash-table))
         (referredhash (make-hash-table))
         (worklist nonterms)
         (worklist2 (copylst worklist)))
    (loop for nonterm in nonterms do
     (progn
       (setf (gethash nonterm firsthash) NIL
             (gethash nonterm nullablehash) NIL)
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
             (setf (gethash nonterm nullablehash) T
                   worklist (append (gethash nonterm referredhash)
                                    (cdr worklist)))
             (setf worklist (cdr worklist)))))
      (loop while worklist2 do
       (let* ((nonterm (car worklist2))
              (prods (gethash nonterm prodhash)))
         (labels ((get-first (prod)
                    (when prod
                          (let ((cp (car prod)))
                            (cond ((and (listp cp) (nullablep cp))
                                   (reduce #'nconc (mapcar #'get-first (cdr cp))
                                          :from-end t :initial-value (get-first (cdr prod))))
                                  ((listp cp) (mapcar #'get-first (cdr cp))) 
                                  ((not (gethash cp prodhash)) (list cp))
                                  ((not (nullablep cp)) (gethash cp firsthash))
                                  (T (append (gethash cp firsthash)
                                             (get-first (cdr prod)))))))))
           (let ((firsts (uniq (reduce #'nconc (mapcar #'get-first prods) :from-end t))))
             (if (not (equal firsts (gethash nonterm firsthash)))
                 (setf (gethash nonterm firsthash) firsts
                       worklist2 (append (gethash nonterm referredhash)
                                         (cdr worklist)))
                 (setf worklist2 (cdr worklist2)))))))
      firsthash)))

(defun get-new-items (startitems prodhash)
  (remove-if-not #'identity
    (reduce #'nconc
      (loop for item in startitems collect
        (let* ((lhs (car (lritem-postdot item)))
               (prods (gethash lhs prodhash)))
          (when prods
            (loop for prod in prods collect
              (new-lritem lhs prod)))))
      :from-end t)))

(defun make-lrstate (startitems prodhash)
  (labels ((newstate (items state)
             (let* ((news (get-new-items items prodhash))
                    (state2 (uniq-cls #'lritem-hash #'lritem-equal
                                      state news)))
               (if (eql (length state2) (length state))
                   state
                   (newstate (get-new-items news prodhash) state2)))))
    (new-lrstate (newstate startitems startitems))))

(defun possible-transitions (state)
  (uniq (remove-if-not #'identity
          (loop for item in (lrstate-items state) collect
            (car (lritem-postdot item))))))

(defun follow-transition (state trans)
  (labels ((move-dot (item)
             (make-instance 'lritem :lhs (lritem-lhs item)
                                    :dot (+ (lritem-dot item) 1)
                                    :predot (append (lritem-predot item)
                                                    (list (car (lritem-postdot item))))
                                    :postdot (cdr (lritem-postdot item)))))
    (mapcar #'move-dot
      (remove-if-not (lambda (x) (eql (car (lritem-postdot x)) trans))
        (lrstate-items state)))))

(defun matching-state (states items)
  (with-hash-table-iterator (it states)
    (loop
      (multiple-value-bind (existsp k v) (it)
        (declare (ignore k))
        (cond ((not existsp) (return))
              ((lrstate-contains-items v items) (return v))
              (T NIL))))))

(defparameter *states* (make-hash-table))

(defun compute-transitions (state prodhash)
  (let ((transhash (lrstate-transhash state)))
    (loop for trans in (possible-transitions state) do
      (let* ((follow (follow-transition state trans))
             (match (matching-state *states* follow))
             (target (or match
                         (make-lrstate follow prodhash))))
        (setf (gethash trans transhash) target)
        (when (not match)
          (setf (gethash (lrstate-id target) *states*) target)
          (compute-transitions target prodhash))))))

(defun compute-nfa (prodhash start)
  (let* ((startitems (mapcar (lambda (x) (new-lritem start x))
                             (gethash start prodhash)))
         (startstate (make-lrstate startitems prodhash)))
    (setf (gethash (lrstate-id startstate) *states*) startstate)
    (compute-transitions startstate prodhash)
    (with-hash-table-iterator (it *states*)
      (loop
        (multiple-value-bind (exitp k v) (it)
          (when (not exitp) (return))
          (print v))))))

(defun generate-nfa ()
  (let ((nonterms NIL))
    (with-hash-table-iterator (it *rulehash*)
      (loop
        (multiple-value-bind (entryp k v) (it)
          (declare (ignore v))
          (if entryp
              (setf nonterms (cons k nonterms))
              (return)))))
    (compute-nfa *rulehash* *start-nonterminal*)))
