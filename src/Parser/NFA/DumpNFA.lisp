(in-package :clj.parser.nfa)

(defparameter *rev-productions* (make-clshash #'list-hashf #'list-eqf))

(defun dump-item (item)
  `(make-instance 'lritem
    :lhs ,(lritem-lhs item)
    :dot ,(lritem-dot item)
    :predot ,(lritem-predot item)
    :postdot ,(lritem-postdot item)))

(defun dump-transitions (state)
  (let ((outl NIL))
    (with-hash-table-iterator (it (lrstate-transhash state))
      (loop
        (multiple-value-bind (entryp k v) (it)
          (if entryp
              (setf outl (cons (cons k (lrstate-id v)) outl))
              (return)))))
    outl))

(defun dump-reductions (state)
  (mapcar
    (lambda (x)
        (get-clshash (cons (lritem-lhs x) (lritem-predot x)) *rev-productions*))
    (lrstate-reductions state)))

(defun dump-state (state)
  `(deflrstate ,(lrstate-id state)
     :shift ,(dump-transitions state)
     :reduce ,(dump-reductions state)))

(defun dump-nfa (outf-name)
  (with-open-file (outf (pathname outf-name)
                   :direction :output
                   :if-exists :supersede)
    (with-hash-table-iterator (it *productions*)
      (loop
        (multiple-value-bind (entryp k v) (it)
          (if entryp
              (progn
                (prin1 `(defproduction ,k ,(car v) ,(cdr v)) outf)
                (setf (get-clshash v *rev-productions*) k)
                (terpri outf))
              (return)))))
    (with-hash-table-iterator (it *states*)
      (loop
        (multiple-value-bind (entryp k v) (it)
          (declare (ignore k))
          (if entryp
              (progn (prin1 (dump-state v) outf) (terpri outf))
              (return)))))))
