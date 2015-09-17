(in-package :clj.parser.nfa)

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

(defun dump-state (state)
  `(deflrstate ,(lrstate-id state)
     ,(mapcar #'dump-item (lrstate-items state))
     ,(dump-transitions state)
     ,(mapcar #'dump-item (lrstate-reductions state))))

(defun dump-nfa (outf-name)
  (with-open-file (outf (pathname outf-name)
                   :direction :output
                   :if-exists :supersede)
    (with-hash-table-iterator (it *rulehash*)
      (loop
        (multiple-value-bind (entryp k v) (it)
          (if entryp
              (loop for production in v do
                (prin1
                  `(defproduction ,k ,production)
                  outf)
                (terpri outf))
              (return)))))
    (with-hash-table-iterator (it *states*)
      (loop
        (multiple-value-bind (entryp k v) (it)
          (declare (ignore k))
          (if entryp
              (progn (prin1 (dump-state v) outf) (terpri outf))
              (return)))))))
