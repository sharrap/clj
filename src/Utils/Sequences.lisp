(in-package :clj.utils)

(defun uniq (ls)
  (let ((hash (make-hash-table))
        (outls NIL))
    (loop for item in ls do (setf (gethash item hash) T))
    (with-hash-table-iterator (it hash)
      (loop
        (multiple-value-bind (entryp k v) (it)
          v
          (if entryp
              (setf outls (cons k outls))
              (return)))))
    outls))
