(in-package :clj.utils)

(defun uniq (&rest lsts)
  (let ((hash (make-hash-table))
        (outls NIL))
    (loop for ls in lsts do
      (loop for item in ls do (setf (gethash item hash) T)))
    (with-hash-table-iterator (it hash)
      (loop
        (multiple-value-bind (entryp k v) (it)
          v
          (if entryp
              (setf outls (cons k outls))
              (return)))))
    outls))

;Generalized uniq which supports comparison of arbitrary types.
;Requires a hashing function and an equality test be provided
(defun uniq-cls (hash-fn equal-fn &rest lsts)
  (let ((hash (make-hash-table))
        (outls NIL))
    (loop for ls in lsts do
      (loop for item in ls do
        (let ((hashc (funcall hash-fn item)))
          (multiple-value-bind (v existsp) (gethash hashc hash)
            (when (not (and existsp (find item v :test equal-fn)))
              (progn
                (setf (gethash hashc hash) (cons item v))
                (setf outls (cons item outls))))))))
    outls))
