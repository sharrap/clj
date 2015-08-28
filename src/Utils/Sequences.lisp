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
  (let ((hash (make-clsset hash-fn equal-fn))
        (outls NIL))
    (loop for ls in lsts do
      (loop for item in ls do
        (when (not (get-clsset item hash))
          (progn
            (setf (get-clsset item hash) T)
            (setf outls (cons item outls))))))
    outls))
