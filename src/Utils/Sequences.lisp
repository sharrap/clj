(in-package :clj.utils)

;;;Utility functions on sequences

;;Remove duplicates from a list of simple types in linear time
;;Does not preserve order
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

;Split a sequence according to a delimiter
(defun split-sequence (delimiter sequence &key (remove-empty Nil))
  (let ((length (length sequence)))
    (loop :for left := 0 :then (+ right 1)
          :for right := (or (position delimiter sequence :start left) length)
          :unless (and (= left right) remove-empty)
          :collect (subseq sequence left right)
          :until (>= right length))))
