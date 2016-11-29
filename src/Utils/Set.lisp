(in-package :clj.utils)

;;;A utility Set class which is simply implemented as a hash table
;;;with T values

;;;Like clshash, supports arbitrary keys provided a hashing function
;;;and equality predicate.

(defclass class-set ()
  ((internal-hash :accessor internal-hash
                  :initform (make-hash-table :test #'eql)
                  :initarg  :internal-hash)
   (hashf         :accessor hashf
                  :initform (lambda (x) x 0)
                  :initarg  :hashf)
   (eqf           :accessor eqf
                  :initform (lambda (x y) x y t)
                  :initarg  :eqf)))

(defun make-clsset (hash-fn eq-fn)
  (make-instance 'class-set :hashf hash-fn :eqf eq-fn))

(defun get-clsset (item hash)
  (with-slots (internal-hash hashf eqf) hash
    (let ((hashv (funcall hashf item)))
      (find item (gethash hashv internal-hash) :test eqf))))

;Returns garbage.
;setf to T to store the item in the set, setf to NIL to remove it.
(defun (setf get-clsset) (val item hash)
  (with-slots (internal-hash hashf eqf) hash
    (let* ((hashv (funcall hashf item))
           (v (gethash hashv internal-hash)))
      (cond ((find item v :test eqf)
             (when (not val)
                   (setf (gethash hashv internal-hash)
                         (remove-if (lambda (x) (funcall eqf x item)) v))))
            (val (setf (gethash hashv internal-hash) (cons item v)))
            (T NIL)))))

(defun list-to-clsset (lst hash-fn eq-fn)
  (let ((set (make-clsset hash-fn eq-fn)))
    (loop :for item :in lst
          :do (setf (get-clsset item set) T))))

;Assume that the two have the same hash and equality functions
(defun clsset-union (set1 set2)
  (let ((result (make-clsset (hashf set1) (eqf set1))))
    (loop :for item :in `(,set1 ,set2)
          :do (with-hash-table-iterator (iterator (internal-hash item))
                (loop
                  (multiple-value-bind (entry-p key value) (iterator)
                    (declare (ignore value))
                    (if entry-p (setf (get-clsset key result) T) (return))))))
    result))

(defun clsset-intersection (set1 set2)
  (let ((result (make-clsset (hashf set1) (eqf set1))))
    (with-hash-table-iterator (iterator (internal-hash set1))
      (loop
        (multiple-value-bind (entry-p key value) (iterator)
          (declare (ignore value))
          (if entry-p (setf (get-clsset key result) (get-clsset key set2)) (return)))))
    result))

(defun clsset-difference (set1 set2)
  (let ((result (clsset-union set1 (make-clsset (hashf set1) (eqf set1)))))
    (with-hash-table-iterator (iterator (internal-hash set2))
      (loop
        (multiple-value-bind (entry-p key value) (iterator)
          (declare (ignore value))
          (if entry-p (setf (get-clsset key result) nil) (return)))))
    result))

