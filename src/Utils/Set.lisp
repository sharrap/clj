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
