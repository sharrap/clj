(in-package :clj.utils)

(defparameter *hashmod* 7919)

(defun list-eqf (x y)
  (equal x y))

(defun list-hashf (ls)
  (mod (if (listp ls)
           (apply #'+ (mapcar #'list-hashf ls))
           (sxhash ls))
       *hashmod*))

(defclass class-hash ()
  ((internal-hash :accessor internal-hash
                  :initform (make-hash-table :test #'eql)
                  :initarg  :internal-hash)
   (hashf         :accessor hashf
                  :initform (make-hash-table :test #'eql)
                  :initarg  :hashf)
   (eqf           :accessor eqf
                  :initform (lambda (x y) x y t)
                  :initarg  :eqf)))

(defun make-clshash (hash-fn eq-fn)
  (make-instance 'class-hash :hashf hash-fn :eqf eq-fn))

(defun find-entry (item eqf ls)
  (cond ((not ls) NIL)
        ((funcall eqf (caar ls) item) (car ls))
        (T (find-entry item eqf (cdr ls)))))

(defun get-clshash (item hash)
  (with-slots (internal-hash hashf eqf) hash
    (let* ((hashv (funcall hashf item))
           (entry (find-entry item eqf (gethash hashv internal-hash))))
      (values (cdr entry) (if entry T NIL)))))

(defun (setf get-clshash) (val item hash)
  (with-slots (internal-hash hashf eqf) hash
    (let* ((hashv (funcall hashf item))
           (v (gethash hashv internal-hash))
           (prev (car (find-entry item eqf v))))
      (setf (gethash hashv internal-hash)
            (cons (cons item val)
                  (remove-if (lambda (x) (eql x prev)) v))))))
