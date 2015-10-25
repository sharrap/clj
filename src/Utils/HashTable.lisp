(in-package :clj.utils)

;;;Useful functions relating to hashtables

(defparameter *hashmod* 7919)

;;Equal and hash functions for lists
(defun list-eqf (x y)
  (equal x y))

(defun list-hashf (ls)
  (mod (if (listp ls)
           (apply #'+ (mapcar #'list-hashf ls))
           (sxhash ls))
       *hashmod*))

;;A generalized hashtable which can accept any input given an appropriate
;;test and hashing function
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

;;Find matching entries in a list of candidates
;;with the same hash value
(defun find-entry (item eqf ls)
  (cond ((not ls) NIL)
        ((funcall eqf (caar ls) item) (car ls))
        (T (find-entry item eqf (cdr ls)))))

;;Get an item from a clshash
(defun get-clshash (item hash)
  (with-slots (internal-hash hashf eqf) hash
    (let* ((hashv (funcall hashf item))
           (entry (find-entry item eqf (gethash hashv internal-hash))))
      (values (cdr entry) (if entry T NIL)))))

;;Put an item into a clshash
(defun (setf get-clshash) (val item hash)
  (with-slots (internal-hash hashf eqf) hash
    (let* ((hashv (funcall hashf item))
           (v (gethash hashv internal-hash))
           (prev (car (find-entry item eqf v))))
      (setf (gethash hashv internal-hash)
            (cons (cons item val)
                  (remove-if (lambda (x) (eql x prev)) v))))))

;;Generalized hash-add-list function
;;which supports both normal and cls hashes
;;The input list should be a list of pairs (not two-element lists!)
(defmacro def-h-add-list (name hashfn)
  `(defun ,name (ls hash)
     (loop for item in ls do
       (setf (,hashfn (car item) hash) (cdr item)))
     hash))

(def-h-add-list hash-add-list gethash)
(def-h-add-list clshash-add-list get-clshash)

;;Generate a hash table from a list of pairs (not two-element lists!)
(defun hash-from-list (elements)
  (let ((hash (make-hash-table)))
    (hash-add-list elements hash)
    hash))
