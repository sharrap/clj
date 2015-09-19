(in-package :clj.utils)

(defun curry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))

(defun take (n ls)
  (if (zerop n) NIL (cons (car ls) (take (- n 1) (cdr ls)))))

(defclass istream ()
  ((stream :accessor istream-stream :initform NIL :initarg :stream)
   (top :accessor istream-top :initform NIL :initarg :top)))

(defun istream-read (is)
  (assert (typep is 'istream))
  (when (not (istream-top is))
    (setf (istream-top is) (list (read-char (istream-stream is)))))
  (istream-top is))

(defun istream-next (is)
  (assert (typep is 'istream))
  (assert (istream-top is))
  (setf (istream-top is) (cdr (istream-top is)))
  is)

(defun (setf istream-next) (val is)
  (assert (typep is 'istream))
  (setf (istream-top is) (cons val (istream-top is)))
  is)

(defun hash-add-list (hash ls)
  (if ls
      (progn (setf (gethash (caar ls) hash) (cadar ls))
             (hash-add-list hash (cdr ls)))
      NIL))


(defun split-when-1 (pred lst &optional (save-split NIL))
  (labels ((split-when-int (e ls acc)
             (cond ((not ls) (list (reverse acc)))
                   ((not (funcall e (car ls)))
                    (split-when-int e (cdr ls) (cons (car ls) acc)))
                   (save-split (list (reverse acc) (list (car ls)) (cdr ls)))
                   (T          (list (reverse acc) (cdr ls))))))
    (if (stringp lst)
        (mapcar (curry #'concatenate 'string)
                (split-when-int pred (concatenate 'list lst) NIL))
        (split-when-int pred lst NIL))))

(defun split-when (pred lst &optional (save-split NIL))
  (let ((ans (split-when-1 pred lst save-split)))
    (cond ((not (cadr ans)) (list (car ans)))
          (save-split
           (list* (car ans) (cadr ans) (split-when pred (caddr ans) T)))
          (T (cons (car ans) (split-when pred (cadr ans) NIL))))))

(defun findchr (ch str)
  (find ch str :test #'eql))

;Compose two functions
(defun compose2 (f1 f2)
  (lambda (arg)
    (funcall f1 (funcall f2 arg))))

(defun compose (&rest fns)
  (if fns
      (reduce #'compose2 fns)
      #'identity))

(defun copylst (ls)
  (reduce #'cons ls :initial-value nil :from-end t))
