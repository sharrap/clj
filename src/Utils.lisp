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
  (when (not (istream-top is)) (setf (istream-top is)
                                     (read-char (istream-stream is))))
  (istream-top is))

(defun istream-next (is)
  (assert (typep is 'istream))
  (assert (istream-top is))
  (setf (istream-top is) NIL)
  is)

(defun hash-add-list (hash ls)
  (if ls
      (progn (setf (gethash (caar ls) hash) (cadar ls))
             (hash-add-list hash (cdr ls)))
      NIL))


(defun split-when (pred lst)
  (labels ((split-when-int (e ls acc)
             (if ls
                 (if (funcall e (car ls))
                     (cons (reverse acc) (cdr ls))
                     (split-when-int e (cdr ls) (cons (car ls) acc)))
                 (cons e NIL))))
    (split-when-int pred lst NIL)))
