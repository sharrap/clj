(in-package :clj.utils)

;;;Utility functions that operate on lists

;;Return a list of the first n elements of ls
(defun take (n ls)
  (if (zerop n)
      NIL
      (cons (car ls) (take (- n 1) (cdr ls)))))

;;Split a list the first time the predicate is true
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

;;Split a list every time the predicate is true
(defun split-when (pred lst &optional (save-split NIL))
  (let ((ans (split-when-1 pred lst save-split)))
    (cond ((not (cadr ans)) (list (car ans)))
          (save-split
           (list* (car ans) (cadr ans) (split-when pred (caddr ans) T)))
          (T (cons (car ans) (split-when pred (cadr ans) NIL))))))

;;Predicate to test whether a character is in a sequence
(defun findchr (ch seq)
  (find ch seq :test #'eql))

;;Make an exact copy of a list, for mutation purposes
(defun copylst (ls)
  (reduce #'cons ls :initial-value nil :from-end t))
