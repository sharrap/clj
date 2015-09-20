(in-package :clj.utils)

(defun curry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))

(defun compose2 (f1 f2)
  (lambda (arg)
    (funcall f1 (funcall f2 arg))))

(defun compose (&rest fns)
  (if fns
      (reduce #'compose2 fns)
      #'identity))
