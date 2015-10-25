(in-package :clj.utils)

;;;Useful utility functions relating to functional programming

;;Curry a function with some arguments
(defun curry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))

;;Compose exactly two functions
(defun compose2 (f1 f2)
  (lambda (arg)
    (funcall f1 (funcall f2 arg))))

;;Compose arbitrarily many functions
(defun compose (&rest fns)
  (if fns
      (reduce #'compose2 fns)
      #'identity))
