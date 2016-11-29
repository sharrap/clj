(in-package :clj.utils)

(defstruct q
  (list nil)
  (last nil))

(defun make-queue () (make-instance 'q))

(defun queue-empty (q)
  (not (q-last q)))

(defun enqueue (q el)
  (if (q-list q)
      (let ((last (q-last q)))
        (setf (q-last q) (cons el nil)
              (cdr last) (q-last q)))
      (setf (q-list q) (cons el nil)
            (q-last q) (q-list q))))

(defun dequeue (q)
  (prog1 (car (q-list q))
         (setf (q-list q) (cdr (q-list q)))
         (when (not (q-list q)) (setf (q-last q) nil))))
