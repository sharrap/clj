(in-package :clj.parser)

(defstruct Item
  ((rule nil)
   (dot 0)
   (prev-state 0)
   (is-complete nil)
   (next-symbol nil)))

(defun (make-item rule dot prev-state)
  (let ((is-complete (= dot (length (production-rhs rule)))))
    (make-instance 'Item rule dot prev-state is-complete
                   (if is-complete Nil (elt (production-rhs rule) dot)))))

(defun hash-item (it)
  (* (sxhash rule) (sxhash dot) (sxhash prev-state)))

(defun equal-item (i1 i2)
  (and (production-equals (Item-rule i1) (Item-rule i2))
       (eq (Item-dot i1) (Item-dot i2))
       (eq (Item-prev-state i1) (Item-prev-state i2))))

(defstruct StateSet
  ((queue (make-queue))
   (set   (make-clsset #'hash-item #'equal-item))))

(defun ss-dequeue (ss)
  (dequeue (StateSet-queue ss)))

(defun ss-add (ss it)
  (when (get-clsset (StateSet-set ss))
        (setf (get-clsset it (StateSet-set ss)) t)
        (enqueue (StateSet-queue ss) it)))

(defun ss-is-empty (ss)
  (queue-empty (StateSet-queue ss)))
