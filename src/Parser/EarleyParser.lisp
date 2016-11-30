(in-package :clj.parser)

(defstruct Item
  (rule nil)
  (dot 0)
  (prev-state 0)
  (subtrees nil)
  (is-complete nil)
  (next-symbol nil))

(defun make-item (rule dot prev-state &optional (subtrees nil))
  (let ((is-complete (= dot (length (production-rhs rule)))))
    (make-instance 'Item :rule rule :dot dot :prev-state prev-state :subtrees subtrees :is-complete is-complete
                   :next-symbol (if is-complete Nil (elt (production-rhs rule) dot)))))

(defun hash-item (it)
  (with-slots (rule dot prev-state) it
    (+ (sxhash (production-lhs rule)) (apply #'+ (mapcar #'sxhash (production-rhs rule))) (sxhash dot) (sxhash prev-state))))

(defun equal-item (i1 i2)
  (and (production-equals (Item-rule i1) (Item-rule i2))
       (eq (Item-dot i1) (Item-dot i2))
       (eq (Item-prev-state i1) (Item-prev-state i2))))

(defun item-advance (item tree)
  (make-item (item-rule item) (+ (item-dot item) 1) (item-prev-state item) (cons tree (item-subtrees item))))

(defun item-tree (item)
  (make-instance 'TreeNode :value (make-instance 'Token :type (production-lhs (item-rule item)) :value "") :children (reverse (item-subtrees item))))

(defstruct StateSet
  (queue nil)
  (set nil))

(defun make-state-set ()
  (make-instance 'StateSet :queue (make-queue) :set (make-clsset #'hash-item #'equal-item)))

(defun print-state-set (ss)
  (format t "~a~%" (clsset-to-list (StateSet-set ss))))

(defun ss-dequeue (ss)
  (dequeue (StateSet-queue ss)))

(defun ss-add (ss it)
  (when (not (get-clsset it (StateSet-set ss)))
        (setf (get-clsset it (StateSet-set ss)) t)
        (enqueue (StateSet-queue ss) it)))

(defun ss-is-empty (ss)
  (queue-empty (StateSet-queue ss)))

(defun parse-earley (grammar input)
  (let* ((length (length input))
         (state-sets (make-array (+ length 1))))
    (loop :for i :from 0 :to length
          :do (setf (aref state-sets i) (make-state-set)))
    (loop :for production :in (productions-expanding grammar (grammar-start grammar))
          :do (ss-add (aref state-sets 0) (make-item production 0 0)))
    (loop :for i :from 0 :to length
          :for state-set := (aref state-sets i)
          :do (loop :for item := (ss-dequeue state-set)
                    :while item
                    :if (item-is-complete item) :do
                       (loop :for old-item :in (clsset-to-list (stateset-set (aref state-sets (item-prev-state item))))
                             :when (eq (item-next-symbol old-item) (production-lhs (item-rule item)))
                             :do (ss-add state-set (item-advance old-item (item-tree item))))
                    :else :do
                       (progn (loop :for production :in (productions-expanding grammar (item-next-symbol item))
                                    :do (ss-add state-set (make-item production 0 i)))
                              (when (and (< i length) (eq (token-type (aref input i)) (item-next-symbol item)))
                                    (ss-add (aref state-sets (+ i 1)) (item-advance item (make-instance 'TreeNode :value (aref input i))))))))
    (let ((items (clsset-to-list (stateset-set (aref state-sets length)))))
      (if items
          (item-tree (car items))
          items))))
