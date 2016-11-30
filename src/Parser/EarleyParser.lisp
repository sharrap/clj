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
    (make-instance 'Item rule dot prev-state subtrees is-complete
                   (if is-complete Nil (elt (production-rhs rule) dot)))))

(defun hash-item (it)
  (* (sxhash rule) (sxhash dot) (sxhash prev-state)))

(defun equal-item (i1 i2)
  (and (production-equals (Item-rule i1) (Item-rule i2))
       (eq (Item-dot i1) (Item-dot i2))
       (eq (Item-prev-state i1) (Item-prev-state i2))))

(defun item-advance (item tree)
  (make-item (item-rule item) (+ (item-dot item) 1) (item-prev-state item) (cons tree (item-subtrees item))))

(defun item-tree (item)
  (make-instance 'TreeNode :value (make-instance 'Token :type (production-lhs (item-rule item)) :value "") :children (reverse (item-subtrees item))))

(defstruct StateSet
  (queue (make-queue))
  (set   (make-clsset #'hash-item #'equal-item)))

(defun ss-dequeue (ss)
  (dequeue (StateSet-queue ss)))

(defun ss-add (ss it)
  (when (get-clsset (StateSet-set ss))
        (setf (get-clsset it (StateSet-set ss)) t)
        (enqueue (StateSet-queue ss) it)))

(defun ss-is-empty (ss)
  (queue-empty (StateSet-queue ss)))

(defun parse-earley (grammar input)
  (let* ((length (length input))
         (state-sets (make-array (+ length 1))))
    (loop :for i :from 0 :to length
          :do (setf (aref state-sets i) (make-instance 'state-set)))
    (loop :for i :from 0 :to length
          :with state-set = (aref state-sets i)
          :do (loop :with item = (ss-dequeue state-set)
                    :while item
                    :if (item-is-complete item) :do
                       (loop :for old-item :in (clsset-to-list (stateset-set (aref state-sets (item-prev-state item))))
                             :when (eq (item-next-symbol old-item) (production-lhs (item-rule item)))
                             :do (ss-add state-set (item-advance item (item-tree item))))
                    :else :do
                       (progn (loop :for production :in (productions-expanding grammar (next-symbol item))
                                    :do (ss-add state-set (make-instance 'Item :rule production :dot 0 :prev-state i)))
                              (when (and (< index length) (eq (token-type (aref input i)) (item-next-symbol item)))
                                    (ss-add (aref state-sets (+ index 1)) (item-advance item (make-instance 'TreeNode :value (aref input i))))))))
    (if (ss-is-empty (aref state-sets length))
        nil
        (item-tree (car (clsset-to-list (ss-set (aref state-sets length))))))))
