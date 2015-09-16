(in-package :clj.parser.nfa)

(defclass lritem ()
  ((lhs :accessor lritem-lhs :initform NIL :initarg :lhs)
   (dot :accessor lritem-dot :initform 0 :initarg :dot)
   (predot :accessor lritem-predot :initform NIL :initarg :predot)
   (postdot :accessor lritem-postdot :initform NIL :initarg :postdot)))

(defmethod print-object ((obj lritem) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a -> ~a . ~a" (lritem-lhs obj) (lritem-predot obj)
                                   (lritem-postdot obj))))

(declaim (ftype function lrstate-id))

(let ((x 0))
  (defun try-free-lrstate (state)
    (when (eql (- x 1) (lrstate-id state))
      (decf x 1)))
  (defun uniq-id ()
    (prog1 x (incf x 1))))

(defclass lrstate ()
  ((id        :accessor lrstate-id :initform (uniq-id) :initarg :id)
   (items     :accessor lrstate-items :initform NIL :initarg :items)
   (itemset   :accessor lrstate-itemset :initform NIL :initarg :itemset)
   (transhash :accessor lrstate-transhash :initform (make-hash-table)
              :initarg :transhash)
   (reductions :accessor lrstate-reductions :initform NIL :initarg :reductions)))

(defun render-transitions (lrs)
  (let ((str ""))
    (with-hash-table-iterator (it (lrstate-transhash lrs))
      (loop
        (multiple-value-bind (entryp k v) (it)
          (if entryp
              (setf str
                    (concatenate 'string str (symbol-name k) " --> " (write-to-string (lrstate-id v)) ", "))
              (return)))))
    str))

(defmethod print-object ((obj lrstate) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a: [~a]~%Transitions:~%~a" (lrstate-id obj) (lrstate-items obj) (render-transitions obj))))


(defun lritem-equal (item1 item2)
  (and (eql (lritem-lhs item1) (lritem-lhs item2))
       (equalp (lritem-predot item1) (lritem-predot item2))
       (eql (lritem-dot item1) (lritem-dot item2))
       (equalp (lritem-postdot item1) (lritem-postdot item2))))

(defun lrstate-equal (state1 state2)
  (eql (lrstate-id state1) (lrstate-id state2)))

(defun lritem-hash (item)
  (apply #'+
    (mapcar #'sxhash
      (list (lritem-lhs item) (lritem-predot item)
            (lritem-dot item) (lritem-postdot item)))))

(defun lrstate-hash (state)
  (apply #'+ (mapcar #'lritem-hash (lrstate-items state))))

(defun clsset-from-items (items)
  (let ((clsset (make-clsset #'lritem-hash #'lritem-equal)))
    (loop for item in items do
      (setf (get-clsset item clsset) T))
    clsset))

(defun lrstate-contains-items (state items)
  (with-slots (itemset) state
    (not (remove-if (lambda (x) (get-clsset x itemset)) items))))

(defun lrstate-contains-only (state items)
  (with-slots (itemset) state
    (and (eql (length items) (length (lrstate-items state)))
         (lrstate-contains-items state items))))

(defun new-lrstate (items)
  (make-instance 'lrstate :items items
                          :itemset (clsset-from-items items)
                          :reductions (remove-if-not
                                       (lambda (x) (lritem-postdot x)) items)))

(defun new-lritem (lhs prod)
  (make-instance 'lritem :lhs lhs :postdot prod))
