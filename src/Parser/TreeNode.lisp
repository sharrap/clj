(in-package :clj.parser)

;A node in the parse tree
;value should be a token, while children is a list of trees
(defclass TreeNode ()
  ((value :accessor treenode-value :initform NIL :initarg :value)
   (children :accessor treenode-children :initform NIL :initarg :children)))

(defun print-tree (node &optional (spaces 0))
  (format t (concatenate 'string (make-array spaces :initial-element #\ )))
  (format t "~a ~a~%" (token-type (treenode-value node)) (token-value (treenode-value node)))
  (loop :for child :in (treenode-children node) :do (print-tree child (+ spaces 2))))
