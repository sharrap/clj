(in-package :clj.parser)

(defclass TreeNode ()
  ((value :accessor treenode-value :initform NIL :initarg :value)
   (children :accessor treenode-children :initform NIL :initarg :children)))
