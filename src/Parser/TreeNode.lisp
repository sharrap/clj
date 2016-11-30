(in-package :clj.parser)

;A node in the parse tree
;value should be a token, while children is a list of trees
(defclass TreeNode ()
  ((value :accessor treenode-value :initform NIL :initarg :value)
   (children :accessor treenode-children :initform NIL :initarg :children)))
