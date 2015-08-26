(defpackage :clj.parser
  (:use :cl :clj.utils))

(load "src/Parser/TreeNode.lisp")
(load "src/Parser/LR1.lisp")
(load "src/Parser/Parser.lisp")
