(defpackage :clj.parser
  (:use :cl :clj.utils :clj.lexer))

(load "src/Parser/TreeNode.lisp")
(load "src/Parser/LR1.lisp")
(load "src/Parser/Parser.lisp")
(load "src/Parser/RunNFA.lisp")
