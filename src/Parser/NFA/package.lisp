(defpackage :clj.parser.nfa
  (:use :cl :clj.utils)
  (:export :defrule))

(load "src/Parser/NFA/MakeNFA.lisp")
