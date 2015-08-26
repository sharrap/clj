(defpackage :clj.parser.nfa
  (:use :cl :clj.utils)
  (:export #:defrule #:defterminals #:generate-nfa))

(load "src/Parser/NFA/MakeNFA.lisp")
