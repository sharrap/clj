(defpackage :clj.parser.nfa
  (:use :cl :clj.utils)
  (:export #:defrule #:defterminals #:generate-nfa #:set-start-nonterminal))

(load "src/Parser/NFA/LRClasses.lisp")
(load "src/Parser/NFA/MakeNFA.lisp")
