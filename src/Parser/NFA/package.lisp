(defpackage :clj.parser.nfa
  (:use :cl :clj.utils)
  (:export #:defrule #:defterminals #:generate-nfa #:set-start-nonterminal
           #:dump-nfa #:deflrstate #:defproduction
           #:lrproduction #:lrproduction-index #:lrproduction-lhs #:lrproduction-rhs
           #:lrnfastate #:lrnfastate-index #:lrnfastate-shift #:lrnfasate-reduce))

(load "src/Parser/NFA/LRClasses.lisp")
(load "src/Parser/NFA/MakeNFA.lisp")
(load "src/Parser/NFA/DumpNFA.lisp")
(load "src/Parser/NFA/NFA.lisp")
;(load "src/Parser/NFA/jgrammar.lisp")
