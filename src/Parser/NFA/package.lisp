(defpackage :clj.parser.nfa
  (:use :cl :clj.utils :clj.tok)
  (:export #:defrule #:defterminals #:generate-nfa #:set-start-nonterminal
           #:dump-nfa #:deflrstate #:defproduction
           #:lrproduction #:lrproduction-index #:lrproduction-lhs
           #:lrproduction-len #:lrproduction-rhs
           #:lrnfastate #:lrfnastate-index
           #:lrnfastate-shift #:lrnfastate-reduce
           #:*lrproductions* #:*lrstates* #:*lrterminals*))

(load "src/Parser/NFA/LRClasses.lisp")
(load "src/Parser/NFA/MakeNFA.lisp")
(load "src/Parser/NFA/DumpNFA.lisp")
(load "src/Parser/NFA/NFA.lisp")
;(load "src/Parser/NFA/jgrammar.lisp")
