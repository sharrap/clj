(defpackage :clj.parser.nfa
  (:use :cl :clj.utils)
  (:export #:defrule #:defterminals #:generate-nfa #:set-start-nonterminal
           #:dump-nfa))

(load "src/Parser/NFA/LRClasses.lisp")
(load "src/Parser/NFA/MakeNFA.lisp")
(load "src/Parser/NFA/DumpNFA.lisp")
;(load "src/Parser/NFA/jgrammar.lisp")
