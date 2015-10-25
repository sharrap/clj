#!/usr/bin/env sbcl --script

;(load "src/Main.lisp")
(load "src/Utils/package.lisp")
(load "src/Parser/NFA/package.lisp")
(load "src/Parser/NFA/jgrammar.lisp")

(in-package :clj.parser.nfa)



;(defrule S ({A} |b| |c|))
;(defrule A (C |d|))
;(defrule C (B))
;(defrule B (|e|))
;(defrule Q ({|f|}))

;(defterminals |b| |c| |d| |e| |f|)
;(defrule S (S |c| |b|))
;(set-start-nonterminal 'S)

(in-package :cl-user)

(clj.parser.nfa:generate-nfa)
(clj.parser.nfa:dump-nfa "src/Parser/NFA.lisp")
