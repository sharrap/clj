(in-package :clj.parser)

(load "src/Parser/NFA.lisp")

(defparameter *productions* clj.parser.nfa:*lrproductions*)
(defparameter *states* clj.parser.nfa:*lrstates*)

(defun parse (inp statestack nodestack)
  ...)

(defun do-parse (inp)
  (parse inp '(0) NIL))
