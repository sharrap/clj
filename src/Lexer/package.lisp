(defpackage :clj.lexer
  (:use :cl :clj.utils :clj.tok)
  (:export #:Token #:lex-program
           #:token-type #:token-value #:split-rshift))

;;;clj.lexer implements a lexer which is intended to be used token-by-token,
;;;to allow for efficiency and parser hacking.

(load "src/Lexer/Token.lisp")
(load "src/Lexer/DFA.lisp")
(load "src/Lexer/Lexer.lisp")
