(defpackage :clj.lexer
  (:use :cl :clj.utils)
  (:export #:Token #:lex-next-token
           #:token-type #:token-value))

(load "src/Lexer/Token.lisp")
(load "src/Lexer/DFA.lisp")
(load "src/Lexer/Lexer.lisp")
