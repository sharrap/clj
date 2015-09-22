(defpackage :clj.lexer
  (:use :cl :clj.utils :clj.tok)
  (:export #:Token #:lex-next-token
           #:token-type #:token-value #:split-rshift))

(load "src/Lexer/Token.lisp")
(load "src/Lexer/DFA.lisp")
(load "src/Lexer/Lexer.lisp")
