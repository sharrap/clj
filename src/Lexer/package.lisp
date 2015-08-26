(defpackage :clj.lexer
  (:use :cl :clj.utils)
  (:export '#:Token '#:lex-next-token))

(load "src/Lexer/Token.lisp")
(load "src/Lexer/DFA.lisp")
(load "src/Lexer/Lexer.lisp")
