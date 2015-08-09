(load "src/Utils.lisp")
(load "src/Lexer/Token.lisp")
(load "src/Lexer/DFA.lisp")
(load "src/Lexer/Lexer.lisp")

(defun start (stream)
  (let ((ans (lex-next-token stream)))
    (if ans
        (progn (format T "Token Name: ~a, Value: ~a"
                           (token-type ans) (token-value ans))
               (start stream))
        NIL)))

(defun main ()
  (let ((stream (make-instance 'istream :stream *standard-input* :top NIL)))
    (start stream)))
