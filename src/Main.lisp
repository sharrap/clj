(load "src/Tokens/package.lisp")
(load "src/Utils/package.lisp")
(load "src/Lexer/package.lisp")
(load "src/Parser/package.lisp")

(defun start (stream)
  (let ((ans (clj.lexer:lex-next-token stream)))
    (if ans
        (progn (format T "Token Name: ~a, Value: ~a~%"
                       (clj.lexer:token-type ans)
                       (clj.lexer:token-value ans))
               (clj.utils:istream-next stream)
               (start stream))
        NIL)))

(defun print-tokens (tokens)
  (loop :for token :in tokens
        :do (format t "~a ~a~%" (clj.lexer:token-type token) (clj.lexer:token-value token))))

(defun main ()
  (let ((stream (make-instance 'clj.utils:istream :stream *standard-input* :top NIL)))
    (print-tokens (clj.lexer:lex-program *standard-input*))))
    ;(clj.parser:parse-earley stream)))
    ;(start stream)))
