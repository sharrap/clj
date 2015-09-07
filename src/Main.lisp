(load "src/Utils/package.lisp")
(load "src/Lexer/package.lisp")
(load "src/Parser/package.lisp")
(load "src/Parser/NFA/package.lisp")

(defun start (stream)
  (let ((ans (clj.lexer:lex-next-token stream)))
    (if ans
        (progn (format T "Token Name: ~a, Value: ~a"
                           (clj.lexer:token-type ans) (clj.lexer:token-value ans))
               (start stream))
        NIL)))

(defun main ()
  (let ((stream (make-instance 'istream :stream *standard-input* :top NIL)))
    (start stream)))
