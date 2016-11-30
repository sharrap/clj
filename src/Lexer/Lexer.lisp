(in-package :clj.lexer)

;;Lex an entire program and return a list of tokens
(defun lex-program (inp)
  (loop :for c := (read-char inp nil :eof) :then (if (typep token 'Token) c (read-char inp nil :eof))
        :for token := (funcall #'start-state (if (eq c :eof) #\newline c)) :then (funcall (if (or (not token) (typep token 'Token)) #'start-state token) (if (eq c :eof) #\newline c))
        :when (typep token 'Token) :collect token :into tokens
        :when (eq c :eof) :do (return (if (or (not token) (typep token 'Token)) (append tokens (list (make-instance 'Token :type '|eof| :value nil))) nil))))
