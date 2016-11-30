(load "src/Tokens/package.lisp")
(load "src/Utils/package.lisp")
(load "src/Lexer/package.lisp")
(load "src/Parser/package.lisp")

(defun print-tokens (tokens)
  (loop :for token :in tokens
        :do (format t "~a ~a~%" (clj.lexer:token-type token) (clj.lexer:token-value token))))

(defun file-contents (file)
  (with-open-file (stream file)
    (loop :for line := (read-line stream nil)
          :while line
          :collect line :into lines
          :finally (reduce #'(lambda (x y) (concatenate 'string x (string #\newline) y)) lines))))

(defun main ()
  (let* ((tokens (clj.lexer:lex-program *standard-input*))
         (tokens-vec (make-array (length tokens) :initial-contents tokens))
         (grammar (clj.parser:read-grammar (file-contents "src/Parser/java18.grammar") '|CompilationUnit|)))
    (format t "foo~%")))
