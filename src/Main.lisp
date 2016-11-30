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

(defparameter *java-grammar* (clj.parser:read-grammar (file-contents "src/parser/java18.grammar") (intern "CompilationUnit" :clj.lexer)))
(defparameter *test-grammar* (clj.parser:read-grammar
"S expr eof
expr expr + term
expr term
term identifier"
(intern "S" :clj.lexer)))

(defun main ()
  (let* ((tokens (clj.lexer:lex-program *standard-input*))
         (tokens-vec (make-array (length tokens) :initial-contents tokens))
         (grammar *test-grammar*))
    (print-tokens tokens)
    (clj.parser:print-grammar grammar)
    (clj.parser:print-tree (clj.parser:parse-earley grammar tokens-vec))))
