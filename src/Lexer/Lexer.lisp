(in-package :clj.lexer)

;;Lex a token by repeatedly calling the current state on the next input.
(defun do-lex (state inp)
  (let ((ans (funcall state (istream-read inp))))
    (cond ((typep ans 'Token) ans)
          (ans (do-lex ans (istream-next inp)))
          (T (progn (istream-next inp))
                    ans))))

;;Lex a token from the input straem and return it.
;;If the top of the input stream is already a token, do nothing.
(defun lex-next-token (inp)
  (assert (typep inp 'istream))
  (when (characterp (istream-read inp))
    (setf (istream-next inp) (do-lex #'start-state inp)))
  (istream-read inp))
