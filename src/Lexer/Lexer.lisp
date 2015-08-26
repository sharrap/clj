(in-package :clj.lexer)

(defun do-lex (state inp)
  (let ((ans (funcall state (istream-read inp))))
    (cond ((typep ans 'Token) ans)
          (ans (do-lex ans (istream-next inp)))
          (T (progn (istream-next inp))
                    ans))))

(defun lex-next-token (inp)
  (assert (typep inp 'istream))
  (do-lex #'start-state inp))
