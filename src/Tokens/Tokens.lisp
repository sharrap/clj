(in-package :clj.tok)

(defun reintern (sym)
  (let ((sym2 (intern (symbol-name sym))))
    (export sym2)
    sym2))
