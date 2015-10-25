(in-package :clj.tok)

;;Move a designated symbol from its home package to
;;the clj.tok package, and provide it to the rest
;;of the world
(defun reintern (sym)
  (let ((sym2 (intern (symbol-name sym))))
    (export sym2)
    sym2))
