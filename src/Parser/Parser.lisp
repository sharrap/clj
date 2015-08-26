(in-package :clj.parser)

(defun get-next-token (inp)
  (if (consp inp)
      (car inp)
      (lex-next-token inp)))

(defun drop-first-token (inp)
  (if (consp inp)
      (cdr inp)
      inp))

(defun do-parse (statestack symstack inp)
  (let* ((tok (get-next-token inp))
         (act (get-lr-action (car statestack) (token-type tok))))
    (case (action-type act)
      (REJECT NIL)
      (ACCEPT (car symstack))
      (SHIFT (if (eql (token-type tok) 'EOF)
                 (car symstack)
                 (do-parse
                   (cons (action-value act) statestack)
                   (cons (make-instance 'TreeNode :value tok
                                                  :children NIL)
                         symstack)
                   (drop-first-token inp))))
      (REDUCE
       (let* ((prod (get-lr-production (action-value act)))
              (n (- (length prod) 1)))
         (do-parse (nthcdr n statestack)
                   (cons (make-instance 'TreeNode :value prod
                                                  :children (take n symstack))
                         (nthcdr n symstack))
                   (cons (make-instance 'Token :type 'SYNTHETIC
                                               :value (car prod))
                         inp)))))))

(defun parse (inp)
  (do-parse '(0) '() inp))
