(in-package :clj.parser)

(defparameter *productions* clj.parser.nfa:*lrproductions*)
(defparameter *states* clj.parser.nfa:*lrstates*)
(defparameter *terminals* clj.parser.nfa:*lrterminals*)

(get-possible-reduce-fn (inp statestack nodestack)
  (lambda (x &aux (prod (gethash x *prodhash*)) (plen (lrproduction-len prod)))
    (lambda (callback)
      (funcall callback
               inp
               (nthcdr plen statestack)
               (cons (list prod (take plen nodestack))
                     (nthcdr plen nodestack))))))

(get-possible-shift-fn (inp statestack nodestack nextt n &key (hack NIL))
  (lambda (callback)
    (istream-next inp)
    (let ((resl (funcall callback
                         inp
                         (cons n statestack)
                         (if (typep nextt 'clj.lexer:Token)
                             (cons nextt nodestack)
                             nodestack))))
      (if resl
          resl
          (progn
            (if hack
              (progn
                (istream-next inp)
                (setf (istream-next inp) hack))
              (setf (istream-next inp) nextt))
            NIL)))))

(defun get-possible-move-fns (inp statestack nodestack)
  (let* ((rhs (mapcar (get-possible-reduce-fn inp statestack nodestack)
                      (lrnfastate-reduce (car statestack))))
         (tok (istream-read inp))
         (sym (if (typep tok 'clj.lexer:Token) (token-type tok) tok))
         (lhs (gethash sym (lrnfastate-shift (car statestack)))))
    (cond (lhs
            (cons (get-possible-shift-fn inp statestack nodestack sym lhs)
                  rhs))
          ((and (eql sym 'clj.tok:|rshift|)
                (gethash 'clj.tok:|gt| (lrnfastate-shift (car statestack))))
           (istream-next inp)
           (let ((s (split-rshift tok)))
             (setf (istream-next is) (cdr s))
             (setf (istream-next is) (car s)))
           ;Attempt to hack the parser
           (cons (get-possible-shift-fn inp statestack nodestack sym lhs
                                        :hack tok)
                 rhs))
          (T rhs))))

(defun try-parse (moves)
  (when moves
    (let ((resl (funcall (car moves) #'parse)))
      (if resl
          resl
          (try-parse (cdr moves))))))

(defun parse (inp statestack nodestack)
  (let ((moves (get-possible-reduce-fns inp statestack nodestack))
    (try-parse moves))))

(defun do-parse (inp)
  (parse inp (list (gethash 0 *states*)) NIL))
