(in-package :clj.lexer)

(defconstant +operators+ "+-*/%^&|!~<>=")
(defconstant +separators+ "{}()[],;?")
(defconstant +eqopers+ '("+" "-" "*" "/" "&" "|" "^" "!" "=" "%"
                         ">>" "<<" ">>>" ">" "<"))
(defconstant +whitespace+ " \n\r\t\v")
(defparameter *keyword-hash* (make-hash-table :test #'equal))
(defparameter *operator-hash* (make-hash-table :test #'equal))

(hash-add-list *keyword-hash*
  (mapcar (lambda (x) (list x (intern x)))
          '("abstract"
            "assert"
            "boolean"
            "break"
            "byte"
            "case"
            "catch"
            "char"
            "class"
            "const"
            "continue"
            "default"
            "do"
            "double"
            "else"
            "enum"
            "extends"
            "false"
            "final"
            "finally"
            "float"
            "for"
            "goto"
            "if"
            "implements"
            "import"
            "instanceof"
            "int"
            "interface"
            "long"
            "native"
            "new"
            "null"
            "package"
            "private"
            "protected"
            "public"
            "return"
            "short"
            "static"
            "strictfp"
            "super"
            "switch"
            "synchronized"
            "this"
            "throw"
            "throws"
            "transient"
            "true"
            "try"
            "void"
            "volatile"
            "while")))

(hash-add-list *operator-hash*
  '(("<"    |lt|)
    ("<="   |leq|)
    (">"    |gt|)
    (">="   |geq|)
    ("<<"   |lshift|)
    ("<<="  |lshiftassign|)
    (">>"   |rshift|)
    (">>="  |rshiftassign|)
    (">>>"  |urshift|)
    (">>>=" |urshiftassign|)
    ("++"   |incr|)
    ("--"   |decr|)
    ("+"    |plus|)
    ("+="   |plusassign|)
    ("-"    |minus|)
    ("-="   |minusassign|)
    ("*"    |times|)
    ("*="   |timesassign|)
    ("/"    |div|)
    ("/="   |divassign|)
    ("%"    |mod|)
    ("%="   |modeq|)
    ("="    |assign|)
    ("=="   |eq|)
    ("!"    |not|)
    ("!="   |noteq|)
    ("&"    |and|)
    ("&&"   |booland|)
    ("&="   |andassign|)
    ("^"    |xor|)
    ("^="   |xorassign|)
    ("|"    |or|)
    ("||"   |boolor|)
    ("|="   |orassign|)
    ("->"   |rarrow|)))

(defun bin-digit-p (ch)
  (or (eql ch #\0) (eql ch #\1)))

(defun oct-digit-p (ch)
  (and (char>= ch #\0) (char<= ch #\7)))

(defun hex-digit-p (ch)
  (or (digit-char-p ch)
      (and (char>= ch #\a) (char<= ch #\f))
      (and (char>= ch #\A) (char<= ch #\F))))

(defun emit-oct (str)
  (let ((ans (parse-integer str :radix 8)))
    (if (> ans 255)
        NIL
        (make-instance 'Token :type '|char| :value ans))))

(defun emit-unicode (str)
  (let ((ans (parse-integer str :radix 16)))
    (if (> ans #xffff)
        NIL
        (make-instance 'Token :type '|char| :value ans))))

(defun emit-char (str)
  (make-instance 'Token :type 'CHAR :value (char-code (char str 0))))

(defun make-int (base ex str)
  (let* ((ex2 (expt 2 (- ex 1)))
         (ext (* ex2 2))
         (ans (parse-integer str :radix base))
         (ansn (if (and (>= ans ex2) (<= ans (* 2 ex2)) (not (eql base 10)))
                   (- ans ext))))
    (if (or (>= ans ex2) (< ans (- ext)))
        NIL
        (make-instance 'Token :type (if (eql ex 64) '|intlit| '|longlit|)
                              :value ansn))))

(defun parse-int-ls (ls &key (radix 10))
  (parse-integer (concatenate 'string ls) :radix radix))

(defun parse-float-exp (base ls num dec)
  (let ((n (if ls (parse-int-ls ls :radix base) 0)))
    (* (+ num
          (if (eql dec 0) 0
              (/ dec (expt base (ceiling (log dec base))))))
       (expt 10 n))))

(defun parse-float (lst base)
  (let* ((ls (if (equal (last lst) '(#\.)) (append lst '(#\0)) lst))
         (p (split-when-1 (curry #'eql #\.) ls))
         (head (car p))
         (tail (cadr p))
         (p2 (split-when-1 (curry #'eql #\-) (if tail tail ls)))
         (p3 (if (cadr p2)
                 p2
                 (split-when-1 (curry #'eql #\+) (if tail tail ls))))
         (p4 (if (cadr p3) p3 (list (if tail tail ls) #\0)))
         (ht (car p4))
         (hn (if tail head ht)))
    (parse-float-exp base (if (cdr p2) (cons #\- (cdr p2)) (cdr p4))
                     (if hn (parse-int-ls hn :radix base) 0)
                     (if (and tail ht) (parse-int-ls ht :radix base) 0))))

(defun make-float (base ty str)
  (let* ((ans (parse-float
               (if (stringp str) (concatenate 'list str) str) base)))
    (make-instance 'Token :type (if (or (eql ty #\f)
                                        (eql ty #\F))
                                    '|floatlit| '|doublelit|)
                          :value ans)))

(defun space-char-p (ch)
  (findchr ch +whitespace+))

(defun defstate-rhs (name ch mem arg)
  (cond ((eql (car arg) 'record)
         `(curry (function ,name) (cons ,(if (cdr arg) (cadr arg) ch) ,mem)))
        ((eql (car arg) 'emit)
         `(make-instance 'Token :type ,(cadr arg)
             :value ,(if mem `(concatenate 'string (reverse ,mem)) NIL)))
        ((eql (car arg) 'emit-d)
         (let ((g (gensym)))
           `(lambda (,g) ,g
              (make-instance 'Token :type ,(cadr arg)
                 :value ,(if mem `(concatenate 'string (reverse ,mem)) 'NIL)))))
        ((eql (car arg) 'emit-with)
         `(,(cadr arg) (concatenate 'string (reverse ,mem))))
        ((eql (car arg) 'emit-with-d)
         (let ((g (gensym)))
          `(lambda (,g) ,g
              (,(cadr arg) (concatenate 'string (reverse ,mem))))))
        ((eql (car arg) 'gotom)
         `(,(cadr arg) ,mem ,ch))
        ((eql (car arg) 'nextm)
         `(curry (function ,(cadr arg)) ,mem))
        (T (car arg))))

(defun defstate-args (name ch mem arg)
  (let ((arg1 (car arg)))
    (cond ((eql arg1 'is)
           `((eql ,ch ,(cadr arg))
             ,(defstate-rhs name ch mem (cddr arg))))
          ((eql arg1 'isf)
           `((and (eql ,ch ,(cadr arg))
                  (,(caddr arg) (concatenate 'string (reverse ,mem))))
             ,(defstate-rhs name ch mem (cdddr arg)))) 
          ((eql arg1 'ismem)
           `((and (eql ,ch ,(cadr arg))
                  (equal ,mem (quote ,(reverse
                                       (concatenate 'list (caddr arg))))))
             ,(defstate-rhs name ch mem (cdddr arg))))
          ((eql arg1 'in)
           `((findchr ,ch ,(cadr arg))
             ,(defstate-rhs name ch mem (cddr arg))))
          ((or (and (consp arg1) (not (eql (car arg1) 'lambda))) (eql arg1 'T))
           `(,(car arg) ,(defstate-rhs name ch mem (cdr arg))))
          (T `((,(car arg) ,ch)
           ,(defstate-rhs name ch mem (cdr arg)))))))

(defmacro defstate (name ch mem &rest args)
  `(defun ,name ,(if mem `(,mem ,ch) `(,ch))
    (cond ,@(mapcar (curry #'defstate-args name ch mem)
                    (remove-if-not #'identity args)))))

(defun emit-identifier (id)
  (let ((v (gethash id *keyword-hash*)))
    (if v (make-instance 'Token :type v :value NIL)
          (make-instance 'Token :type '|identifier| :value id))))

(defstate identifier-state ch str
  (alphanumericp record)
  (T emit-with emit-identifier))

(defmacro defnum-state (name base oracle &rest other)
  (let ((g (gensym))
        (under (gensym))
        (main (gensym)))
    `(progn
      (declaim (ftype function ,under))
      (defstate ,main ch str
        (,oracle record)
        (is #\l emit-with-d (lambda (,g) (make-int ,base 64 ,g)))
        (is #\L emit-with-d (lambda (,g) (make-int ,base 64 ,g)))
        (is #\_ nextm ,under)
        ,@other
        (T emit-with (lambda (,g) (make-int ,base 64 ,g))))
      (defstate ,name ch str
        (,oracle gotom ,main)
        (is #\. gotom ,main)
        (T NIL))
      (defstate ,under ch str
        (is #\_ nextm ,under)
        (,oracle gotom ,main)
        (T NIL)))))

(defun ftest (ch)
  (findchr ch "fFdD"))

(defmacro deffloatexp-state (name base)
  (let* ((main (gensym))
         (g (gensym))
         (under (gensym)))
   `(progn
      (declaim (ftype function ,main))
      (defstate ,under ch str
        (is #\_ nextm ,under)
        (digit-char-p gotom ,main)
        (T NIL))
      (defstate ,main ch str
        (digit-char-p record)
        (is #\_ nextm ,under)
        (ftest emit-with-d (lambda (,g) (make-float ,base ch ,g)))
        (T emit-with (lambda (,g) (make-float ,base NIL ,g))))
      (defstate ,name ch str
        (is #\+ (curry (function ,main) (cons #\+ str)))
        (is #\- (curry (function ,main) (cons #\- str)))
        (digit-char-p (curry (function ,main) (cons #\+ str)))
        (T NIL)))))

(deffloatexp-state hexfloatexp-state 16)
(deffloatexp-state decfloatexp-state 10)

(defmacro deffloatdot-state (name test expstate expteststr base)
  (let* ((under (gensym))
         (main (gensym))
         (g (gensym)))
   `(progn
      (declaim (ftype function ,main))
      (defstate ,under ch str
        (is #\_ nextm ,under)
        (,test gotom ,main)
        (T NIL))
      (defstate ,main ch str
        (,test record)
        (is #\_ nextm ,under)
        ((findchr ch ,expteststr) nextm ,expstate)
        (ftest emit-with-d (lambda (,g) (make-float ,base ch ,g)))
        (T emit-with (lambda (,g) (make-float ,base NIL ,g))))
      (defstate ,name ch str
        (is #\_ NIL)
        (T gotom ,main)))))

(deffloatdot-state hexfloatdot-state hex-digit-p hexfloatexp-state "pP" 16)
(deffloatdot-state decfloatdot-state digit-char-p decfloatexp-state "eE" 10)

(defnum-state decnum-state 10 digit-char-p NIL
  (is #\e nextm decfloatexp-state)
  (is #\E nextm decfloatexp-state)
  (is #\. (curry #'decfloatdot-state (cons #\. str)))
  (ftest emit-with-d (lambda (_) (make-float 10 ch _))))

(defnum-state hexnum-state 16 hex-digit-p
  (is #\p nextm hexfloatexp-state)
  (is #\P nextm hexfloatexp-state)
  (is #\. (curry #'hexfloatdot-state (cons #\. str))))

(defnum-state octnum-state 8 oct-digit-p NIL)
(defnum-state binnum-state 2 bin-digit-p NIL)

(defstate zero-state ch NIL
  (is #\x nextm hexnum-state)
  (is #\X nextm hexnum-state)
  (is #\b nextm binnum-state)
  (is #\B nextm binnum-state)
  (is #\. (curry #'decfloatdot-state (list #\. #\0)))
  (digit-char-p gotom octnum-state)
  (T (make-int 10 32 "0")))

(defun emit-separator (ch)
  (make-instance 'Token :value NIL :type
    (case ch
      (#\{ '|lbrace|)
      (#\} '|rbrace|)
      (#\[ '|lbrack|)
      (#\] '|rbrack|)
      (#\( '|lparen|)
      (#\) '|rparen|)
      (#\, '|comma|)
      (#\; '|semi|)
      (#\? '|question|))))

(defun emit-oper (str)
  (make-instance 'token :type (gethash str *operator-hash*) :value NIL))

(defstate separator-state ch NIL
  (T (lambda (_) _ (emit-separator ch))))

(defstate line-comment-state ch str
  (is #\linefeed emit '|comment|)
  (is #\return emit '|comment|)
  (T record))

(declaim (ftype function block-comment-end-state))

(defstate block-comment-state ch str
  (is #\* nextm block-comment-end-state)
  (T record))

(defstate block-comment-end-state ch str
  (is #\/ emit-d '|comment|)
  (T (block-comment-state (cons #\* str) ch)))

(defstate operator-state ch str
  ((and (digit-char-p ch)
        (eql str '(#\-))) gotom decnum-state)
  (ismem #\< "<" record)
  (ismem #\> ">" record)
  (ismem #\& "&" record)
  (ismem #\| "|" record)
  (ismem #\> ">>" record)
  (ismem #\+ "+" record)
  (ismem #\- "-" record)
  (ismem #\> "-" record)
  (isf #\= (lambda (s) (find s +eqopers+ :test #'equal)) record)
  (ismem #\/ "/" (curry #'line-comment-state NIL))
  (ismem #\* "/" (curry #'block-comment-state NIL))
  (T emit-with emit-oper))

(declaim (ftype function string-state))

(defmacro srecord (ch)
  `(curry #'string-state (cons ,ch str)))

(defstate escaped-string-state ch str
  (is #\" (srecord #\"))
  (is #\n (srecord #\linefeed))
  (is #\r (srecord #\return))
  (is #\f (srecord #\page))
  (is #\b (srecord #\backspace))
  (is #\t (srecord #\tab))
  (is #\\ (srecord #\\))
  (is #\' (srecord #\'))
  (T NIL))

(defstate emit-string-state ch str
  (is #\# emit '|string|) ;Suppress unused warnings
  (T emit '|string|))

(defstate string-state ch str
  (is #\" nextm emit-string-state)
  (is #\\ nextm escaped-string-state)
  (T record))

(defstate done-char-state ch str
  (is #\' emit-with-d emit-char)
  (T NIL))

(defmacro crecord (ch)
  `(curry #'done-char-state (cons ,ch NIL)))

(defstate octal-char-state ch str
  (oct-digit-p record)
  (is #\' emit-with-d emit-oct)
  (T NIL))

(defstate unicode-char-state ch str
  (is #\u nextm unicode-char-state)
  (hex-digit-p record)
  (is #\' emit-with-d emit-unicode)
  (T NIL))

(defstate escaped-char-state ch NIL
  (is #\u nextm unicode-char-state)
  (is #\\ (crecord #\\))
  (is #\" (crecord #\"))
  (is #\' (crecord #\'))
  (is #\t (crecord #\tab))
  (is #\b (crecord #\backspace))
  (is #\n (crecord #\linefeed))
  (is #\r (crecord #\return))
  (is #\f (crecord #\page))
  (T NIL))

(defstate char-state ch NIL
  (is #\' NIL)
  (is #\0 gotom octal-char-state)
  (is #\\ #'escaped-char-state)
  (T (curry #'done-char-state (list ch))))

(defstate twodot-state ch NIL
  (is #\. emit-d '|threedots|)
  (T NIL))

(defstate dot-state ch NIL
  (digit-char-p (curry #'decfloatdot-state (list ch #\. #\0)))
  (is #\. nextm twodot-state)
  (T emit '|dot|))

(defstate colon-state ch NIL
  (is #\: emit-d '|twocolon|)
  (T emit-d '|colon|))

(defstate start-state ch NIL
  (alpha-char-p gotom identifier-state)
  (is #\$ gotom identifier-state)
  (is #\@ emit-d '|at|)
  (is #\0 #'zero-state)
  (digit-char-p gotom decnum-state)
  (is #\. #'dot-state)
  (in +operators+ (curry #'operator-state (list ch)))
  (in +separators+ (separator-state ch))
  (is #\: nextm colon-state)
  (is #\" nextm string-state)
  (is #\' #'char-state)
  (space-char-p #'start-state)
  (T NIL))
