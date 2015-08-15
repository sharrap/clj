(defconstant +operators+ "+-*/%^&|!~<>=")
(defconstant +separators+ "{}()[],.;:?")
(defconstant +eqopers+ '("+" "-" "*" "/" "&" "|" "^" "!" "=" "%"
                         ">>" "<<" ">>>" ">" "<"))
(defconstant +whitespace+ " \n\r\t\v")
(defparameter *keyword-hash* (make-hash-table :test #'equal))
(defparameter *operator-hash* (make-hash-table :test #'equal))

(hash-add-list *keyword-hash*
  '(("abstract" ABSTRACT)
    ("assert" ASSERT)
    ("boolean" BOOLEAN)
    ("break" BREAK)
    ("byte" BYTE)
    ("case" CASE)
    ("catch" CATCH)
    ("char" CHAR)
    ("class" CLASS)
    ("const" CONST)
    ("continue" CONTINUE)
    ("default" DEFAULT)
    ("do" DO)
    ("double" DOUBLE)
    ("else" ELSE)
    ("enum" ENUM)
    ("extends" EXTENDS)
    ("false" FALSE)
    ("final" FINAL)
    ("finally" FINALLY)
    ("float" FLOAT)
    ("for" FOR)
    ("goto" GOTO)
    ("if" IF)
    ("implements" IMPLEMENTS)
    ("import" IMPORT)
    ("instanceof" INSTANCEOF)
    ("int" INT)
    ("interface" INTERFACE)
    ("long" LONG)
    ("native" NATIVE)
    ("new" NEW)
    ("null" NULL)
    ("package" PACKAGE)
    ("private" PRIVATE)
    ("protected" PROTECTED)
    ("public" PUBLIC)
    ("return" RETURN)
    ("short" SHORT)
    ("static" STATIC)
    ("strictfp" STRICTFP)
    ("super" SUPER)
    ("switch" SWITCH)
    ("synchronized" SYNCHRONIZED)
    ("this" THIS)
    ("throw" THROW)
    ("throws" THROWS)
    ("transient" TRANSIENT)
    ("true" TRUE)
    ("try" TRY)
    ("void" VOID)
    ("volatile" VOLATILE)
    ("while" WHILE)))

(hash-add-list *operator-hash*
  '(("<"    LT)
    ("<="   LEQ)
    (">"    GT)
    (">="   GEQ)
    ("<<"   LSHIFT)
    ("<<="  LSHIFTEQ)
    (">>"   RSHIFT)
    (">>="  RSHIFTEQ)
    (">>>"  URSHIFT)
    (">>>=" URSHIFTEQ)
    ("++"   INCR)
    ("--"   DECR)
    ("+"    PLUS)
    ("+="   PLUSEQ)
    ("-"    MINUS)
    ("-="   MINUSEQ)
    ("*"    TIMES)
    ("*="   TIMESEQ)
    ("/"    DIV)
    ("/="   DIVEQ)
    ("%"    MOD)
    ("%="   MODEQ)
    ("="    ASSIGN)
    ("=="   EQ)
    ("!"    NOT)
    ("!="   NOTEQ)
    ("&"    AND)
    ("&&"   BOOLAND)
    ("&="   ANDEQ)
    ("^"    XOR)
    ("^="   XOREQ)
    ("|"    OR)
    ("||"   BOOLOR)
    ("|="   OREQ)))

(defun findchr (ch str)
  (find ch str :test #'eql))

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
        (make-instance 'Token :type 'CHAR :value ans))))

(defun emit-unicode (str)
  (let ((ans (parse-integer str :radix 16)))
    (if (> ans #xffff)
        NIL
        (make-instance 'Token :type 'CHAR :value ans))))

(defun emit-char (str)
  (make-instance 'Token :type 'CHAR :value (char-code (coerce str 'character))))

(defun make-int (base ex str)
  (let* ((ex2 (expt 2 (- ex 1)))
         (ext (* ex2 2))
         (ans (parse-integer str :radix base))
         (ansn (if (and (>= ans ex2) (<= ans (* 2 ex2)) (not (eql base 10)))
                   (- ans ext))))
    (if (or (>= ans ex2) (< ans (- ext)))
        NIL
        (make-instance 'Token :type (if (eql ex 64) 'INTLIT 'LONGLIT) :value ansn))))

(defun parse-int-ls (ls &key (radix 10))
  (parse-integer (concatenate 'string ls) :radix radix))

(defun parse-float-exp (base ls num dec)
  (let ((n (if ls (parse-int-ls ls :radix base) 0)))
    (* (+ num
          (if (eql dec 0) 0
              (let ((e (log dec 10)))
                (/ dec (expt 10 (ceiling e))))))
       (expt 10 n))))

(defun parse-float (ls base)
  (let* ((p (split-when (curry #'eql #\.) ls))
         (head (car p))
         (tail (cdr p))
         (p2 (split-when (curry #'eql #\+) (if tail tail ls)))
         (p3 (if p2 p2 (split-when (curry #'eql #\-) (if tail tail ls))))
         (ht (car p3))
         (hn (if tail head ht)))
    (parse-float-exp base (if p2 (cdr p2) (cons #\- (cdr p3)))
                     (if hn (parse-int-ls hn :radix base) 0)
                     (if (and tail ht) (parse-int-ls ht :radix base) 0))))

(defun make-float (base ty str)
  (let* ((ans (parse-float
               (if (stringp str) (concatenate 'list str) str) base)))
    (make-instance 'Token :type (if (or (eql ty #\f)
                                        (eql ty #\F))
                                    'FLOATLIT 'DOUBLELIT)
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
         (let ((_ (gensym)))
           `(lambda (,_) ,_
              (make-instance 'Token :type ,(cadr arg)
                 :value ,(if mem `(concatenate 'string (reverse ,mem)) 'NIL)))))
        ((eql (car arg) 'emit-with)
         `(,(cadr arg) (concatenate 'string (reverse ,mem))))
        ((eql (car arg) 'emit-with-d)
         (let ((_ (gensym)))
          `(lambda (,_) ,_
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
          ((or (consp arg1) (eql arg1 'T))
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
          (make-instance 'Token :type 'IDENTIFIER :value id))))

(defstate identifier-state ch str
  (alphanumericp record)
  (T emit-with emit-identifier))

(defstate annotation-state ch str
  (alphanumericp record)
  (T emit 'ANNOTATION))

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

(defmacro deffloatexp-state (name emit-test base)
  (let ((main (gensym))
        (g (gensym))
        (under (gensym)))
   `(progn
      (declaim (ftype function ,main))
      (defstate ,under ch str
        (digit-char-p record)
        (is #\_ nextm ,main)
        (T gotom ,main))
      (defstate ,main ch str
        (digit-char-p gotom ,under)
        (,emit-test emit-with-d (lambda (,g) (make-float ,base ch ,g)))
        (T emit-with (lambda (,g) (make-float ,base NIL ,g))))
      (defstate ,name ch str
        (is #\+ (curry (function ,main) (cons #\+ str)))
        (is #\- (curry (function ,main) (cons #\- str)))
        (digit-char-p (curry (function ,main) (cons #\+ str)))
        (,emit-test gotom ,main)
        (T NIL)))))

(defun ftest (ch)
  (findchr ch "fFdD"))
(defun dtest (ch)
  (findchr ch "lL"))

(deffloatexp-state hexfloatexp-state ftest 16)
(deffloatexp-state decfloatexp-state dtest 10)

(defstate hexfloatdot-state ch str
  (hex-digit-p record)
  (is #\p nextm hexfloatexp-state)
  (is #\P nextm hexfloatexp-state)
  (T (make-float 16 NIL (reverse str))))

(defnum-state decnum-state 10 digit-char-p NIL)
(defnum-state octnum-state 8 oct-digit-p NIL)
(defnum-state hexnum-state 16 hex-digit-p
  (is #\p nextm hexfloatexp-state)
  (is #\P nextm hexfloatexp-state)
  (is #\. (curry #'hexfloatdot-state (cons #\. str))))
(defnum-state binnum-state 2 bin-digit-p NIL)

(defstate zero-state ch NIL
  (is #\x nextm hexnum-state)
  (is #\X nextm hexnum-state)
  (is #\b nextm binnum-state)
  (is #\B nextm binnum-state)
  (digit-char-p gotom octnum-state)
  (T (make-int 10 32 "0")))

(defun emit-separator (ch)
  (make-instance 'Token :value NIL :type
    (case ch
      (#\{ 'LBRACE)
      (#\} 'RBRACE)
      (#\[ 'LBRACK)
      (#\] 'RBRACK)
      (#\( 'LPAREN)
      (#\) 'RPAREN)
      (#\, 'COMMA)
      (#\. 'DOT)
      (#\; 'SEMI)
      (#\: 'COLON)
      (#\? 'QUESTION))))

(defun emit-oper (str)
  (make-instance 'token :type (gethash str *operator-hash*) :value NIL))

(defstate separator-state ch NIL
  (T (lambda (_) _ (emit-separator ch))))

(defstate line-comment-state ch str
  (is #\linefeed emit 'COMMENT)
  (is #\return emit 'COMMENT)
  (T record))

(declaim (ftype function block-comment-end-state))

(defstate block-comment-state ch str
  (is #\* nextm block-comment-end-state)
  (T record))

(defstate block-comment-end-state ch str
  (is #\/ emit-d 'COMMENT)
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
  (is #\# emit 'STRING) ;Suppress unused warnings
  (T emit 'STRING))

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

(defstate start-state ch NIL
  (alpha-char-p gotom identifier-state)
  (is #\$ gotom identifier-state)
  (is #\@ nextm annotation-state)
  (is #\0 #'zero-state)
  (digit-char-p gotom decnum-state)
  (in +operators+ (curry #'operator-state (list ch)))
  (in +separators+ (separator-state ch))
  (is #\" nextm string-state)
  (is #\' #'char-state)
  (space-char-p #'start-state)
  (T NIL))
