(in-package :clj.lexer)

(defconstant +operators+ "+-*/%^&|!~<>=")
(defconstant +separators+ "{}()[],;?@")
(defconstant +eqopers+ '("+" "-" "*" "/" "&" "|" "^" "!" "=" "%"
                         ">>" "<<" ">>>" ">" "<"))
(defconstant +whitespace+ " \n\r\t\v")
(defparameter *keyword-hash* (make-hash-table :test #'equal))
(defparameter *operator-hash* (make-hash-table :test #'equal))

(hash-add-list
  (mapcar (lambda (x) (cons x (intern x)))
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
            "while"))
  *keyword-hash*)

(hash-add-list
  '(("<"    . |lt|)
    ("<="   . |leq|)
    (">"    . |gt|)
    (">="   . |geq|)
    ("<<"   . |lshift|)
    ("<<="  . |lshiftassign|)
    (">>"   . |rshift|)
    (">>="  . |rshiftassign|)
    (">>>"  . |urshift|)
    (">>>=" . |urshiftassign|)
    ("++"   . |incr|)
    ("--"   . |decr|)
    ("+"    . |plus|)
    ("+="   . |plusassign|)
    ("-"    . |minus|)
    ("-="   . |minusassign|)
    ("*"    . |times|)
    ("*="   . |timesassign|)
    ("/"    . |div|)
    ("/="   . |divassign|)
    ("%"    . |mod|)
    ("%="   . |modassign|)
    ("="    . |assign|)
    ("=="   . |eq|)
    ("!"    . |not|)
    ("!="   . |noteq|)
    ("&"    . |and|)
    ("&&"   . |booland|)
    ("&="   . |andassign|)
    ("^"    . |xor|)
    ("^="   . |xorassign|)
    ("|"    . |or|)
    ("||"   . |boolor|)
    ("|="   . |orassign|)
    ("<>"   . |diamond|)
    ("->"   . |rarrow|)
    ("{"    . |lbrace|)
    ("}"    . |rbrace|)
    ("["    . |lbrack|)
    ("]"    . |rbrack|)
    ("("    . |lparen|)
    (")"    . |rparen|)
    (","    . |comma|)
    (";"    . |semi|)
    ("@"    . |at|)
    ("?"    . |question|)
    (":"    . |colon|)
    ("::"   . |twocolons|)
    ("."    . |dot|)
    ("..."  . |threedots|))
  *operator-hash*)

(loop for hash in (list *operator-hash* *keyword-hash*) do
  (with-hash-table-iterator (it hash)
    (loop
      (multiple-value-bind (entryp k v) (it)
        (if entryp
            (reintern v)
            (return))))))

;;Useful predicates
(defun bin-digit-p (ch)
  (or (eql ch #\0) (eql ch #\1)))

(defun oct-digit-p (ch)
  (and (char>= ch #\0) (char<= ch #\7)))

(defun hex-digit-p (ch)
  (or (digit-char-p ch)
      (and (char>= ch #\a) (char<= ch #\f))
      (and (char>= ch #\A) (char<= ch #\F))))

(defun space-char-p (ch)
  (findchr ch +whitespace+))

;;Separators are (roughly) all characters which appear completely on their own
;;and need not be separated by whitespace.
(defun emit-separator (ch)
  (make-instance 'Token :value NIL :type
    (gethash (string ch) *operator-hash*)))

;;Operators are characters used in expressions (roughly)
;;This distinction is a bit hairy. The main difference is that
;;operators may need to be joined (ie < < vs <<).
(defun emit-oper (str)
  (make-instance 'token :type (gethash str *operator-hash*) :value NIL))

;;Identifier admitter. Checks if it can emit a keyword instead.
(defun emit-identifier (id)
  (let ((v (gethash id *keyword-hash*)))
    (if v (make-instance 'Token :type v :value NIL)
          (make-instance 'Token :type '|identifier| :value id))))

;;Token emission functions for chars
;;Emit a token given a string of digits in the appropriate base.
(defun emit-oct (str)
  (let ((ans (parse-integer str :radix 8)))
    (if (> ans 255)
        NIL
        (make-instance 'Token :type '|charlit| :value ans))))

(defun emit-unicode (str)
  (let ((ans (parse-integer str :radix 16)))
    (if (> ans #xffff)
        NIL
        (make-instance 'Token :type '|charlit| :value ans))))

(defun emit-char (str)
  (make-instance 'Token :type '|charlit| :value (char-code (char str 0))))

;;Numeric emission functions
;;Emit numeric tokens of a specified type via a digit string in
;;the appropriate base.
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

;;Part of the 'defstate' macro defined below.
;;Describes a simple DSL for writing DFA states.
;;This function handles the right hand side (roughly equivalent to the
;;action in a cond expression).
;;Actions:
;;  record:      Save the token to the state's internal memory and continue.
;;  emit:        Produce a token of the appropriate type and return to start.
;;  emit-d:      Upon receiving the next input, ignore it and produce a token
;;               of the appropriate type.
;;  emit-with:   Pass the current memory to a specified function and
;;               expect it to produce a token.
;;  emit-with-d: A delayed version of emit-with above.
;;  gotom:       Pass to a specified state and reconsidered the given char,
;;               saving the char in the state's memory.
;;  nextm:       Save the char in the state's memory and then transition to
;;               the specified state.
;;  <default>:   Execute the provided code.
;;
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

;;Part of the 'defstate' macro below.
;;This function defines the left hand side (roughly equivalent to the
;;condition in a cond expression) of the various state transitions.
;;Actions:
;;    is:        Check if the character equals a specific char
;;    isf:       Check if the character equals a specific char and also
;;               if the contents of memory satisfy a particular predicate.
;;    ismem:     Check if the character equals a specific char and also
;;               if the contents of memory thus far are a specific string.
;;    in:        Check if the character is an element of a sequence.
;;    <a list>:  Execute the provided function exactly as specified.
;;    <default>: The provided value is expected to be a unary function which
;;               returns a boolean value. Execute it with the current char
;;               as its argument.
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

;;Defines a new DFA state.
;;Arguments:
;;  name: The name of the state (also its function name when it is defun'd).
;;  ch:   The name of the character variable passed to the function.
;;  mem:  The name of the memory variable for the state, or NIL if no memory
;;        is required.
;;  args: A list of state transitions, described in defstate-args and
;;        defstate-rhs above.
(defmacro defstate (name ch mem &body args)
  `(defun ,name ,(if mem `(,mem ,ch) `(,ch))
    (cond ,@(mapcar (curry #'defstate-args name ch mem)
                    (remove-if-not #'identity args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Actual DFA states follow
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstate identifier-state ch str
  (alphanumericp record)
  (T emit-with emit-identifier))

;;Different numeric types have to handle a fairly similar set of situations
;;(type specificiers at the end, different bases, possibility to be recognized
;;as a float, etc).
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

;;Tracks the exponentiation part of floats.
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

;;Continues reading floating point numbers after the period
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

;;Matches decimal numbers
(defnum-state decnum-state 10 digit-char-p NIL
  (is #\e nextm decfloatexp-state)
  (is #\E nextm decfloatexp-state)
  (is #\. (curry #'decfloatdot-state (cons #\. str)))
  (ftest emit-with-d (lambda (_) (make-float 10 ch _))))

;;Matches hexadecimal numbers
(defnum-state hexnum-state 16 hex-digit-p
  (is #\p nextm hexfloatexp-state)
  (is #\P nextm hexfloatexp-state)
  (is #\. (curry #'hexfloatdot-state (cons #\. str))))

;;Octal and Binary numbers don't admit floating point
(defnum-state octnum-state 8 oct-digit-p NIL)
(defnum-state binnum-state 2 bin-digit-p NIL)

;;Zero could be a few things: A number, the beginning of a float,
;;the beginning of an octal number, or the beginning of a hexadecimal
;;number.
(defstate zero-state ch NIL
  (is #\x nextm hexnum-state)
  (is #\X nextm hexnum-state)
  (is #\b nextm binnum-state)
  (is #\B nextm binnum-state)
  (is #\. (curry #'decfloatdot-state (list #\. #\0)))
  (digit-char-p gotom octnum-state)
  (T (make-int 10 32 "0")))

;;This isn't a very interesting state per se, it just
;;gets the separator emission logic out of the main state.
(defstate separator-state ch NIL
  (T (lambda (_) _ (emit-separator ch))))

;;Line comment--eat characters until you see a newline.
(defstate line-comment-state ch str
  (is #\linefeed emit '|comment|)
  (is #\return emit '|comment|)
  (T record))

;;The block comment states bounce back and forth when *s are read
;;to accurately parse block comments.
(declaim (ftype function block-comment-end-state))

(defstate block-comment-state ch str
  (is #\* nextm block-comment-end-state)
  (T record))

(defstate block-comment-end-state ch str
  (is #\/ emit-d '|comment|)
  (T (block-comment-state (cons #\* str) ch)))

;;Operator state: mostly responsible for joining together multi-character
;;operators given shorter versions.
(defstate operator-state ch str
  ((and (digit-char-p ch)
        (eql str '(#\-))) gotom decnum-state)
  (ismem #\< "<" record)
  (ismem #\> "<" record)
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

;;Properly records any escaped characters in strings.
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

;;Records string literals
(defstate string-state ch str
  (is #\" emit-d '|strlit|)
  (is #\\ nextm escaped-string-state)
  (T record))

;;Records characters, including escaped characters
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

;;Identify an octal vs non-octal char.
;;Unicode needs to be identified later.
(defstate char-state ch NIL
  (is #\' NIL)
  (is #\0 gotom octal-char-state)
  (is #\\ #'escaped-char-state)
  (T (curry #'done-char-state (list ch))))

;;Special state to identify the ... type.
(defstate twodot-state ch NIL
  (is #\. emit-d '|threedots|)
  (T NIL))

;;Identify the difference between raw .s, floats, and
;;...s
(defstate dot-state ch NIL
  (digit-char-p (curry #'decfloatdot-state (list ch #\. #\0)))
  (is #\. nextm twodot-state)
  (T emit '|dot|))

;;Identify the difference between colons and double colons
(defstate colon-state ch NIL
  (is #\: emit-d '|twocolons|)
  (T emit-d '|colon|))

;;The main starting point for all tokens.
;;Mostly serves as a dispatcher to simplify things.
(defstate start-state ch NIL
  (alpha-char-p gotom identifier-state)
  (is #\$ gotom identifier-state)
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
