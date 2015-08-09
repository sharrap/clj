(defclass Token ()
  ((type :accessor token-type :initform NIL :initarg :type)
   (value :accessor token-value :initform NIL :initarg :value)))
