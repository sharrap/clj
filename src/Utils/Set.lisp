;A simple implementation of a set. Just sits on top of hash tables.

(defmacro make-hash-set (&rest args)
  `(make-hash-table ,args))

(defmacro set-contains (e set)
  `(gethash ,e ,set))

(defmacro set-add (e set)
  `(setf (gethash ,e ,set) T))

(defmacro set-remove (e set)
  `(remhash ,e ,set))
