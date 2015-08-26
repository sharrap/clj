;A simple implementation of a set. Just sits on top of hash tables.

(in-package :clj.utils)

(defmacro make-hash-set (&rest args)
  (if args
      `(make-hash-table ,args)
      `(make-hash-table)))

(defmacro set-contains (e set)
  `(gethash ,e ,set))

(defmacro set-add (e set)
  `(setf (gethash ,e ,set) T))

(defmacro set-remove (e set)
  `(remhash ,e ,set))
