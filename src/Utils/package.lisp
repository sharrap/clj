(defpackage :clj.utils
  (:use :cl)
  (:export #:make-clsset #:get-clsset
           #:curry #:take #:istream #:istream-read #:istream-next
           #:hash-add-list #:split-when-1 #:split-when #:findchr
           #:compose2 #:compose #:copylst #:uniq #:uniq-cls))

(load "src/Utils/Utils.lisp")
(load "src/Utils/Set.lisp")
(load "src/Utils/Sequences.lisp")
