(defpackage :clj.utils
  (:use :cl)
  (:export #:make-hash-set #:set-contains #:set-add #:set-remove
           #:curry #:take #:istream #:istream-read #:istream-next
           #:hash-add-list #:split-when-1 #:split-when #:findchr
           #:compose2 #:compose #:copylst))

(load "src/Utils/Utils.lisp")
(load "src/Utils/Set.lisp")
