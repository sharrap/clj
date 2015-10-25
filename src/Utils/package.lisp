(defpackage :clj.utils
  (:use :cl)
  (:export #:make-clsset #:get-clsset
           #:curry #:take #:istream #:istream-read #:istream-next
           #:hash-add-list #:clshash-add-list #:hash-from-list
           #:split-when-1 #:split-when #:findchr
           #:compose2 #:compose #:copylst #:uniq #:uniq-cls
           #:make-clshash #:get-clshash
           #:list-eqf #:list-hashf))

(load "src/Utils/Function.lisp")
(load "src/Utils/Set.lisp")
(load "src/Utils/HashTable.lisp")
(load "src/Utils/Sequences.lisp")
(load "src/Utils/Stream.lisp")
(load "src/Utils/List.lisp")
