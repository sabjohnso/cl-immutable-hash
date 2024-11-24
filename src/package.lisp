(in-package :cl-user)

(defpackage :immutable-hash
  (:nicknames :hash)
  (:use :cl)
  (:export
   #:immutable-set
   #:immutable-set-p
   #:immutable-set-empty
   #:immutable-set-count
   #:immutable-set-member
   #:immutable-set-to-list
   #:immutable-set-add
   #:immutable-set-remove
   #:immutable-set-complement
   #:immutable-set-union
   #:immutable-set-intersection
   #:immutable-set-difference
   #:immutable-set-symmetric-difference
   #:empty-immutable-set
   #:immutable-set-sub-super
   #:immutable-set-equal
   #:immutable-set-disjoint
   #:immutable-set-distinct
   #:list-to-immutable-set))
