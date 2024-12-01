(in-package :cl-user)

(defpackage :immutable-hash
  (:nicknames :ih)
  (:use :cl)

  (:export
   #:hash)

  ;; Set exports
  (:export
   #:immutable-set
   #:immutable-set-p
   #:immutable-set-empty
   #:immutable-set-count
   #:immutable-set-member
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
   #:immutable-set-to-list
   #:immutable-set-to-seq
   #:immutable-set-first
   #:immutable-set-rest
   #:list-to-immutable-set
   #:immutable-set-fmap
   #:immutable-set-pure
   #:immutable-set-product
   #:immutable-set-fapply
   #:immutable-set-flatmap
   #:immutable-set-flatten
   #:make-immutable-set-context)

  ;; Hash table exports
  (:export
   #:empty-immutable-hash-map
   #:immutable-hash-map
   #:alist-to-immutable-hash-map
   #:immutable-hash-map-p
   #:immutable-hash-map-empty-p
   #:immutable-hash-map-count
   #:immutable-hash-map-has-key
   #:immutable-hash-map-ref
   #:immutable-hash-map-keys
   #:immutable-hash-map-values
   #:immutable-hash-map-items
   #:immutable-hash-map-keys-seq
   #:immutable-hash-map-values-seq
   #:immutable-hash-map-items-seq
   #:immutable-hash-map-add
   #:immutable-hash-map-remove
   #:immutable-hash-map-union
   #:immutable-hash-map-intersection
   #:immutable-hash-map-difference
   #:immutable-hash-map-symmetric-difference))

(defpackage :set
  (:nicknames :set)
  (:use :cl :immutable-hash)
  (:shadow :set :set-difference :complement :count :member :remove :union :intersection :first :rest)
  (:export
   #:set
   #:set-p
   #:empty-p
   #:count
   #:member
   #:add
   #:remove
   #:complement
   #:union
   #:intersection
   #:difference
   #:symmetric-difference
   #:empty-set
   #:sub-super
   #:set-equal
   #:disjoint
   #:distinct
   #:set-to-list
   #:in-set
   #:first
   #:rest
   #:set-fmap
   #:set-pure
   #:set-product
   #:set-fapply
   #:set-flatmap
   #:set-flatten
   #:make-immutable-set-context
   #:list-to-set))

(defpackage :hash-map
  (:nicknames :hash-map)
  (:use :cl :immutable-hash)
  (:shadow :count :remove :union :intersection)
  (:export
   #:empty-hash-map
   #:hash-map
   #:alist-to-hash-map
   #:hash-map-p
   #:empty-p
   #:count
   #:has-key
   #:ref
   #:keys
   #:values
   #:items
   #:in-keys
   #:in-values
   #:in-items
   #:add
   #:remove
   #:union
   #:intersection
   #:difference
   #:symmetric-difference))
