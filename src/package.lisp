(in-package :cl-user)

(defpackage :immutable-hash
  (:nicknames :ih)
  (:use :cl)

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
   #:list-to-immutable-set
   #:hash)

  ;; Hash table exports
  (:export
   #:empty-immutable-hash-table
   #:immutable-hash-table
   #:alist-to-immutable-hash-table
   #:immutable-hash-table-p
   #:immutable-hash-table-empty-p
   #:immutable-hash-table-count
   #:immutable-hash-table-has-key
   #:immutable-hash-table-ref
   #:immutable-hash-table-keys
   #:immutable-hash-table-values
   #:immutable-hash-table-items
   #:immutable-hash-table-add
   #:immutable-hash-table-remove
   #:immutable-hash-table-union
   #:immutable-hash-table-intersection
   #:immutable-hash-table-difference
   #:immutable-hash-table-symmetric-difference))

(defpackage :immutable-hash-terse-set
  (:nicknames :set)
  (:use :cl :immutable-hash)
  (:shadow :set :set-difference)
  (:export
   #:set
   #:set-p
   #:set-empty-p
   #:set-count
   #:set-member
   #:set-add
   #:set-remove
   #:set-complement
   #:set-union
   #:set-intersection
   #:set-difference
   #:set-symmetric-difference
   #:empty-set
   #:set-sub-super
   #:set-equal
   #:set-disjoint
   #:set-distinct
   #:set-to-list
   #:list-to-set))

(defpackage :immutable-hash-map
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
   #:add
   #:remove
   #:union
   #:intersection
   #:difference
   #:symmetric-difference))
