(in-package :cl-user)

(defpackage immutable-hash/test.set
  (:use :cl :5am :immutable-hash)
  (:export :run-all-tests!))

(in-package :immutable-hash/test.set)

(defun run-all-tests! ()
  (run! 'set))

(def-suite table)

(in-suite table)

(defmacro -> (arg &rest forms)
  (if (null forms) arg
      (destructuring-bind ((fun . args) . forms) forms
        `(-> (,fun ,arg ,@args) ,@forms))))

(test immutable-set
  (let ((set (immutable-set)))
    (is (immutable-set-p set))
    (is (immutable-set-empty set))
    (is (zerop (immutable-set-count set)))
    (is-false (immutable-set-member set 'x))
    (is (equal nil (immutable-set-to-list set))))

  (let ((set (immutable-set 'a)))
    (is (immutable-set-p set))
    (is-false (immutable-set-empty set))
    (is (= 1 (immutable-set-count set)))
    (is-true (immutable-set-member set 'a))
    (is (equal '(a) (immutable-set-to-list set))))

  (let* ((members '(a b c d e f g h i))
         (set (apply #'immutable-set members)))
    (is (= (length members) (immutable-set-count set) ))
    (is (immutable-set-member set 'a))
    (is (immutable-set-member set 'b))
    (is (immutable-set-member set 'c))
    (is (immutable-set-member set 'd))
    (is (immutable-set-member set 'e))
    (is (immutable-set-member set 'f))
    (is (immutable-set-member set 'g))
    (is (immutable-set-member set 'h))
    (is (immutable-set-member set 'i)))

  (let* ((n 100)
         (set (apply #'immutable-set (loop for i from 0 to (1- n) collecting i))))
    (is (immutable-set-p set))
    (is-false (immutable-set-empty set))
    (loop for i from 0 to (1- n) do (is (immutable-set-member set i)))))


(test immutable-set-add
  (let ((set (immutable-set-add (immutable-set) 'x)))
    (is (immutable-set-p set))
    (is (= 1 (immutable-set-count set)))
    (is (immutable-set-member set 'x)))

  (let ((set (immutable-set "x")))
    (is (= 1 (immutable-set-count (immutable-set-add set "x"))))))

(test immutable-set-remove
  (let* ((set0 (immutable-set 'a 'b 'c))
         (set1 (immutable-set-remove set0 'a))
         (set2 (immutable-set-remove set1 'x)))
    (is-true (immutable-set-member set0 'a))
    (is-false (immutable-set-member set1 'a))
    (is-false (immutable-set-member set1 'x))
    (is-false (immutable-set-member set2 'x)))

    (let* ((n 64)
           (set (apply #'immutable-set (loop for i from 0 to (- n 1) collecting i)))
           (new-set (loop for i from 0 to (1- n)
                          for new-set = (immutable-set-remove set i)
                            then (immutable-set-remove new-set i)
                          finally (return new-set))))
      (is (= n (immutable-set-count set)))
      (is (= 0 (immutable-set-count new-set)))))


(test immutable-set-union
  (let* ((set0 (immutable-set "a" "b" "c"))
         (set1 (immutable-set "c" "d" "e")))

    ;; union of `SET0' with an empty set
    (let ((new-set (immutable-set-union set0 (immutable-set))))
      (is-true (= (immutable-set-count set0) (immutable-set-count new-set)))
      (loop for member in (immutable-set-to-list set0)
            do (is-true (immutable-set-member new-set member))))

    ;; union of an empty set with `SET0'
    (let ((new-set (immutable-set-union (immutable-set) set0)))
      (is-true (= (immutable-set-count set0) (immutable-set-count new-set)))
      (loop for member in (immutable-set-to-list set0)
            do (is-true (immutable-set-member new-set member))))

    ;; union of `SET0' with itself
    (let ((new-set (immutable-set-union set0 set0)))
      (is-true (= (immutable-set-count set0) (immutable-set-count new-set)))
      (loop for member in (immutable-set-to-list set0)
            do (is-true (immutable-set-member new-set member))))

    (let ((new-set (immutable-set-union set0 set1)))
      ;; All members of `SET0' are in `NEW-SET'
      (loop for member in (immutable-set-to-list set0)
            do (is-true (immutable-set-member new-set member)))

      ;; All members of `SET1' are in `NEW-SET'
      (loop for member in (immutable-set-to-list set1)
            do (is-true (immutable-set-member new-set member)))

      ;; All members of `NEW-SET' are in `SET0' or `SET1'
      (loop for member in (immutable-set-to-list new-set)
            do (is-true (or (immutable-set-member set0 member)
                            (immutable-set-member set1 member)))))))


(test immutable-set-intersection
  (let* ((set0 (immutable-set "a" "b" "c"))
         (set1 (immutable-set "c" "d" "e"))
         (set2 (immutable-set "e" "f" "g")))
    ;; intersection of `SET0' with an empty set
    (is (immutable-set-empty (immutable-set-intersection set0 empty-immutable-set)))

    ;; intersection of an empty set with `SET0'
    (is (immutable-set-empty (immutable-set-intersection empty-immutable-set set0)))

    ;; intersection of `SET0' with itself
    (let ((new-set (immutable-set-intersection set0 set0)))
      (is (immutable-set-equal set0 new-set)))

    ;; intersection of distinct overlapping sets
    (let ((new-set (immutable-set-intersection set0 set1)))
      (is (immutable-set-sub-super new-set set0))
      (is (immutable-set-sub-super new-set set1)))

    ;; intersection of nonempty disjoint sets
    (let ((new-set (immutable-set-intersection set0 set2)))
      (is (immutable-set-empty new-set)))))


(test immutable-set-difference
  (let* ((set0 (immutable-set :a :b :c))
         (set1 (immutable-set :c :d :e))
         (set2 (immutable-set :e :f :g)))
    ;; remove an empty set from `SET0'
    (is (immutable-set-equal set0 (immutable-set-difference set0 empty-immutable-set)))

    ;; remove `SET0' from an empty set
    (is (immutable-set-empty (immutable-set-difference empty-immutable-set set0)))

    ;; remove `SET0' from itself
    (is (immutable-set-empty (immutable-set-difference set0 set0)))

    ;; set difference with a distinct overlapping set
    (let ((new-set (immutable-set-difference set0 set1)))
      (is (immutable-set-sub-super new-set set0))
      (is (immutable-set-disjoint new-set set1)))

    ;; set difference with a disjoint set
    (let ((new-set (immutable-set-difference set0 set2)))
      (is (immutable-set-equal new-set set0))
      (is (immutable-set-disjoint new-set set2)))))

(test immutable-set-symmetric-difference
  (let* ((set0 (immutable-set :a :b :c))
         (set1 (immutable-set :c :d :e))
         (set2 (immutable-set :e :f :g)))
    ;; symmetric-difference with an empty set on the right
    (is (immutable-set-equal set0 (immutable-set-symmetric-difference set0 empty-immutable-set)))

    ;; symmetric-difference with an empty set on the left
    (is (immutable-set-equal set0 (immutable-set-symmetric-difference empty-immutable-set set0)))

    ;; symmetric-difference with itself
    (is (immutable-set-empty (immutable-set-symmetric-difference set0 set0)))

    ;; symmetric-difference with a distinct overlapping set
    (let ((new-set (immutable-set-symmetric-difference set0 set1)))
      (is (immutable-set-sub-super new-set (immutable-set-union set0 set1)))
      (is (immutable-set-disjoint new-set (immutable-set-intersection set0 set1))))

    ;; symmetric-difference with a disjoint overlapping set
    (let ((new-set (immutable-set-symmetric-difference set0 set2)))
      (is (immutable-set-equal (immutable-set-union set0 set2) new-set)))))


(test immutable-set-complement

  ;; `IMMUTABLE-SET-COMPLEMENT' is implemented symbolically. So, to tests it
  ;; without invoking the same symbolic rule, results are projected onto a
  ;; set (poorly) named `GENESIS', and compared to concrete results in
  ;; `GENESIS'.
  (let* ((n 10) ;; number of items in `GENESIS'
         (n0 2) ;; number of items in `SET0'
         (n1 3) ;; number of items in `SET1'
         (overlap 1)) ;; number of common items between `SET0' and `SET1'
    (assert (< (- (+ n0 n1) overlap) n))
    (assert (and (< overlap n0) (< overlap n1)))
    (let ((genesis (list-to-immutable-set
                    (loop for i from 0 to (1- n)
                          collecting i)))
          (set0 (list-to-immutable-set
                 (loop for i from 0 to (1- n0)
                       collecting i)))
          (set1 (list-to-immutable-set
                 (loop for i from (- n0 overlap) to (- (+ n0 n1) overlap)
                       collecting i))))
      (labels ((in-genesis (set) (immutable-set-intersection genesis set))
               (comp (set) (in-genesis (immutable-set-complement set))))

        ;; Test the `COMP' before continuing with other tests
        (is (immutable-set-equal
             (comp set0)
             (list-to-immutable-set
              (loop for i from n0 to (1- n) collecting i))))

        ;;
        ;; ... Complement of empty set
        ;;
        (is (immutable-set-equal
             genesis
             (in-genesis (immutable-set-complement empty-immutable-set))))

        ;;
        ;; ... Complement of genesis
        ;;
        (is (immutable-set-empty
             (in-genesis
              (immutable-set-complement genesis))))

        ;;
        ;; ... Intersection with complements
        ;;
        (is (immutable-set-equal
             (immutable-set-intersection (comp set0) (comp set1))
             (in-genesis
              (immutable-set-intersection
               (immutable-set-complement set0)
               (immutable-set-complement set1)))))

        (is (immutable-set-equal
             (immutable-set-intersection (comp set0) set1)
             (in-genesis (immutable-set-intersection (immutable-set-complement set0) set1))))

        (is (immutable-set-equal
             (immutable-set-intersection set0 (comp set1))
             (in-genesis
              (immutable-set-intersection set0 (immutable-set-complement set1)))))

        ;;
        ;; ... Union with complements
        ;;
        (is (immutable-set-equal
             (immutable-set-union (comp set0) (comp set1))
             (in-genesis
              (immutable-set-union
               (immutable-set-complement set0)
               (immutable-set-complement set1)))))

        (is (immutable-set-equal
             (immutable-set-union (comp set0) set1)
             (in-genesis (immutable-set-union (immutable-set-complement set0) set1))))

        (is (immutable-set-equal
             (immutable-set-union set0 (comp set1))
             (in-genesis
              (immutable-set-union set0 (immutable-set-complement set1)))))

        ;;
        ;; ... Symmetric difference with complements
        ;;
        (is (immutable-set-equal
             (immutable-set-symmetric-difference (comp set0) (comp set1))
             (in-genesis
              (immutable-set-symmetric-difference
               (immutable-set-complement set0)
               (immutable-set-complement set1)))))

        (is (immutable-set-equal
             (immutable-set-symmetric-difference (comp set0) set1)
             (in-genesis (immutable-set-symmetric-difference (immutable-set-complement set0) set1))))

        (is (immutable-set-equal
             (immutable-set-symmetric-difference set0 (comp set1))
             (in-genesis
              (immutable-set-symmetric-difference set0 (immutable-set-complement set1)))))

        ;;
        ;; Difference with complements
        ;;
        (is (immutable-set-equal
             (immutable-set-difference (comp set0) (comp set1))
             (in-genesis (immutable-set-difference
                          (immutable-set-complement set0)
                          (immutable-set-complement set1)))))

        (is (immutable-set-equal
             (immutable-set-difference (comp set0) set1)
             (in-genesis (immutable-set-difference (immutable-set-complement set0) set1))))

        (is (immutable-set-equal
             (immutable-set-difference set0 (comp set1))
             (in-genesis
              (immutable-set-difference set0 (immutable-set-complement set1)))))

        #||#))))
