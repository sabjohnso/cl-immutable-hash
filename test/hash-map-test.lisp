(in-package :cl-user)

(defpackage :immutable-hash/test.hash-map
  (:use :cl :5am :immutable-hash)
  (:export :run-all-tests!))

(in-package :immutable-hash/test.hash-map)

(defun run-all-tests! ()
  (run! 'table))

(def-suite table)

(in-suite table)

(defmacro -> (form &rest forms)
  (if (null forms) form
      (destructuring-bind ((fun . args) . more-forms) forms
        `(-> (,fun ,form ,@args) ,@more-forms))))

(test hash-map-construction

  ;; The empty hash table
  (let ((table empty-immutable-hash-map))
    (is-true (immutable-hash-map-p table))
    (is-true (immutable-hash-map-p table))
    (is-false (immutable-hash-map-has-key table 'a)))

  ;; Constructing an empty hash table
  (let ((table (immutable-hash-map)))
    (is-true (immutable-hash-map-p table))
    (is-true (immutable-hash-map-empty-p table))
    (is-false (immutable-hash-map-has-key table 'a)))


  (let ((table (immutable-hash-map '(a . b))))
    (is-true (immutable-hash-map-p table))
    (is-false (immutable-hash-map-empty-p table))
    (is-true (immutable-hash-map-has-key table 'a)))

  (let ((table (immutable-hash-map '(a . b) '(c . d) '(e . f)) ))
    (is-true (immutable-hash-map-p table))
    (is-false (immutable-hash-map-empty-p table))
    (is-true (immutable-hash-map-has-key table 'a))
    (is (eq 'b (immutable-hash-map-ref table 'a)))
    (is-true (immutable-hash-map-has-key table 'c))
    (is (eq 'd (immutable-hash-map-ref table 'c)))
    (is-true (immutable-hash-map-has-key table 'e))
    (is (eq 'f (immutable-hash-map-ref table 'e)))
    (is-false (immutable-hash-map-has-key table 'g)))

  (let ((table (alist-to-immutable-hash-map '((a . b) (c . d) (e . f)))))
    (is-true (immutable-hash-map-p table))
    (is-false (immutable-hash-map-empty-p table))
    (is-true (immutable-hash-map-has-key table 'a))
    (is (eq 'b (immutable-hash-map-ref table 'a)))
    (is-true (immutable-hash-map-has-key table 'c))
    (is (eq 'd (immutable-hash-map-ref table 'c)))
    (is-true (immutable-hash-map-has-key table 'e))
    (is (eq 'f (immutable-hash-map-ref table 'e)))
    (is-false (immutable-hash-map-has-key table 'g))))

(test hash-map-add
  (let ((table (-> empty-immutable-hash-map
                   (immutable-hash-map-add 'a 'b)
                   (immutable-hash-map-add 'c 'd)
                   (immutable-hash-map-add 'e 'f))))
    (is (= 3 (immutable-hash-map-count table)))
    (is-true (immutable-hash-map-p table))
    (is-false (immutable-hash-map-empty-p table))
    (is (eq 'b (immutable-hash-map-ref table 'a)))
    (is (eq 'd (immutable-hash-map-ref table 'c)))
    (is (eq 'f (immutable-hash-map-ref table 'e)))))


(test hash-map-remove
  (let* ((n 100)
         (table (alist-to-immutable-hash-map (loop for i from 0 to (1- n) collecting (cons i (random (expt 2 32))))))
         (new-table (loop for i from 0 to (1- n)
                          for new-table = (immutable-hash-map-remove table i) then (immutable-hash-map-remove new-table i)
                          finally (return new-table))))
    (is (immutable-hash-map-empty-p new-table))))

(test hash-map-union
  (let* ((table0 (alist-to-immutable-hash-map
                '((a . 1)
                  (b . 2)
                  (c . 3))))
         (table1 (alist-to-immutable-hash-map
                '((c . 4)
                  (d . 5)
                  (e . 6))))
         (new-table (immutable-hash-map-union table0 table1 #'cons)))
    (is (immutable-hash-map-p new-table))
    (is (= 5 (immutable-hash-map-count new-table)))
    (is (= 1 (immutable-hash-map-ref new-table 'a)))
    (is (= 2 (immutable-hash-map-ref new-table 'b)))
    (is (equal '(3 . 4) (immutable-hash-map-ref new-table 'c)))
    (is (= 5  (immutable-hash-map-ref new-table 'd)))
    (is (= 6  (immutable-hash-map-ref new-table 'e)))))

(test hash-map-intersection
  (let* ((table0 (alist-to-immutable-hash-map
                '((a . 1)
                  (b . 2)
                  (c . 3))))
         (table1 (alist-to-immutable-hash-map
                '((c . 4)
                  (d . 5)
                  (e . 6))))
         (new-table (immutable-hash-map-intersection table0 table1 #'cons)))
    (is (immutable-hash-map-p new-table))
    (is (= 1 (immutable-hash-map-count new-table)))
    (is (equal '(3 . 4) (immutable-hash-map-ref new-table 'c)))))

(test hash-map-difference
  (let* ((table0 (alist-to-immutable-hash-map
                  '((a . 1)
                    (b . 2)
                    (c . 3))))
         (table1 (alist-to-immutable-hash-map
                  '((c . 4)
                    (d . 5)
                    (e . 6))))
         (new-table (immutable-hash-map-difference table0 table1)))
    (is (immutable-hash-map-p new-table))
    (is (= 2 (immutable-hash-map-count new-table)))
    (is (immutable-hash-map-has-key new-table 'a))
    (is (immutable-hash-map-has-key new-table 'b))))

(test hash-map-symmetric-difference
  (let* ((table0 (alist-to-immutable-hash-map
                  '((a . 1)
                    (b . 2)
                    (c . 3))))
         (table1 (alist-to-immutable-hash-map
                  '((c . 4)
                    (d . 5)
                    (e . 6))))
         (new-table (immutable-hash-map-symmetric-difference table0 table1)))
    (is (immutable-hash-map-p new-table))
    (is (= 4 (immutable-hash-map-count new-table)))
    (is (immutable-hash-map-has-key new-table 'a))
    (is (immutable-hash-map-has-key new-table 'b))
    (is (immutable-hash-map-has-key new-table 'd))
    (is (immutable-hash-map-has-key new-table 'e))))
