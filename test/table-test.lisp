(in-package :cl-user)

(defpackage :immutable-hash/test.table
  (:use :cl :5am :immutable-hash)
  (:export :run-all-tests!))

(in-package :immutable-hash/test.table)

(defun run-all-tests! ()
  (run! 'table))

(def-suite table)

(in-suite table)

(defmacro -> (form &rest forms)
  (if (null forms) form
      (destructuring-bind ((fun . args) . more-forms) forms
        `(-> (,fun ,form ,@args) ,@more-forms))))

(test hash-table-construction

  ;; The empty hash table
  (let ((table empty-immutable-hash-table))
    (is-true (immutable-hash-table-p table))
    (is-true (immutable-hash-table-p table))
    (is-false (immutable-hash-table-has-key table 'a)))

  ;; Constructing an empty hash table
  (let ((table (immutable-hash-table)))
    (is-true (immutable-hash-table-p table))
    (is-true (immutable-hash-table-empty-p table))
    (is-false (immutable-hash-table-has-key table 'a)))


  (let ((table (immutable-hash-table '(a . b))))
    (is-true (immutable-hash-table-p table))
    (is-false (immutable-hash-table-empty-p table))
    (is-true (immutable-hash-table-has-key table 'a)))

  (let ((table (immutable-hash-table '(a . b) '(c . d) '(e . f)) ))
    (is-true (immutable-hash-table-p table))
    (is-false (immutable-hash-table-empty-p table))
    (is-true (immutable-hash-table-has-key table 'a))
    (is (eq 'b (immutable-hash-table-ref table 'a)))
    (is-true (immutable-hash-table-has-key table 'c))
    (is (eq 'd (immutable-hash-table-ref table 'c)))
    (is-true (immutable-hash-table-has-key table 'e))
    (is (eq 'f (immutable-hash-table-ref table 'e)))
    (is-false (immutable-hash-table-has-key table 'g)))

  (let ((table (alist-to-immutable-hash-table '((a . b) (c . d) (e . f)))))
    (is-true (immutable-hash-table-p table))
    (is-false (immutable-hash-table-empty-p table))
    (is-true (immutable-hash-table-has-key table 'a))
    (is (eq 'b (immutable-hash-table-ref table 'a)))
    (is-true (immutable-hash-table-has-key table 'c))
    (is (eq 'd (immutable-hash-table-ref table 'c)))
    (is-true (immutable-hash-table-has-key table 'e))
    (is (eq 'f (immutable-hash-table-ref table 'e)))
    (is-false (immutable-hash-table-has-key table 'g))))

(test hash-table-add
  (let ((table (-> empty-immutable-hash-table
                   (immutable-hash-table-add 'a 'b)
                   (immutable-hash-table-add 'c 'd)
                   (immutable-hash-table-add 'e 'f))))
    (is (= 3 (immutable-hash-table-count table)))
    (is-true (immutable-hash-table-p table))
    (is-false (immutable-hash-table-empty-p table))
    (is (eq 'b (immutable-hash-table-ref table 'a)))
    (is (eq 'd (immutable-hash-table-ref table 'c)))
    (is (eq 'f (immutable-hash-table-ref table 'e)))))


(test hash-table-remove
  (let* ((n 100)
         (table (alist-to-immutable-hash-table (loop for i from 0 to (1- n) collecting (cons i (random (expt 2 32))))))
         (new-table (loop for i from 0 to (1- n)
                          for new-table = (immutable-hash-table-remove table i) then (immutable-hash-table-remove new-table i)
                          finally (return new-table))))
    (is (immutable-hash-table-empty-p new-table))))

(test hash-table-union
  (let* ((table0 (alist-to-immutable-hash-table
                '((a . 1)
                  (b . 2)
                  (c . 3))))
         (table1 (alist-to-immutable-hash-table
                '((c . 4)
                  (d . 5)
                  (e . 6))))
         (new-table (immutable-hash-table-union table0 table1 #'cons)))
    (is (immutable-hash-table-p new-table))
    (is (= 5 (immutable-hash-table-count new-table)))
    (is (= 1 (immutable-hash-table-ref new-table 'a)))
    (is (= 2 (immutable-hash-table-ref new-table 'b)))
    (is (equal '(3 . 4) (immutable-hash-table-ref new-table 'c)))
    (is (= 5  (immutable-hash-table-ref new-table 'd)))
    (is (= 6  (immutable-hash-table-ref new-table 'e)))))

(test hash-table-intersection
  (let* ((table0 (alist-to-immutable-hash-table
                '((a . 1)
                  (b . 2)
                  (c . 3))))
         (table1 (alist-to-immutable-hash-table
                '((c . 4)
                  (d . 5)
                  (e . 6))))
         (new-table (immutable-hash-table-intersection table0 table1 #'cons)))
    (is (immutable-hash-table-p new-table))
    (is (= 1 (immutable-hash-table-count new-table)))
    (is (equal '(3 . 4) (immutable-hash-table-ref new-table 'c)))))

(test hash-table-difference
  (let* ((table0 (alist-to-immutable-hash-table
                  '((a . 1)
                    (b . 2)
                    (c . 3))))
         (table1 (alist-to-immutable-hash-table
                  '((c . 4)
                    (d . 5)
                    (e . 6))))
         (new-table (immutable-hash-table-difference table0 table1)))
    (is (immutable-hash-table-p new-table))
    (is (= 2 (immutable-hash-table-count new-table)))
    (is (immutable-hash-table-has-key new-table 'a))
    (is (immutable-hash-table-has-key new-table 'b))))

(test hash-table-symmetric-difference
  (let* ((table0 (alist-to-immutable-hash-table
                  '((a . 1)
                    (b . 2)
                    (c . 3))))
         (table1 (alist-to-immutable-hash-table
                  '((c . 4)
                    (d . 5)
                    (e . 6))))
         (new-table (immutable-hash-table-symmetric-difference table0 table1)))
    (is (immutable-hash-table-p new-table))
    (is (= 4 (immutable-hash-table-count new-table)))
    (is (immutable-hash-table-has-key new-table 'a))
    (is (immutable-hash-table-has-key new-table 'b))
    (is (immutable-hash-table-has-key new-table 'd))
    (is (immutable-hash-table-has-key new-table 'e))))
