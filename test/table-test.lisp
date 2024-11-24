(in-package :cl-user)

(defpackage :immutable-hash/test.table
  (:use :cl :5am :immutable-hash)
  (:export :run-all-tests!))

(in-package :immutable-hash/test.table)

(defun run-all-tests! ()
  (run! 'table))

(def-suite table)

(in-suite table)

(test placeholder
  (is (= (+ 1 2) 3)))
