(in-package :cl-user)

(defpackage :immutable-hash-system
  (:use :cl :asdf :uiop))

(in-package :immutable-hash-system)

(defsystem :immutable-hash
  :description "Immutable hash tables and sets"
  :depends-on (:bordeaux-threads)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "utility")
     (:file "core")
     (:file "table")
     (:file "set"))))
  :in-order-to
  ((test-op
    (load-op :immutable-hash)
    (load-op :immutable-hash/test))))

(defsystem :immutable-hash/test
  :description "Tests for `IMMUTABLE-HASH'"
  :depends-on (:fiveam :immutable-hash)
  :components
  ((:module "test"
    :components
    ((:file "table-test")
     (:file "set-test"))))
  :perform (test-op (op sys)
            (symbol-call :immutable-hash/test.set :run-all-tests!)
            (symbol-call :immutable-hash/test.table :run-all-tests!)))
