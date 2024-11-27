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
     (:file "set")
     (:file "terse-set")
     (:file "hash-map")
     (:file "terse-hash-map"))))
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
    ((:file "hash-map-test")
     (:file "set-test"))))
  :perform (test-op (op sys)
            (symbol-call :immutable-hash/test.set :run-all-tests!)
            (symbol-call :immutable-hash/test.hash-map :run-all-tests!)))
