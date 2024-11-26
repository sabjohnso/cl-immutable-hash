(in-package :immutable-hash)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct immutable-hash-table
    (node nil :type optional-node)))

(defmethod print-object ((obj immutable-hash-table) stream)
  (with-slots (node) obj
      (print-object
       (cons 'immutable-hash-table (and node (node-to-list node)))
       stream)))
(define-constant empty-immutable-hash-table (make-immutable-hash-table))

(defun immutable-hash-table (&rest key-value-pairs)
  (if key-value-pairs
      (make-immutable-hash-table
       :node (loop for key-value-pair in key-value-pairs
                   for node = (destructuring-bind (key . value) key-value-pair
                                (node-add key value (hash key) 0 (make-node)))

                     then (destructuring-bind (key . value) key-value-pair
                            (node-add key value (hash key) 0 node))
                   finally (return node)))
   empty-immutable-hash-table))


;; (declaim (ftype (function (immutable-hash-table) boolean) immutable-hash-table-empty-p))
(defun immutable-hash-table-empty-p (table)
  (with-slots (node) table
    (null node)))


(defun immutable-hash-table-count (table)
  (with-slots (node) table
    (if (null node) 0
        (node-item-count node))))


;; (declaim (ftype (function (immutable-hash-table t) boolean) immutable-hash-table-has-key))
(defun immutable-hash-table-has-key (table key)
  (with-slots (node) table
    (and node (node-has-key key (hash key) 0 node))))

(defun alist-to-immutable-hash-table (alist)
  (apply #'immutable-hash-table alist))

(defun immutable-hash-table-ref (table key)
  (with-slots (node) table
    (if node (node-lookup key (hash key) 0 node)
        (lookup-error key))))

(defun immutable-hash-table-add (table key value)
  (if (immutable-hash-table-has-key table key)
      (immutable-hash-table-add (immutable-hash-table-remove table key) key value)
      (with-slots (node) table
        (make-immutable-hash-table
         :node (node-add key value (hash key) 0 (or node (make-node)))))))

(defun immutable-hash-table-remove (table key)
  (if (immutable-hash-table-has-key table key)
      (with-slots (node) table
        (make-immutable-hash-table
         :node (node-remove key (hash key) 0 node)))
      table))

(defun immutable-hash-table-keys (table)
  (with-slots (node) table
    (mapcar #'car (and node (node-to-list node)))))

(defun immutable-hash-table-values (table)
  (with-slots (node) table
    (mapcar #'cdr (and node (node-to-list node)))))

(defun immutable-hash-table-items (table)
  (with-slots (node) table
    (and node (node-to-list node))))

(defun immutable-hash-table-union (table0 table1 &optional (combine #'cons))
  (immutable-hash-table-union2 table0 table1 combine))

(defun immutable-hash-table-union2 (table0 table1 combine)
  (labels ((combine (arg0 arg1)
             (funcall combine arg0 arg1))
           (combine-key (key)
             (combine (immutable-hash-table-ref table0 key)
                      (immutable-hash-table-ref table1 key)))
           (get-value (key)
             (if (immutable-hash-table-has-key table0 key)
                 (combine-key key)
                 (immutable-hash-table-ref table1 key))))

    (loop for key in (immutable-hash-table-keys table1)
          for new-table = (immutable-hash-table-add table0 key (get-value key))
            then (immutable-hash-table-add new-table key (get-value key))
          finally (return new-table))))


(defun immutable-hash-table-intersection (table0 table1 &optional (combine #'cons))
  (flet ((combine-key (key)
           (funcall combine
                    (immutable-hash-table-ref table0 key)
                    (immutable-hash-table-ref table1 key))))
    (loop for key in (immutable-hash-table-keys table1)
          for new-table = (if (immutable-hash-table-has-key table0 key)
                              (immutable-hash-table-add empty-immutable-hash-table key
                                (combine-key key))
                              empty-immutable-hash-table)
            then (if (immutable-hash-table-has-key table0 key)
                     (immutable-hash-table-add new-table key (combine-key key))
                     new-table)
          finally (return new-table))))

(defun immutable-hash-table-difference (table0 table1)
  (loop for key in (immutable-hash-table-keys table1)
        for new-table = (immutable-hash-table-remove table0 key)
          then (immutable-hash-table-remove new-table key)
        finally (return new-table)))

(defun immutable-hash-table-symmetric-difference (table0 table1)
  (immutable-hash-table-difference
   (immutable-hash-table-union table0 table1 #'cons)
   (immutable-hash-table-intersection table0 table1 (lambda (x y) (declare (ignore x y)) nil))))
