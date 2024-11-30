(in-package :immutable-hash)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct immutable-hash-map
    (node nil :type optional-node)))

(defmethod print-object ((obj immutable-hash-map) stream)
  (if *print-readably*
      (call-next-method)
      (with-slots (node) obj
        (print-object
         `(hash-map:hash-map ,(and node (node-to-list node)))
         stream))))

(define-constant empty-immutable-hash-map (make-immutable-hash-map))

(defun immutable-hash-map (&rest key-value-pairs)
  (if key-value-pairs
      (make-immutable-hash-map
       :node (loop for key-value-pair in key-value-pairs
                   for node = (destructuring-bind (key . value) key-value-pair
                                (node-add key value (hash key) 0 (make-node)))

                     then (destructuring-bind (key . value) key-value-pair
                            (node-add key value (hash key) 0 node))
                   finally (return node)))
   empty-immutable-hash-map))

(defun immutable-hash-map-empty-p (table)
  (with-slots (node) table
    (null node)))

(defun immutable-hash-map-count (table)
  (with-slots (node) table
    (if (null node) 0
        (node-item-count node))))

(defun immutable-hash-map-has-key (table key)
  (with-slots (node) table
    (and node (node-has-key key (hash key) 0 node))))

(defun alist-to-immutable-hash-map (alist)
  (apply #'immutable-hash-map alist))

(defun immutable-hash-map-ref (table key)
  (with-slots (node) table
    (if node (node-lookup key (hash key) 0 node)
        (lookup-error key))))

(defun immutable-hash-map-add (table key value)
  (if (immutable-hash-map-has-key table key)
      (immutable-hash-map-add (immutable-hash-map-remove table key) key value)
      (with-slots (node) table
        (make-immutable-hash-map
         :node (node-add key value (hash key) 0 (or node (make-node)))))))

(defun immutable-hash-map-remove (table key)
  (if (immutable-hash-map-has-key table key)
      (with-slots (node) table
        (make-immutable-hash-map
         :node (node-remove key (hash key) 0 node)))
      table))

(defun immutable-hash-map-keys (table)
  (with-slots (node) table
    (mapcar #'car (and node (node-to-list node)))))

(defun immutable-hash-map-values (table)
  (with-slots (node) table
    (mapcar #'cdr (and node (node-to-list node)))))

(defun immutable-hash-map-items (table)
  (with-slots (node) table
    (and node (node-to-list node))))

(defun immutable-hash-map-union (table0 table1 &optional (combine #'cons))
  (immutable-hash-map-union2 table0 table1 combine))

(defun immutable-hash-map-union2 (table0 table1 combine)
  (labels ((combine (arg0 arg1)
             (funcall combine arg0 arg1))
           (combine-key (key)
             (combine (immutable-hash-map-ref table0 key)
                      (immutable-hash-map-ref table1 key)))
           (get-value (key)
             (if (immutable-hash-map-has-key table0 key)
                 (combine-key key)
                 (immutable-hash-map-ref table1 key))))

    (loop for key in (immutable-hash-map-keys table1)
          for new-table = (immutable-hash-map-add table0 key (get-value key))
            then (immutable-hash-map-add new-table key (get-value key))
          finally (return new-table))))

(defun immutable-hash-map-intersection (table0 table1 &optional (combine #'cons))
  (flet ((combine-key (key)
           (funcall combine
                    (immutable-hash-map-ref table0 key)
                    (immutable-hash-map-ref table1 key))))
    (loop for key in (immutable-hash-map-keys table1)
          for new-table = (if (immutable-hash-map-has-key table0 key)
                              (immutable-hash-map-add empty-immutable-hash-map key
                                (combine-key key))
                              empty-immutable-hash-map)
            then (if (immutable-hash-map-has-key table0 key)
                     (immutable-hash-map-add new-table key (combine-key key))
                     new-table)
          finally (return new-table))))

(defun immutable-hash-map-difference (table0 table1)
  (loop for key in (immutable-hash-map-keys table1)
        for new-table = (immutable-hash-map-remove table0 key)
          then (immutable-hash-map-remove new-table key)
        finally (return new-table)))

(defun immutable-hash-map-symmetric-difference (table0 table1)
  (immutable-hash-map-difference
   (immutable-hash-map-union table0 table1 #'cons)
   (immutable-hash-map-intersection table0 table1 (lambda (x y) (declare (ignore x y)) nil))))

(defun immutable-hash-map-fmap (fun table)
  (loop for item in (immutable-hash-map-items table)
        for accum = (immutable-hash-map (cons (car item) (funcall fun (cdr item))))
          then (immutable-hash-map-add accum (car item) (funcall fun (cdr item)))
        finally (return accum)))

(defun immutable-hash-map-items-seq (table)
  (lz:lazy
    (with-slots (node) table
      (if node (node-to-seq node)
          lz:empty-seq))))

(defun immutable-hash-map-keys-seq (table)
  (lz:seq-fmap #'car (immutable-hash-map-items-seq table)))

(defun immutable-hash-map-values-seq (table)
  (lz:seq-fmap #'cdr (immutable-hash-map-items-seq table)))
