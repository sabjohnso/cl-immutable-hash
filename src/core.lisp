;; This file is part of the immutable-hash library
(in-package :immutable-hash)

(eval-when (:load-toplevel :compile-toplevel :execute)

  (define-constant log2-chunk-size 5
    "Log2 of the number slots in a block")

  (define-constant chunk-size (expt 2 log2-chunk-size)
    "The number of slots in a block")

  (define-constant hash-fragment-mask (1- (expt 2 log2-chunk-size))
    "A mask used to select a subhash for banch indexing"))

(defgeneric hash (obj))
(defmethod hash (obj)
  (sxhash obj))

(defmethod hash ((obj function))
  (sxhash (sb-kernel:get-lisp-obj-address obj)))

(deftype hash ()
  (type-of (sxhash nil)))

(deftype flags-data ()
  `(unsigned-byte ,chunk-size))

(defstruct flags (data 0 :type flags-data))

(defun flags ()
  (make-flags :data 0))

(defun flags-empty-p (flags)
  (with-slots (data) flags
    (zerop data)))

(defun flags-count (flags)
  (assert (= chunk-size 32) () "This function is only valid if the chunk size is 32")
  (with-slots (data) flags
    (let ((count (logand #xffffffff)))
      (setf count (- count (logand (ash count -1) #x55555555)))
      (setf count (+ (logand count #x33333333) (logand (ash count -2) #x33333333)))
      (setf count (logand (+ count (ash count -4)) #x0f0f0f0f))
      (setf count (+ count (ash count -8)))
      (setf count (+ count (ash count -16)))
      (logand count #x3f))))

(defun flags-intersection (flags0 flags1)
  (make-flags :data (logand (flags-data flags0) (flags-data flags1))))

(defun flags-union (flags0 flags1)
  (make-flags :data (logior (flags-data flags0) (flags-data flags1))))

(defun flags-difference (flags0 flags1)
  (make-flags :data (logandc2 flags0 flags1)))

(defun flags-symmetric-difference (flags0 flags1)
  (make-flags :data (logxor (flags-data flags0) (flags-data flags1))))

(defmethod print-object ((obj flags) stream)
  (with-slots (data) obj
    (format stream "#b~32,'0b" data)))

(defun flags-ref (flags index)
  (with-slots (data) flags
    (not (zerop (ldb (byte 1 index) data)))))

(defun flags-set (flags index)
  (with-slots (data) flags
    (make-flags :data (logior data (ash 1 index)))))

(defun flags-unset (flags index)
  (with-slots (data) flags
    (make-flags :data (logxor data (ash 1 index)))))

(defun flags-set! (flags index)
  (with-slots (data) flags
    (setf (ldb (byte 1 index) data) 1)))

(defun flags-unset! (flags index)
  (with-slots (data) flags
    (setf (ldb (byte 1 index) data) 0)))

;; (declaim (ftype (function (hash unsigned-byte) hash) hash-fragment)
;;          (ftype (function () flags) make-flags)
;;          (ftype (function (flags unsigned-byte) boolean) flags-ref)
;;          (inline hash-fragment))

(defun hash-fragment (hash depth)
  "Return the hash fragment for the input hash at the input depth"
  (logand hash-fragment-mask
          (ash hash (- (* depth log2-chunk-size)))))

(defstruct leaf
  "A type  describign a `LEAF' with a hash value and key"
  (hash 0  :type (unsigned-byte 62))
  (key nil :type t)
  (value nil :type t))

(deftype optional-leaf-or-node ()
  '(or node leaf null))

(deftype storage ()
  `(simple-array optional-leaf-or-node (,chunk-size)))

(defun make-empty-storage ()
  "Return an empty storage array"
  (make-array (list chunk-size)
    :element-type 'optional-leaf-or-node
    :initial-element nil))

(defstruct chunk
  "A type describing storage that may be shared by multiple nodes"
  (data (make-empty-storage) :type storage)
  (used (make-flags) :type flags)
  (leaf (make-flags) :type flags)
  (lock (bt:make-lock):type bt:lock))


(defstruct node
  "A type describing a branch node."
  (chunk (make-chunk) :type chunk)
  (used (make-flags) :type flags))

(defmethod print-object ((obj node) stream)
  (if *print-readably*
      (let ((*print-readably* nil))
        (call-next-method))
      (call-next-method)))

(defun node-using-slot (node hash-fragment)
  (with-slots (used) node
    (flags-ref used hash-fragment)))

(defun chunk-using-slot (chunk hash-fragment)
  (flags-ref (chunk-used chunk) hash-fragment))

(defun chunk-slot-is-leaf (chunk hash-fragment)
  (flags-ref (chunk-leaf chunk) hash-fragment))


(defun node-has-key (key hash depth node)
  (declare (type node node))
  (let ((hash-fragment (hash-fragment hash depth)))
    (with-slots (chunk used) node
      (and (node-using-slot node hash-fragment)
           (progn
             (assert (chunk-using-slot chunk hash-fragment)
                     (node chunk hash-fragment))
             (chunk-has-key key hash depth hash-fragment chunk))))))

(defun node-add (key value hash depth node)
  (declare (type node node))
  (let ((hash-fragment (hash-fragment hash depth)))
    (with-slots (chunk used) node
      (let ((new-chunk (chunk-add key value hash depth hash-fragment node chunk))
            (new-flags (flags-set (copy-flags used) hash-fragment)))
        (assert (chunk-using-slot new-chunk hash-fragment))
        (assert (flags-ref new-flags hash-fragment))
        (make-node :chunk new-chunk :used new-flags)))))

(defun chunk-add (key value hash depth hash-fragment node chunk)
  (cond ((not (chunk-using-slot chunk hash-fragment))
         (chunk-add/unused-slot key value hash depth hash-fragment node chunk))

        ((and (chunk-using-slot chunk hash-fragment)
              (chunk-slot-is-leaf chunk hash-fragment)
              (node-using-slot node hash-fragment))
         (chunk-add/collision key value hash depth hash-fragment node chunk))

        ((and (chunk-using-slot chunk hash-fragment)
              (node-using-slot node hash-fragment))
         (chunk-add/recur key value hash depth hash-fragment node chunk))

        ((and (chunk-using-slot chunk hash-fragment))
         (chunk-add/only-chunk-uses-slot key value hash depth hash-fragment node chunk))

        (t (error "condition is not implemented for chunk-add"))))

(defun chunk-add/unused-slot (key value hash depth hash-fragment node chunk)
  (assert (node-p node))
  (assert (chunk-p chunk))
  (if (chunk-try-to-acquire-slot chunk hash-fragment)
      (with-slots (data leaf) chunk
        (setf (aref data hash-fragment) (make-leaf :hash hash :key key :value value))
        (flags-set! leaf hash-fragment)
        chunk)
      (chunk-add key value hash depth hash-fragment node chunk)))

(defun chunk-add/only-chunk-uses-slot (key value hash depth hash-fragment node chunk)
  (let ((new-chunk (copy-chunk-for-node node chunk)))
    (chunk-add key value hash depth hash-fragment node new-chunk)))

(defun chunk-try-to-acquire-slot (chunk hash-fragment)
  (assert (chunk-p chunk))
  (with-slots (used lock) chunk
    (bt:with-lock-held (lock)
      (if (chunk-using-slot chunk hash-fragment) nil
          (progn (flags-set! used hash-fragment) t)))))

(defun copy-chunk-for-node (node chunk)
  (assert (node-p node))
  (assert (chunk-p chunk))
  (let ((new-chunk (make-chunk)))
    (with-slots (data used leaf) new-chunk
      (loop for i from 0 to (1- chunk-size)
            when (node-using-slot node i)
              do (progn
                   (setf (aref data i) (chunk-ref chunk i))
                   (flags-set! used i)
                   (when (chunk-slot-is-leaf chunk i)
                     (flags-set! leaf i)))))
    new-chunk))

(defun chunk-add/collision (key value hash depth hash-fragment node chunk)
  (let* ((new-chunk (copy-chunk-for-node node chunk))
         (leaf (chunk-ref chunk hash-fragment))
         (new-node (node-add key value hash (1+ depth) (make-node))))
    (assert (leaf-p leaf))
    (with-slots (hash key value) leaf
      (let ((new-node (node-add key value hash (1+ depth) new-node)))
        (with-slots (data used leaf) new-chunk
          (setf (aref data hash-fragment) new-node)
          (flags-set! used hash-fragment)
          (flags-unset! leaf hash-fragment)
          new-chunk)))))

(defun chunk-add/recur (key value hash depth hash-fragment node chunk)
  (let ((new-chunk (copy-chunk-for-node node chunk))
        (new-node (node-add key value hash (1+ depth) (chunk-ref chunk hash-fragment))))
    (with-slots (data) new-chunk
      (setf (aref data hash-fragment) new-node)
      new-chunk)))

(defun chunk-ref (chunk hash-fragment)
  (aref (chunk-data chunk) hash-fragment))

(defun node-ref (node hash-fragment)
  (with-slots (chunk) node
    (chunk-ref chunk hash-fragment)))

(defun node-item-count (node)
  (the integer
    (with-slots (chunk used) node
      (loop for i from 0 to (1- chunk-size)
            summing (the integer
                      (if (node-using-slot node i)
                          (if (chunk-slot-is-leaf chunk i) 1
                              (node-item-count (the node (chunk-ref chunk i))))
                          0))))))

(defun chunk-has-key (key hash depth hash-fragment chunk)
  (assert (chunk-p chunk))
  (assert (chunk-using-slot chunk hash-fragment) (chunk hash-fragment)
          "`CHUNK-HAS-KEY' is called with a hash-fragment (~s) that is not used.~%~s" hash-fragment chunk)
  (if (chunk-slot-is-leaf chunk hash-fragment) (= hash (leaf-hash (chunk-ref chunk hash-fragment)))
      (node-has-key key hash (1+ depth) (chunk-ref chunk hash-fragment))))

(defun node-to-list (node)
  (with-slots (chunk) node
    (loop for i from 0 to (1- chunk-size)
          appending (if (node-using-slot node i)
                        (if (chunk-slot-is-leaf chunk i)
                            (let ((leaf (chunk-ref chunk i)))
                              (with-slots (key value) leaf
                                (list (cons key value))))
                            (node-to-list (chunk-ref chunk i)))
                        nil))))

(defun node-to-seq (node)
  (with-slots (chunk) node
    (labels ((recur (i)
               (lz:lazy (if (= i chunk-size) lz:empty-seq
                            (if (node-using-slot node i)
                                (if (chunk-slot-is-leaf chunk i)
                                    (let ((leaf (chunk-ref chunk i)))
                                      (with-slots (key value) leaf
                                        (lz:seq-cons (cons key value)
                                                     (recur (1+ i)))))
                                    (let ((node (chunk-ref chunk i)))
                                      (lz:seq-append (node-to-seq node)
                                                     (recur (1+ i)))))
                                (recur (1+ i)))))))
      (recur 0))))

(defun node-remove (key hash depth node)
  (let ((hash-fragment (hash-fragment hash depth)))
    (assert (node-using-slot node hash-fragment))
    (with-slots (chunk used) node
      (if (chunk-slot-is-leaf chunk hash-fragment)

          ;; The slot in the chunk is a leaf. So, the leaf can be removed simply
          ;; by clearing the `USED' flag for the slot.  If it is the only slot being
          ;; used by the node, we can return  `NIL'.
          (let ((new-flags (flags-unset used hash-fragment)))
            (if (flags-empty-p new-flags) nil
                (make-node :chunk chunk :used (flags-unset (node-used node) hash-fragment))))


          ;; The slot in the chunk is a node. So, we recursively remove the item from
          ;; the node.
          (let ((new-node (node-remove key hash (1+ depth) (chunk-ref chunk hash-fragment))))
            (if (null new-node)

                ;; Removing the item from the child node returned `NIL'. If that child
                ;; node was the only slot being used by the current node, we return `NIL'
                ;; again. Otherwise, well just unset the `USED' flag for that slot.
                (let ((new-flags (flags-unset used hash-fragment)))
                  (if (flags-empty-p new-flags) nil
                      (let ((new-chunk (copy-chunk-for-node node chunk)))
                        (assert (chunk-p new-chunk))
                        (with-slots (data used leaf) new-chunk
                          (setf (aref data hash-fragment) nil)
                          (flags-unset! used hash-fragment)
                          (make-node :chunk new-chunk :used new-flags)))))

                ;; The node returned by the recursive call the `NODE-REMOVE' returned a
                ;; nonempty node. We create a copy of the chunk and write the new node
                ;; into it.
                (let ((new-chunk (copy-chunk-for-node node chunk)))
                  (with-slots (data) new-chunk
                    (setf (aref data hash-fragment) new-node)
                    (make-node :chunk new-chunk :used used)))))))))

(defun node-lookup (key hash depth node)
  (let* ((hash-fragment (hash-fragment hash depth))
         (item (node-ref node hash-fragment)))
    (cond ((node-p item) (node-lookup key hash (1+ depth) item))
          ((leaf-p item)
           (with-slots ((new-hash hash) value) item
             (if (= hash new-hash) value
                 (lookup-error key))))
          ((null item) (lookup-error key)))))

(defun lookup-error (key)
  (error "Could not lookup key ~s" key))
