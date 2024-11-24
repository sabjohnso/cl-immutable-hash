(in-package :immutable-hash)

(deftype optional-node ()
  `(or null node))

(defstruct immutable-set
  (node nil :type optional-node)
  (comp nil :type boolean))

(defun immutable-set-complement (set)
  (with-slots (node comp) set
    (make-immutable-set :node node :comp (not comp))))

(defun immutable-set-complement-p (set)
  (immutable-set-comp set))

(defmethod print-object ((obj immutable-set) stream)
  (with-slots (node comp) obj
    (if comp
        (print-object
         `(immutable-set-complement
           (immutable-set ,@(immutable-set-to-list (immutable-set-complement obj))))
         stream)
        (print-object `(immutable-set ,@(immutable-set-to-list obj)) stream))))

(define-symbol-macro empty-immutable-set (immutable-set))

(defun immutable-set-count (set)
  (with-slots (node) set
    (if (null node) 0
        (node-item-count node))))

(defun immutable-set-empty (set)
  (and (zerop (immutable-set-count set))
       (not (immutable-set-complement-p set))))

(defun immutable-set-member (set member)
  (with-slots (node comp) set
    (if comp
        (not (and node (node-has-key member (sxhash member) 0 node)))
        (and node (node-has-key member (sxhash member) 0 node)))))

(defun immutable-set-union (&rest sets)
  (cond ((= 2 (length sets)) (immutable-set-union2 (car sets) (cadr sets)))
        ((null sets) (immutable-set))
        ((= 1 (length sets)) (car sets))
        (t (apply #'immutable-set-union (immutable-set-union2 (car sets) (cadr sets)) (cddr sets)))))

(defun immutable-set-union2 (set0 set1)
  (cond ((immutable-set-empty set0) set1)

        ((immutable-set-empty set1) set0)

        ((and (immutable-set-complement-p set0) (immutable-set-complement-p set1))
         (immutable-set-complement
          (immutable-set-intersection
           (immutable-set-complement set0)
           (immutable-set-complement set1))))

        ((immutable-set-complement-p set0)
         (immutable-set-complement
          (immutable-set-difference (immutable-set-complement set0) set1)))

        ((immutable-set-complement-p set1)
         (immutable-set-union set1 set0))

        (t (swap-when (> (immutable-set-count set0) (immutable-set-count set1))
               ((set0 set1))
             (let ((set0-members (immutable-set-to-list set0)))
               (loop for member in set0-members
                     for new-set = (immutable-set-add set1 member)
                       then (immutable-set-add new-set member)
                     finally (return new-set)))))))

(defun immutable-set-intersection (&rest sets)
  (let ((n (length sets)))
    (cond ((= n 2) (immutable-set-intersection2 (car sets) (cadr sets)))
          ((> n 2) (apply #'immutable-set-intersection
                          (immutable-set-intersection2 (car sets) (cadr sets))
                          (cddr sets)))
          ((= n 1) (car sets))
          ((zerop n) (immutable-set)))))

(defun immutable-set-intersection2 (set0 set1)
  (cond ((and (immutable-set-complement-p set0)
              (immutable-set-complement-p set1))
         (immutable-set-complement
          (immutable-set-union
           (immutable-set-complement set0)
           (immutable-set-complement set1))))

        ((immutable-set-complement-p set0)
         (immutable-set-difference set1 (immutable-set-complement set0)))

        ((immutable-set-complement-p set1)
         (immutable-set-difference set0 (immutable-set-complement set1)))

        (t (swap-when (> (immutable-set-count set0)
                         (immutable-set-count set1))
               ((set0 set1))
             (let ((new-set empty-immutable-set))
               (loop for member in (immutable-set-to-list set0)
                     when (immutable-set-member set1 member)
                       do (setf new-set (immutable-set-add new-set member))
                     finally (return new-set)))))))

(defun immutable-set-difference (&rest sets)
  (cond ((= 2 (length sets)) (immutable-set-difference2 (car sets) (cadr sets)))
        ((< 2 (length sets))
         (apply #'immutable-set-difference
                (immutable-set-difference2 (car sets) (cadr sets)) (cddr sets)))
        ((= 1 (length sets)))
        (t (immutable-set))))

(defun immutable-set-difference2 (set0 set1)
  (cond ((and (immutable-set-complement-p set0)
              (immutable-set-complement-p set1))
         (immutable-set-difference
          (immutable-set-complement set1)
          (immutable-set-complement set0)))

        ((immutable-set-complement-p set0)
         (immutable-set-complement
          (immutable-set-union
           (immutable-set-complement set0)
           set1)))

        ((immutable-set-complement-p set1)
         (immutable-set-intersection set0 (immutable-set-complement set1)))

        (t (let ((new-set set0))
             (loop for member in (immutable-set-to-list set1)
                   do (setf new-set (immutable-set-remove new-set member))
                   finally (return new-set))))))

(defun immutable-set-symmetric-difference (&rest sets)
  (let ((n (length sets)))
    (cond ((= n 2)
           (let ((set0 (car sets))
                 (set1 (cadr sets)))
             (immutable-set-difference
              (immutable-set-union set0 set1)
              (immutable-set-intersection set0 set1))))
          ((> n 2) (error "Not implemented"))
          ((= n 1) (car sets))
          ((zerop n) (immutable-set)))))

(defun immutable-set (&rest members)
  (if members
      (loop for member in members
            for set = (immutable-set-add (immutable-set) member) then (immutable-set-add set member)
            finally (return set))
      (make-immutable-set :node nil)))

(defun immutable-set-add (set new-member)
  (declare (type immutable-set set))
  (if (immutable-set-member set new-member) set
      (with-slots (node) set
        (make-immutable-set
         :node (node-add new-member t (sxhash new-member) 0 (or node (make-node)))))))

(defun immutable-set-remove (set member)
  (declare (type immutable-set set))
  (with-slots (node) set
    (if (and node (immutable-set-member set member))
        (make-immutable-set
         :node (node-remove member (sxhash member) 0 node))
        set)))

(defun immutable-set-to-list (set)
  (with-slots (node comp) set
    (if comp (error "Cannot enumerate members of an infinite set -( 0 )-")
        (mapcar #'car (and node (node-to-list node))))))

(defun immutable-set-sub-super (subset superset)
  (cond ((and (immutable-set-complement-p subset) (immutable-set-complement-p superset))
         (error "not implemented -( 00 )-"))
        ((immutable-set-complement-p subset) (error "not implemented -( 1 )-"))
        ((immutable-set-complement-p superset) (error "not implemented -( 2 )-"))
        (t (loop for member in (immutable-set-to-list subset)
                 when (not (immutable-set-member superset member))
                   do (return nil)
                 finally (return t)))))

(defun immutable-set-equal (set0 set1)
  (and (immutable-set-sub-super set0 set1)
       (immutable-set-sub-super set1 set0)))

(defun immutable-set-distinct (set0 set1)
  (not (immutable-set-equal set0 set1)))

(defun immutable-set-disjoint (set0 set1)
  (immutable-set-empty (immutable-set-intersection set0 set1)))

(defun list-to-immutable-set (inputs)
  (apply #'immutable-set inputs))