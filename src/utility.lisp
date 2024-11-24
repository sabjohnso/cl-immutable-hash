(in-package :immutable-hash)

(defmacro define-constant (name value &optional doc)
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc))))

(defmacro swap-when (test (( var0 var1) &rest more-bindings) &body body)
  "When test is `T' swap the variables `VAR0' and `VAR1'"
  (assert (symbolp var0))
  (assert (symbolp var1))
  (let ((test-result (gensym "TEST-RESULT")))
    (if (null more-bindings)
        `(let ((,test-result ,test))
           (let ((,var0 (if ,test-result ,var1 ,var0))
                 (,var1 (if ,test-result ,var0 ,var1)))
             ,@body))
        `(let ((,test-result ,test))
           (swap-when ,test-result
               ((,var0 ,var1))
             (swap-when ,test-result
                 (,@more-bindings)
               ,@body))))))
