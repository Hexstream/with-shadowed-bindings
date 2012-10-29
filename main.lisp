(in-package #:with-shadowed-bindings)

(defun %analyze (binding &optional env)
  '(values kind name &rest keys &key &allow-other-keys)
  (flet ((inner (binding)
           ))
    (multiple-value-bind (kind name &rest keys) (inner binding)
      (cond (kind (apply #'values kind name keys))
            ((consp binding)
             ))))
  (etypecase binding
    (symbol (values :variable binding))
    ((cons (eql function)
           (cons symbol null))
     (destructuring-bind (name &rest keys) (rest binding)
       (apply #'values
              (if (macro-function name env)
                  :macro
                  :function)
              name
              keys)))
    ((cons (eql function)
           (cons (cons (eql setf) (cons symbol null))
                 list))
     (destructuring-bind (function-name &rest keys) (rest binding)
       (apply #'values :setf-function (second function-name) keys)))))

(defun %add-shadowing (body kind name)
  (ecase kind
    (:variable
     (let ((macro-name (gensym (symbol-name name))))
       `(macrolet ((,macro-name ()
                     (error "Can't access shadowed variable ~S."
                            ',name)))
          (symbol-macrolet ((,name (,macro-name)))
            (declare (ignorable ,name))
            ,@body))))
    (:macro `(macrolet ((,name (&rest rest)
                          (declare (ignore rest))
                          (error "Can't access shadowed macro ~S."
                                 ',name)))
               (declare (ignorable ,name))
               ,@body))
    ((:function :setf-function)
     (let ((name (ecase kind
                   (:function name)
                   (:setf-function `(setf ,name)))))
       `(flet ((,name (&rest rest)
                 (declare (ignore rest))
                 (error "Can't access shadowed function ~S."
                        ',name)))
          (declare (ignorable #',name))
          ,@body)))))

(defmacro with-shadowed-bindings (bindings &body body &environment env)
  (if bindings
      (map-bind (reduce) (((binding body) bindings)
                          (() :from-end t :initial-value body))
        (multiple-value-call #'%add-shadowing body (%analyze binding env)))
      `(progn ,@body)))
