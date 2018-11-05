(in-package #:with-shadowed-bindings)

(defun %analyze (binding &optional env)
  '(values kind name)
  (etypecase binding
    (symbol (values :variable binding))
    ((cons (eql function) (cons t null))
     (let ((name (second binding)))
       (etypecase name
         (symbol (values (if (macro-function name env)
                             :macro
                             :function)
                         name))
         ((cons (eql setf) (cons symbol null))
          (values :setf-function (second name))))))))

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
