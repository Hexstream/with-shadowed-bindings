(in-package #:with-shadowed-bindings)

(defun %analyze (binding &optional env)
  '(values kind name)
  (etypecase binding
    (symbol (values :variable binding))
    ((cons (eql function) (cons t null))
     (let ((name (second binding)))
       (values (etypecase name
                 (symbol (if (macro-function name env)
                             :macro
                             :function))
                 ((cons (eql setf) (cons symbol null))
                  :function))
               name)))))

(define-condition with-shadowed-bindings:invalid-access (error)
  ((%kind :initarg :kind
          :reader with-shadowed-bindings:kind)
   (%name :initarg :name
          :reader with-shadowed-bindings:name))
  (:report (lambda (condition stream)
             (format stream "Can't access shadowed ~A ~S."
                     (ecase (with-shadowed-bindings:kind condition)
                       (:variable "variable")
                       (:macro "macro")
                       (:function "function"))
                     (with-shadowed-bindings:name condition)))))

(defun with-shadowed-bindings:invalid-access (kind name)
  (error 'invalid-access :kind kind :name name))

(defun (setf with-shadowed-bindings:invalid-access) (new kind name)
  (declare (ignore new))
  (with-shadowed-bindings:invalid-access kind name))

(defun %add-shadowing (body kind name)
  (let ((invalid-access `(with-shadowed-bindings:invalid-access ',kind ',name)))
    (ecase kind
      (:variable
       `(symbol-macrolet ((,name ,invalid-access))
          (declare (ignorable ,name))
          ,@body))
      ((:function :macro)
       `(flet ((,name (&rest rest)
                 (declare (ignore rest))
                 ,invalid-access))
          (declare (ignorable #',name))
          ,@body)))))

(defmacro with-shadowed-bindings (bindings &body body &environment env)
  (if bindings
      (reduce (lambda (binding body)
                (multiple-value-call #'%add-shadowing body (%analyze binding env)))
              bindings :from-end t :initial-value body)
      `(progn ,@body)))
