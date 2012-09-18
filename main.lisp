(in-package #:with-shadowed-bindings)

(defmacro with-shadowed-bindings (bindings &body body)
  (multiple-value-bind (variables normal-functions setf-functions)
      (do ((bindings bindings (cdr bindings))
	   variables
	   normal-functions
	   setf-functions)
	  ((endp bindings)
	   (flet ((frob (list)
		    (nreverse
		     (delete-duplicates list :test #'equal))))
	     (values (frob variables)
		     (frob normal-functions)
		     (frob setf-functions))))
	(let ((binding (first bindings)))
	  (etypecase binding
	    (symbol
	     (push binding variables))
	    ((cons (eql function)
		   (cons symbol null))
	     (push (second binding) normal-functions))
	    ((cons (eql function)
		   (cons (cons (eql setf) (cons symbol null)) null))
	     (push (second binding) setf-functions)))))
    (reduce
     (lambda (bindings-and-function body)
       (destructuring-bind (bindings function) bindings-and-function
	 (if bindings
	     (funcall function bindings body)
	     body)))
     (list
      (list variables
	    (lambda (names body)
	      (let ((macro-names
		     (mapcar (lambda (name)
			       (gensym (symbol-name name)))
			     names)))
		`(macrolet
		     ,(mapcar
		       (lambda (name macro-name)
			 `(,macro-name
			   ()
			   (error "Can't access shadowed variable ~S."
				  ',name)))
		       names
		       macro-names)
		   (symbol-macrolet
		       ,(mapcar
			 (lambda (name macro-name)
			   `(,name (,macro-name)))
			 names
			 macro-names)
		     ,@body)))))
      (list normal-functions
	    (lambda (names body)
	      `(macrolet
		   ,(mapcar
		     (lambda (name)
		       `(,name
			 (&rest rest)
			 (declare (ignore rest))
			 (error "Can't access shadowed ~
                                 function or macro ~S." ',name)))
		     names)
		 ,@body)))
      (list setf-functions
	    (lambda (bindings body)
	      `(flet ,(mapcar
		       (lambda (binding)
			 `(,binding
			   (&rest rest)
			   (declare (ignore rest))
			   (error "Can't access shadowed function ~S."
				  ',binding)))
		       bindings)
		 ,@body))))
     :from-end t
     :initial-value body)))
