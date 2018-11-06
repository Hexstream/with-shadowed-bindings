(cl:defpackage #:with-shadowed-bindings_tests
  (:use #:cl #:parachute)
  (:import-from #:with-shadowed-bindings
                #:with-shadowed-bindings
                #:invalid-access))

(cl:in-package #:with-shadowed-bindings_tests)

(defmacro %test-macro ()
  't)

(define-test "main"
  :compile-at :execute
  (let ((my-protected-variable :initial-value))
    (flet ((backdoor ()
             my-protected-variable)
           ((setf backdoor) (new-value)
             (setf my-protected-variable new-value)))
      (with-shadowed-bindings (my-protected-variable)
        (fail my-protected-variable 'invalid-access)
        (fail (setf my-protected-variable :illegally-set-value) 'invalid-access)
        (is eq :initial-value (backdoor))
        (setf (backdoor) :second-value)
        (is eq :second-value (backdoor))
        (fail my-protected-variable 'invalid-access)
        (let ((backdoor-alias #'backdoor))
          (with-shadowed-bindings (#'backdoor)
            (fail (backdoor) 'invalid-access)
            (setf (backdoor) :third-value)
            (fail (backdoor) 'invalid-access)
            (eq (funcall backdoor-alias) :third-value)
            (with-shadowed-bindings (#'(setf backdoor))
              (fail (setf (backdoor) :fourth-value) 'invalid-access))))
        (is eq (backdoor) :third-value))
      (is eq my-protected-variable :third-value)))
  (with-shadowed-bindings (%test-macro)
    (is eq (%test-macro) t))
  (with-shadowed-bindings (#'%test-macro)
    (fail (%test-macro) 'invalid-access)))
