(cl:defpackage #:with-shadowed-bindings
  (:use #:cl)
  (:export #:with-shadowed-bindings ; Import this single symbol for normal usage. Don't (:use)!

           #:invalid-access
           #:kind
           #:name))
