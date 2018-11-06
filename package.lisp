(cl:defpackage #:with-shadowed-bindings
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:export #:with-shadowed-bindings ; Import this single symbol for normal usage. Don't (:use)!

           #:invalid-access
           #:kind
           #:name))
