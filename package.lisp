(cl:defpackage #:with-shadowed-bindings
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:shadowing-import-from #:enhanced-multiple-value-bind #:multiple-value-bind)
  (:export #:with-shadowed-bindings))
