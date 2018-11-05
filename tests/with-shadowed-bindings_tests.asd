(asdf:defsystem #:with-shadowed-bindings_tests

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "with-shadowed-bindings unit tests."

  :depends-on ("with-shadowed-bindings"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:with-shadowed-bindings_tests)))
