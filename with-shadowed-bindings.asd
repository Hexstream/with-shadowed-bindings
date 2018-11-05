(asdf:defsystem #:with-shadowed-bindings

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Establishes a new lexical context within which specified bindings are explicitly shadowed, making it clear that they are not referenced within, thereby reducing cognitive load."

  :depends-on ("map-bind")

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
