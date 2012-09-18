(asdf:defsystem #:hexstream-project-template

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "To be described."

  :depends-on (#:map-bind)

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
