;;;; capitalized-export.asd

(asdf:defsystem #:capitalized-export
  :description "Capitalized-export provides a readtable that lets users export accessible symbols by capitalizing them."
  :author "Peter von Etter"
  :license  "LGPL-3.0"
  :version "0.0.1"
  :components ((:file "src/capitalized-export"))
  :depends-on (#:buffering-readtable))
