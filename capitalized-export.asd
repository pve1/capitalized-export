;;;; capitalized-export.asd

(asdf:defsystem #:capitalized-export
  :description "Describe capitalized-export here"
  :author "Peter von Etter"
  :license  "LLGPL"
  :version "0.0.1"
  :components ((:file "src/capitalized-export"))
  :depends-on (#:buffering-readtable))
