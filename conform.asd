;;;; conform.asd

(asdf:defsystem #:conform
  :description "Complex, /interactive/ server-rendered HTML forms."
  :author "Mark Polyakov"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "conform")))
