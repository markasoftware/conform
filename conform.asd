;;;; Copyright 2020 Mark Polyakov
;;;; Released under the MIT license

(asdf:defsystem #:conform
  :description "Interactive, composable, server-rendered HTML forms."
  :author "Mark Polyakov"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "conform")))

(asdf:defsystem #:conform/examples
  :description "Examples for an interactive, composable, server-rendered HTML form framework."
  :author "Mark Polyakov"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:conform #:alexandria #:hunchentoot)
  :pathname "examples"
  :components ((:file "package")
               (:file "html")
               (:file "conformlets")
               (:file "server")))
