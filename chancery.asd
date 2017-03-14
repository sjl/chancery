(asdf:defsystem :chancery
  :description "A library for procedurally generating text, inspired by Tracery."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "1.0.0"

  :depends-on (:named-readtables)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "readtable")
                             (:file "chancery")))))
