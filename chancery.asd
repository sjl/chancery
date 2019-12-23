(asdf:defsystem :chancery
  :description "A library for procedurally generating text, inspired by Tracery."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/chancery/"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:named-readtables)

  :in-order-to ((asdf:test-op (asdf:test-op :chancery.test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "readtable")
                             (:file "chancery")))))
