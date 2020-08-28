(asdf:defsystem :chancery.test
  :description "Test suite for Chancery"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:chancery
               :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "chancery.test:run-tests"))))
