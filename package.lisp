(defpackage :chancery
  (:use :cl :chancery.quickutils)
  (:export
    :define-rule
    :define-string

    :create-rule
    :create-string

    :generate
    :generate-string

    :quote
    :eval
    :list
    :vector

    :cap
    :cap-all
    :q
    :a
    :s
    :pos

    :*random*))
