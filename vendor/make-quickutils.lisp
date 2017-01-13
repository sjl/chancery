(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :flip
               :rcurry
               :riffle
               :split-sequence

               )
  :package "CHANCERY.QUICKUTILS")
