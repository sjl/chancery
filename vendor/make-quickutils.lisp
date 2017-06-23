(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-list
               :flip
               :range
               :rcurry
               :riffle
               :split-sequence
               :symb

               )
  :package "CHANCERY.QUICKUTILS")
