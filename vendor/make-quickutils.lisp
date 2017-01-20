(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-list
               :flip
               :hash-table-alist
               :range
               :rcurry
               :riffle
               :split-sequence

               )
  :package "CHANCERY.QUICKUTILS")
