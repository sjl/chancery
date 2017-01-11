(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :flip
               :mkstr
               :once-only
               :rcurry
               :riffle
               :split-sequence
               :symb
               :with-gensyms

               )
  :package "CHANCERY.QUICKUTILS")
