(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
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
