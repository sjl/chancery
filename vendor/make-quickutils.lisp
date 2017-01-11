(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :mkstr
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "CHANCERY.QUICKUTILS")
