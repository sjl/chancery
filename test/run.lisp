#+ecl (setf compiler::*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload '(:chancery :chancery.test))
(time (asdf:test-system 'chancery))
(quit)
