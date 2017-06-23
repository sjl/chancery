#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload 'chancery)
(time (asdf:test-system 'chancery))
(quit)
