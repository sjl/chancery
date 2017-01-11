(in-package :chancery)

(defun vector-reader (stream char)
  (declare (ignore char))
  (coerce (read-delimited-list #\] stream t) 'vector))


(named-readtables:defreadtable :chancery
  (:merge :standard)
  (:macro-char #\[ #'vector-reader t)
  (:macro-char #\] (get-macro-character #\) nil)))
