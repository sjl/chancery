(in-package :chancery)

(defun vector-reader (stream char)
  (declare (ignore char))
  (coerce (read-delimited-list #\] stream t) 'vector))


(defun eval-reader (stream char)
  (declare (ignore char))
  `(eval ,(read stream t t t)))

(defun gen-reader (stream char)
  (declare (ignore char))
  `(generate ,(read stream t t t)))

(defun gen-string-reader (stream char)
  (declare (ignore char))
  `(generate-string ,(read stream t t t)))


(named-readtables:defreadtable :chancery
  (:merge :standard)
  (:macro-char #\$ #'gen-string-reader nil)
  (:macro-char #\@ #'gen-reader nil)
  (:macro-char #\! #'eval-reader nil)
  (:macro-char #\[ #'vector-reader t)
  (:macro-char #\] (get-macro-character #\) nil)))
