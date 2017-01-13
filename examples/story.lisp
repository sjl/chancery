(in-package :chancery)
(named-readtables:in-readtable :chancery)

(defun pronoun (gender)
  (case gender
    (:female "she")
    (:male "he")
    (:neuter "it")
    (t "they")))

(defun posessive-pronoun (gender)
  (case gender
    (:female "her")
    (:male "his")
    (:neuter "its")
    (t "their")))


(defparameter *name* nil)
(defparameter *pronoun* nil)
(defparameter *possessive* nil)

(define-rule hero
  (list "freyja" :female)
  (list "loki" :male)
  (list "time" :neuter)
  (list "frobboz" :frobbian))

(define-rule weapon
  "axe" "sword" "banana")

(define-rule animal
  "mouse" "squirrel" "beaver" "antelope" "rabbit" "elk")

(define-rule monster
  "dragon" "kraken" "chimera")

(define-rule story%
  ([*name* cap] "took up" *possessive* "mighty" weapon "and went forth."
   [*pronoun* cap] "slew the" monster "and the people rejoiced.")
  ("Once upon a time there was" [animal a] "named" [*name* cap] :. "."
   [*pronoun* cap] "went to the village and was very happy."))

(defun story ()
  (destructuring-bind (name gender) (hero)
    (let ((*name* name)
          (*pronoun* (pronoun gender))
          (*possessive* (posessive-pronoun gender)))
      (story%))))
