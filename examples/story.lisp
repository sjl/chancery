(in-package :chancery)
(named-readtables:in-readtable :chancery)

(define-rule name
  "arjun"
  "yuuma"
  "jess"
  "bob smith")

(define-rule nature-noun
  "ocean"
  "mountain"
  "forest"
  "cloud"
  "river"
  "tree"
  "sky"
  "sea"
  "desert")

(define-rule animal
  "unicorn"
  "raven"
  "turkey"
  "wallaby"
  "sparrow"
  "scorpion"
  "coyote"
  "eagle"
  "owl"
  "lizard"
  "zebra"
  "duck"
  "kitten")

(define-rule color
  "orange"
  "blue"
  "white"
  "black"
  "grey"
  "purple"
  "indigo"
  "turquoise")

(define-rule activity
  "running"
  "jumping"
  "flying"
  "carousing")

(define-rule sentence
  ("The" color animal "of the" nature-noun "is called" [name cap-all q] :. ".")
  ("The" animal "was" activity "in the" [nature-noun s] :. ".")
  ([name cap-all pos] "favorite color is" color :. ".")
  ([nature-noun cap] "air is fresh.")
  ("The" [animal s] "were" activity "in the" nature-noun :. "."))


(define-rule pronoun
  "he" "she")

(define-rule posessive-pronoun
  "his" "her")

(define-rule omen
  "good omen"
  "bad omen")


(define-rule story%
  ("There once was" [color a] animal "named"  hero :. "."
   cap-pro "journeyed to a distant" nature-noun "to find" [animal a] :. "."
   "On the way" pronoun "saw" (eval (+ 10 (random 10))) [animal s] activity :. "."
   hero "considered this to be a" omen :. ".")
  (bind ((victim animal))
    (hero "came upon a sick" victim :. "."
     cap-pro "touched the" victim "and" posessive-pronoun "wounds were healed.")))

(define-rule story
  (bind* ((hero [name cap-all])
          (pronoun pronoun)
          (cap-pro [pronoun cap]))
    (story%)))

; (iterate (repeat 30) (pr (sentence)))


;;;; Pronoun agreement through binding
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

(define-rule hero
  (list "freyja" :female)
  (list "loki" :male)
  (list "time" :neuter)
  (list "frobboz" :frobbian))

(define-rule weapon
  "axe" "sword" "banana")

(define-rule animal
  "mouse" "squirrel" "beaver" "antelope" "rabbit" "elk")

(define-rule story%
  ([name cap] "took up" posessive "mighty" weapon "and went forth."
   [pronoun cap] "slew the dragon and the people rejoiced.")
  ("Once upon a time there was" [animal a] "named" [name cap] :. "."
   [pronoun cap] "went to the village and was very happy."))

(define-rule story
  (bind* (((name gender) hero)
          (pronoun [gender pronoun])
          (posessive [gender posessive-pronoun]))
    (story%)))
