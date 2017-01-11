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

