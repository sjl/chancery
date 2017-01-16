(in-package :chancery)
(named-readtables:in-readtable :chancery)

(define-rule (material :distribution (:zipf :exponent 0.7))
  :iron
  :steel
  :silver
  :mithril
  :meteoric-iron
  :adamantine)

(define-rule kind
  :dagger
  :short-sword
  :long-sword
  :axe
  :mace
  :hammer)

(define-rule (bonus :distribution :zipf)
  1 2 3 4)

(define-rule (monster :distribution :weighted)
  (10 :goblin)
  (5 :elf)
  (5 :dwarf)
  (1 :dragon))

(define-rule magic
  (('+ bonus) material kind)
  (material kind :of monster :slaying)
  (:glowing material kind))

(define-rule vanilla
  (material kind))

(define-rule (weapon :distribution :weighted)
  (10 vanilla)
  (2 magic))

(map nil #'print (gimme 40 (weapon)))
