Usage
=====

Chancery is a library for procedurally generating text and data in Common
Lisp.  It's heavily inspired by [Tracery][], and is essentially just some Lispy
syntactic sugar for writing [grammars][cfg].

[Tracery]: http://www.crystalcodepalace.com/traceryTut.html
[cfg]: https://en.wikipedia.org/wiki/Context-free_grammar

[TOC]

Rules
-----

Rules are the core construct Chancery uses to generate data, and can be created
with `define-rule`:

    :::lisp
    (define-rule animal
      "cat"
      "dog"
      "mouse")

    (define-rule color
      "black"
      "white"
      "brown"
      "gray")

Rules are compiled into vanilla Lisp functions, so you can call them like you
would any other function.

    :::lisp
    (loop :repeat 10
          :collect (list (color) (animal)))
    ; =>
    (("gray" "dog") ("white" "mouse") ("gray" "dog")
     ("black" "mouse") ("gray" "cat") ("gray" "cat")
     ("white" "mouse") ("white" "dog") ("gray" "mouse")
     ("brown" "cat"))

Basic rules select one of their body terms at random and evaluate it.  Most
kinds of objects (e.g. strings, keywords, numbers) evaluate to themselves, but
there are a few exceptions.

Symbols evaluate to `(funcall 'symbol)`, so rules can easily call other rules:

    :::lisp
    (define-rule cat-name
      "fluffy"
      "whiskers")

    (define-rule dog-name
      "spot"
      "fido"
      "lassie")

    (define-rule animal-name
      cat-name
      dog-name)

    (loop :repeat 10 :collect (animal-name))
    ; =>
    ("spot" "spot" "fido" "fluffy" "fido" "spot"
     "fluffy" "spot" "spot" "lassie")

Lists recursively evaluate their members and return the result as a fresh list:

    :::lisp
    (define-rule pet
      (cat-name color :cat)
      (dog-name color :dog))

    (loop :repeat 5 :collect (pet))
    ; =>
    (("fluffy" "brown" :CAT) ("fluffy" "white" :CAT)
     ("fido" "brown" :DOG) ("lassie" "white" :DOG)
     ("fluffy" "black" :CAT))

If you want to return a literal symbol or list from a rule you can `quote` it:

    :::lisp
    (define-rule foo
      'a
      '(x y))

    (loop :repeat 5 :collect (foo))
    ; =>
    (A (X Y) (X Y) A A)

Distributions
-------------

By default each body term in a rule has an equal chance of being chosen.

Chancery also includes support for weighting the terms so some will be chosen
more often than others:

    :::lisp
    (define-rule (metal :distribution :weighted)
      (10 iron)
      (5 steel)
      (2 silver)
      (1 gold)
      (1 platinum))

Weighting each term by hand can be tedious.  Chancery can automatically
calculate weights based on the order of the body terms according to [Zipf's
law][zipf]:

    :::lisp
    (define-rule (armor-type :distribution :zipf)
      :scale-mail
      :chain-mail
      :banded-mail
      :plate-mail)

[zipf]: https://en.wikipedia.org/wiki/Zipf's_law

Text Generation
---------------

Modifiers
---------

Evaluation
----------

Reader Macros
-------------
