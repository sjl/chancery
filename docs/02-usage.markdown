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

Basic rules select one of their body terms at random and evaluate it according
to some special rules.  Most kinds of objects (e.g. strings, keywords, numbers)
evaluate to themselves, but there are a few exceptions.

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

Text Generation
---------------

Grammars are often used to generate strings, and Chancery has a few extra bits
of syntactic sugar to make it easy to generate readable text.  `define-string`
can be used to define a rule function that stringifies its result before
returning it:

    :::lisp
    (define-string animal "cat" "dog" "mouse")
    (define-string name "Alice" "Bob" "Carol")
    (define-string place "mountain" "forest" "river")

    (define-string story
      (name "the" animal "went to the" place)
      ("a friendly" animal "lived near a" place))

    (loop :repeat 5 :collect (story))
    ; =>
    ; ("Alice the mouse went to the river"
    ;  "Bob the cat went to the forest"
    ;  "a friendly mouse lived near a river"
    ;  "a friendly cat lived near a forest"
    ;  "Bob the cat went to the river")

`define-string` works the same way as `define-rule`, except that the evaluation
of certain items is slightly different.

Strings evaluate to themselves.  `NIL` evaluates to an empty string.  Other
non-keyword symbols evaluate to procedure calls, as with `define-rule`.

Lists evaluate their arguments and concatenate them with one `#\Space` between
each.  If you don't want a space between two items you can use the special
symbol `:.` to suppress it:

    :::lisp
    (define-string foo "x" "y")

    (define-string with-spaces
      (foo foo foo))

    (define-string without-spaces
      (foo :. foo :. foo))

    (with-spaces)    ; => "x x y"
    (without-spaces) ; => "xyx"

The special form `(quote ...)` evaluates to its argument, *without* stringifying
it.  Note that this is one case where a `define-string` could return something
that isn't a string, so use it with care.

Everything else (except vectors, which we'll talk about shortly) evaluates to
the result of calling `princ-to-string` on it.

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

The Zipf distribution can itself take an argument `exponent`, which is the
exponent characterizing the distribution.  The default is `1.0`, and larger
exponents will result in the earlier terms being chosen more often:

    :::lisp
    (define-rule (foo :distribution :zipf)
      :a :b :c :d :e)

    (define-rule (bar :distribution (:zipf :exponent 3.0))
      :a :b :c :d :e)

    (define-rule (baz :distribution (:zipf :exponent 0.25))
      :a :b :c :d :e)

    (count :a (loop :repeat 1000 :collect (foo)))
    ; => 413

    (count :a (loop :repeat 1000 :collect (bar)))
    ; => 831

    (count :a (loop :repeat 1000 :collect (baz)))
    ; => 257

[zipf]: https://en.wikipedia.org/wiki/Zipf's_law

Single Generation
-----------------

Sometimes it can be handy to evaluate a Chancery expression without defining an
actual rule.  The `generate` macro evaluates vanilla Chancery expressions, and
`generate-string` evaluates Chancery string expressions:

    :::lisp
    (define-rule x 1 2 3)
    (define-rule animal "cat" "dog" "mouse")

    (generate :foo)    ; => :FOO
    (generate x)       ; => 2
    (generate (x x x)) ; => (2 2 1)

    (generate-string :foo)             ; => "FOO"
    (generate-string x)                ; => "1"
    (generate-string ("fuzzy" animal)) ; => "fuzzy mouse"

String Modifiers
----------------

Procedurally generating readable text can be tricky, so Chancery includes some
syntax and a few helper functions to make your life easier.

Inside a `define-string` vectors are evaluated specially.  The first element of
the vector is evaluated as usual in `define-string`.  All remaining elements in
the vector are treated as function names, and the result is threaded through
each function in turn.  Finally, the resulting value will be stringified. For
example:

    :::lisp
    (define-rule animal "cat" "dog" "mouse")

    (generate-string ("the" animal "ran"))
    ; =>
    ; "the cat ran"

    (generate-string ("the" #(animal string-upcase) "ran"))
    ; =>
    ; "the MOUSE ran"

Chancery defines a few helpful functions for generating English text:

* `s` pluralizes its argument.
* `a` adds an indefinite article ("a" or "an") to the front of its argument.
* `cap` capitalizes the first letter of its argument.
* `cap-all` capitalizes each word in its argument.)
* `pos` makes its argument possessive by adding an apostrophe (and possibly an s).

These are just normal Lisp functions that happen to be useful as modifiers:

    :::lisp
    (define-string animal "cat" "aardvark" "fly")

    (loop :repeat 5
          :collect (generate-string
                     ("a bunch of" #(animal s))))
    ; =>
    ; ("a bunch of flies" "a bunch of flies" "a bunch of aardvarks"
    ;  "a bunch of cats" "a bunch of aardvarks")

    (loop :repeat 5
          :collect (generate-string
                     (#(animal a cap) "is a good pet")))
    ; =>
    ; ("A cat is a good pet" "A fly is a good pet" "A fly is a good pet"
    ;  "A cat is a good pet" "An aardvark is a good pet")

English is a messy language.  Pull requests to improve these functions (or add
more useful ones) are welcome.

Evaluation
----------

Sometimes it can be useful to switch out of "Chancery evaluation" and back into
normal Lisp evaluation.  The `(eval ...)` special form can be used to do this in
Chancery rules and string rules:

    :::lisp
    (define-rule treasure
      :weapon
      :armor
      ((eval (random 100)) :gold))

    (loop :repeat 5 :collect (treasure))
    ; =>
    ; ((93 :GOLD)
    ;  :WEAPON
    ;  (83 :GOLD)
    ;  :WEAPON
    ;  :ARMOR)

    (define-string animal "cat" "dog")
    (define-string story
      ("The" animal "had" (eval (+ 2 (random 3))) "friends."))

    (loop :repeat 5 :collect (story))
    ; =>
    ; ("The dog had 3 friends." "The cat had 2 friends." "The dog had 3 friends."
    ;  "The cat had 4 friends." "The dog had 2 friends.")

You can think of `eval` and `generate` as two sides of the same coin, kind of
like quasiquote's backquote and comma.  `eval` flips from Chancery evaluation to
Lisp evaluation, and `generate` flips in the other direction.

Reader Macros
-------------

Chancery provides four reader macros you can use to make defining grammars
even more concise.  It uses [named-readtables][] to keep them safely stashed
away unless you want them.

The first reader macro is `[...]` to replace the standard vector `#(...)`
syntax, to make string modifiers stand out more:

    :::lisp
    (named-readtables:in-readtable :chancery)

    (define-string animal "cat" "dog")

    (generate-string ("a few" [animal s]))
    ; => "a few cats"

The reader macro `!form` expands to `(eval form)`:

    :::lisp
    (named-readtables:in-readtable :chancery)

    (define-rule treasure
      :weapon
      :armor
      (!(random 100) :gold))

    (loop :repeat 5 :collect (treasure))
    ; =>
    ; (:ARMOR (53 :GOLD) (49 :GOLD) :ARMOR :WEAPON)

The reader macro `@form` expands to `(generate form)`, and `$form` expands to
`(generate-string form)`.  Note that none of th

    :::lisp
    (named-readtables:in-readtable :chancery)

    (define-string animal "cat" "dog")

    $("the" animal "ran")
    ; => "the cat ran"

Together these let you jump in and out of Chancery-style evaluation as needed:

    :::lisp
    (named-readtables:in-readtable :chancery)

    (define-string animal "cat" "dog")

    (define-string story
      ("the" animal "went to sleep")
      !(let ((pet (animal)))
         $("the" !pet "was a good" !pet)))

    (loop :repeat 4 :collect (story))
    ; =>
    ; ("the dog was a good dog" "the cat was a good cat"
    ;  "the dog was a good dog" "the cat went to sleep")

This last example is a little tricky, but it helps to think of `$` and `@` as
quasiquote's backquote, and `!` as comma.  Flipping back and forth between Lisp
and Chancery can let you define some really interesting grammars.





[named-readtables]: https://common-lisp.net/project/named-readtables/

Runtime Rule Creation
---------------------

All of the Chancery things we've been using so far have been macros.  If you
want to create rules at runtime you can use their function counterparts:

* `create-rule` takes a list of expressions and options, and returns a rule
  function.
* `create-string` takes a list of expressions and options, and returns a string
  function.
* `invoke-generate` takes an expression and evaluates it.
* `invoke-generate-string` takes an expression and evaluates it and stringifies
  the result.

For example:

    (define-rule foo :a :b :c)
    (foo)
    ; versus
    (funcall (create-rule (list :a :b :c)))

    (define-string (bar :distribution :zipf)
      ("one" foo)
      ("two" [foo s]))
    (bar)
    ; versus
    (funcall (create-string (list '("one" foo)
                                  '("two" [foo s]))
               :distribution :zipf))

Tips
----

Chancery aims to be a very thin layer on top of Lisp.  Chancery rules are just
vanilla Lisp functions—you can `describe` them, `disassemble` them, `trace`
them, `funcall` them, and anything else you might normally want to do with
functions.  Similarly, Chancery string modifiers are just unary functions—you
can create your own, or even use built-in Lisp functions like `string-upcase`.

Remember that you can flip back and forth between Chancery evaluation and normal
Lisp evaluation.  Sometimes it's easier to just do a quick `!(if foo then else)`
than to make Yet Another Rule.  Use your own judgement to determine when a rule
is getting too hairy and needs to be split into simpler parts, just like you
would for any other Lisp function.

Examples
--------

If you want some less trivial examples than the ones seen here you might want to
take a look at some of the Twitter bots I've built with Chancery:

* [@git\_commands](http://twitter.com/git_commands)
  ([code](https://github.com/sjl/magitek/blob/master/src/robots/git-commands.lisp))
  tweets procedurally-generated Git command summaries.
* [@lisp\_talks](http://twitter.com/lisp_talks)
  ([code](https://github.com/sjl/magitek/blob/master/src/robots/lisp-talks.lisp))
  tweets random topics for Lisp talks.
* [@rpg\_shopkeeper](http://twitter.com/rpg_shopkeeper)
  ([code](https://github.com/sjl/magitek/blob/master/src/robots/rpg-shopkeeper.lisp))
  generates random fantasy RPG items, and sells them for appropriate prices.
