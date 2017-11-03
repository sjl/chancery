# API Reference

The following is a list of all user-facing parts of Chancery.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `CHANCERY`

### `*RANDOM*` (variable)

The random number generation function to use (default: `CL:RANDOM`).

### `A` (function)

    (A STRING)

Add an indefinite article (a or an) to the front of `string`.

### `CAP` (function)

    (CAP STRING)

Capitalize the first character of `string`.

### `CAP-ALL` (function)

    (CAP-ALL STRING)

Capitalize each word of `string`.

### `CREATE-RULE` (function)

    (CREATE-RULE EXPRESSIONS &REST OPTIONS)

Return a function that will return random elements of `expressions`.

  `options` should be of the form:

    (&key documentation (distribution :uniform) (arguments '()))

  `documentation` will be used as a docstring for the resulting function.

  `distribution` denotes the distribution of elements returned.

  `arguments` is the arglist of the resulting function.

  Examples:

    (create-rule (list :blue :red :green))

    (create-rule (list :copper :silver :gold :platinum)
      :documentation "Return a random metal."
      :distribution :zipf)

  See the full documentation for more information.

  

### `CREATE-STRING` (function)

    (CREATE-STRING EXPRESSIONS &REST OPTIONS)

Return a function that will return random stringified elements of `expressions`.

  `options` should be of the form:

    (&key documentation (distribution :uniform) (arguments '()))

  `documentation` will be used as a docstring for the resulting function.

  `distribution` denotes the distribution of elements returned.

  `arguments` is the arglist of the resulting function.

  Examples:

    (create-string (list "white" "gray" "black"))

    (create-string '((100 (color "cat"))
                     (100 (color "dog"))
                     (100 (color "dragon")))
      :distribution :weighted)

  See the full documentation for more information.

  

### `DEFINE-RULE` (macro)

    (DEFINE-RULE NAME-AND-OPTIONS &REST EXPRESSIONS)

Define a function that will return random elements of `expressions`.

  `name-and-options` should be of the form:

    (name &key documentation (distribution :uniform) (arguments '()))

  If no options are needed a bare symbol can be given.

  `name` is the symbol under which the resulting function will be defined.

  `documentation` will be used as a docstring for the resulting function.

  `distribution` denotes the distribution of elements returned.

  `arguments` is the arglist of the resulting function.

  Examples:

    (define-rule color
      :blue
      :green
      :red)

    (define-rule (metal :documentation "Return a random metal."
                        :distribution :zipf)
      :copper
      :silver
      :gold
      :platinum)

  See the full documentation for more information.

  

### `DEFINE-STRING` (macro)

    (DEFINE-STRING NAME-AND-OPTIONS &REST EXPRESSIONS)

Define a function that will return random stringified elements of `expressions`.

  `name-and-options` should be of the form:

    (name &key documentation (distribution :uniform) (arguments '()))

  If no options are needed a bare symbol can be given.

  `name` is the symbol under which the resulting function will be defined.

  `documentation` will be used as a docstring for the resulting function.

  `distribution` denotes the distribution of elements returned.

  `arguments` is the arglist of the resulting function.

  Examples:

    (define-string color "white" "gray" "black")

    (define-string (animal :distribution :weighted)
      (100 (color "cat"))
      (100 (color "dog"))
      (100 (color "dragon")))

  See the full documentation for more information.

  

### `GENERATE` (macro)

    (GENERATE EXPRESSION)

Generate a single Chancery expression.

  Example:

    (define-rule x 1 2 3)

    (generate (x x x))
    ; => (1 3 1)

  

### `GENERATE-STRING` (macro)

    (GENERATE-STRING EXPRESSION)

Generate and stringify a single Chancery string expression.

  Example:

    (define-string x 1 2 3)

    (generate-string (x x x))
    ; => "1 3 1"

  

### `POS` (function)

    (POS STRING)

Make `string` posessive by adding an apostrophe (and possibly an s).

### `S` (function)

    (S STRING)

Pluralize `string`.

