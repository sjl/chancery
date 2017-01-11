# API Reference

The following is a list of all user-facing parts of Chancery.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `CHANCERY`

### `A` (function)

    (A STRING)

Add an indefinite article (a or an) to the front of `string`.

### `CAP` (function)

    (CAP STRING)

Capitalize the first character of `string`.

### `CAP-ALL` (function)

    (CAP-ALL STRING)

Capitalize each word of `string`.

### `DEFINE-RULE` (macro)

    (DEFINE-RULE NAME &REST EXPRESSIONS)

Define a Chancery rule for the symbol `name`.

  Each expression in `expressions` can be any valid Chancery expression.  When
  the rule is invoked one will be chosen at random and evaluated.

  Examples:

    (define-rule name "Alice" "Bob" "Carol")
    (define-rule place "forest" "mountain")
    (define-rule emotion "happy" "sad")

    (define-rule sentence
      (name "was" emotion :. ".")
      (name "went to the" place :. "."))

  

### `POS` (function)

    (POS STRING)

Make `string` posessive by adding an apostrophe (and possibly an s).

### `Q` (function)

    (Q STRING)

Wrap `string` in quotation marks.

### `S` (function)

    (S STRING)

Pluralize `string`.

