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

    (DEFINE-RULE NAME-AND-OPTIONS &REST EXPRESSIONS)

### `DEFINE-STRING` (macro)

    (DEFINE-STRING NAME-AND-OPTIONS &REST EXPRESSIONS)

### `GEN` (macro)

    (GEN EXPRESSION)

Generate a single Chancery expression.

### `GEN-STRING` (macro)

    (GEN-STRING EXPRESSION)

Generate a single Chancery string expression.

### `POS` (function)

    (POS STRING)

Make `string` posessive by adding an apostrophe (and possibly an s).

### `S` (function)

    (S STRING)

Pluralize `string`.

