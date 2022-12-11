# Syntax

Wipple has a minimal syntax with just a few constructs:

-   **Comments** are ignored.
-   **Blocks** represent a sequence of lists.
-   **Lists** represent a sequence of expressions.
-   **Operators** and **attributes** change how lists are parsed.

## Comments

A comment begins with `--` and continues until the end of the line. The contents of a comment are ignored. For example, writing `x -- y` is equivalent to writing `x`.

## Blocks

A block begins with `{` and ends with `}`. The top level of a file is also implicitly a block.

Each line in a block is parsed into a list, so `{ (a b c) }` is equivalent to `{ a b c }`. If a line is indented using a tab character, then it becomes part of the previous line. For example:

```wipple
-- This:
a b c
  d e f

-- is equivalent to:
a b c d e f
```

## Lists

A list begins with `(` and ends with `)`. Each statement in a block is also implicitly a list.

If the list contains no operators, then it is evaluated in one of three ways:

-   If the list is empty, then it evaluates to itself.
-   If the list contains one expression, then the list is replaced by the expression. For example, `(foo)` is the same as `foo` and `((foo) (bar) (baz))` is the same as `(foo bar baz)`.
-   If the list contains two or more expressions, then the first expression is called with the remaining expressions.

If the first item in a list is a template, then the template is expanded with the remaining items in the list at compile time. Otherwise, the list is evaluated at runtime. For example, consider a template `duplicate` that accepts an input `x` and evaluates to `(x x)` — writing `duplicate a` is equivalent to writing `a a`.

## Operators

Operators are a type of template that are written between one or more expressions on each side. For example, consider an operator `o` that is placed between two expressions `x` and `yf` and evaluates to `y x` — writing `a o b` is equivalent to writing `b a`.

Every operator has a "precedence", where higher-precedence operators have priority over lower-precedence ones. For example, consider an operator `a` that has a higher precedence than an operator `b` — writing `x a y b c` is equivalent to writing `x a (y b c)`.

Every precedence defines an "associativity", indicating which direction the operators of that precedence should be parsed if there are more than one. For example, consider an operator `o` that is left-associative — writing `x o y o b` is equivalent to writing `(x o y) o b`. Operators do not need to have an associativity; in that case, writing more than one operator in the same list is an error.

## Attributes

Attributes are an alternative way to use templates. An attribute begins with with `[` and ends with `]`, and applies to the line below it. For example:

```
-- This:
[a x]
[b y]
z

-- Is equivalent to:
a x (b y z)
```

## Atoms

Atoms allow you to fill a list with information. There are three kinds of atoms:

-   **Names**: `x`, `foo`, `favorite-color`, `set!`
-   **Numbers**: `42`, `-5`, `3.14`
-   **Text**: `""`, `"Hello, world!"`, `"line 1\nline2"`
