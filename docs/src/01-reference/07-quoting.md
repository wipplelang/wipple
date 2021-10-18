# Quoting

You can prefix expressions with a single quote (`'`) to change how they are evaluated: instead of reducing the expression as normal, its structural components are each evaluated individually, and the resulting expression is a value at runtime.

The primary use of quotation is when building lists:

```wipple
numbers : '(1 2 3)
```

Without the quote, the expression would be interpreted as a function call, which is invalid for numbers.

Unlike Lisp, quoting in Wipple is "shallow" and only uses the structural form of the value being directly quoted. This means that `'((f x) (f y))` evaluates `f x` and `f y`. You can quote the list twice to preserve the entire structure:

```wipple
f : x -> x
x : 1
y : 2

'((f x) (f y)) :: List Number
''((f x) (f y)) :: List (List Name)
```

Names have no substructure, so you only need to quote them once:

```wipple
'x :: Name
```

Quoting numbers and text has no effect. Blocks and attributes are compile-time constructs and can't be quoted.
