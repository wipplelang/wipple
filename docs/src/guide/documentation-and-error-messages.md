# Documentation and error messages

Wipple displays your documentation comments in IDEs and error messages, and allows you to customize errors using the type system.

## Links in documentation comments

Documentation comments are regular comments (`--`) that appear immediately above a definition. Within documentation comments, you can use Markdown links to reference any defined type parameters. Each of these links will resolve to the expression that corresponds to the type parameter.

For example, the `Add` trait has a documentation comment that looks like this:

```wipple
-- [`Add`] adds [`left`] and [`right`] together, returning a [`sum@type`].
Add : left right (infer sum) => trait (left right -> sum)
```

When you write `1 + 2`:

- `Add` is replaced with the expression `1 + 2`.
- `left` is replaced with the expression `1`, and `right` is replaced with the expression `2`.
- `sum@type` is replaced with `Number`, the type of `sum`.

```markdown
`1 + 2` adds `1` and `2` together, returning a `Number`.
```

Sometimes, your definition will accept a concrete type, such as `Number`, instead of a type parameter, but you still want to reference the provided expression in a link. Wipple supports this with _inline type annotations_:

```wipple
-- [`positive?`] returns `True` if [`n`] is greater than 0.
positive? :: (n :: Number) -> Boolean
positive? : n -> n >= 0

positive? 1 -- `positive?` returns `True` if `1` is greater than `0`.
```

You can use inline type annotations anywhere in the signature, including on types containing type parameters of their own.

## Custom errors with `error` instances

An _error instance_ is an instance marked with the `error` keyword and has no value. When a trait resolves to an error instances, Wipple will replace the default error message with the instance's comment:

```wipple
-- Can't count the contents of a block. Are you missing `do`?
error instance (Count {output})
```

You can combine `error` with `default` to customize the error displayed when no other instances match:

```wipple
-- Can't count the contents of [`value`].
default error instance (Count value)
```

Error instances also support inline type annotations:

```wipple
Count : value => trait (value -> Number)

-- Can't count the contents of [`block`]. Try adding `do` to count the
-- inner [`output`].
error instance (Count (block :: {output}))

number : {123}
Count number -- Can't count the contents of `number`.
```

In addition, you can define traditional "expected/found"-style error messages with the `Mismatched` trait. Whenever a group has more than one type, Wipple will attempt to resolve `Mismatched` instances for each pair of types. To display a custom error message, define an error instance for the types you are interested in.

```wipple
Distance : type {meters :: Number}

[unit] m :: Number -> Distance
m : value -> Distance {meters : value}

-- Missing a unit for distance. Try adding `m` after [`n`].
error instance (Mismatched (n :: Number) Distance)

forward :: Distance -> ()
forward : todo

forward 50
```
