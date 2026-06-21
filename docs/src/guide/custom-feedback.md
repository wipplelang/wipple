# Custom feedback

In Wipple, you're encouraged to provide custom feedback when your API is used incorrectly. Wipple supports defining custom error messages using the type system.

## Error instances

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

The example above uses a link to reference the `value` parameter. Wipple will automatically replace the link with the first member of the group `value` belongs to:

```wipple
Uncountable : type

x : Uncountable
Count x -- Can't count the contents of `x`.
```

To list all members of the group, use ``[`value@related`]``, and to display the type of the group, use ``[`value@type`]``.

```wipple
-- Can't count the contents of [`value`] (a [`value@type`] value).
default error instance (Count value)

x : Uncountable
Count x -- Can't count the contents of `x` (a `Uncountable` value).
```

Finally, if you want to reference group members in an error message, but your instance works with concrete types, you can use an inline type annotation:

```wipple
Count : value => trait (value -> Number)

-- Can't count the contents of [`block`]. Try adding `do` to count the
-- inner [`output`].
error instance (Count (block :: {output}))

number : {123}
Count number
```

## The `Mismatched` trait

Wipple also supports defining traditional "expected/found"-style error messages with the `Mismatched` trait. Whenever a group has more than one type, Wipple will attempt to resolve `Mismatched` instances for each pair of types. To display a custom error message, define an error instance for the types you are interested in.

```wipple
Distance : type {m :: Number}

[unit] m :: Number -> Distance
m : value -> Distance {m : value}

[unit] cm :: Number -> Distance
cm : value -> (value / 100) m

-- Missing a unit for distance. Try adding `m` or `cm` after [`n`].
error instance (Mismatched (n :: Number) Distance)

forward :: Distance -> ()
forward : todo

forward 50
```
