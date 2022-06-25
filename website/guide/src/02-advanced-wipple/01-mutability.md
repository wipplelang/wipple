# Mutability

Wipple doesn't allow you to reassign the value of a variable once you've declared it. Instead, you can **shadow** an existing variable by assigning to the same name:

```wipple
x : 1 -- first assignment
x : "hello" -- second assignment
```

Notice how the types don't need to be the same — this is because the two `x`s are distinct values.

Importantly, any code referring to the original `x` will continue to do so:

```wipple
x : 1
show-x : () -> show x
x : 2
show-x () -- displays 1, not 2
```

However, there are circumstances where you actually need to change a variable's value and have that change be shared across the program. To accommodate this, Wipple provides a `Mutable` type!

You can create a new `Mutable` value by using the `mutable` function:

```wipple
-- mutable :: A => A -> Mutable A
x : mutable 1
```

To retrieve the value inside, use `get`:

```wipple
-- get :: A => Mutable A -> A
show-x : () -> show (get x)
```

And use `set!` to change it:

```wipple
-- set :: A => A -> Mutable A -> ()
x . set! 2
```

Now when you call `show-x`, you'll get `2` instead of `1`!

By convention, any function that changes the value of a `Mutable` input ends with `!`. There is no need to append `!` to functions that only mutate internal state.

There are many useful functions for changing mutable values; here are just a few:

<table>
    <tbody>
        <tr>
            <td><strong>Function</strong></td>
            <td><strong>Type</strong></td>
            <td><strong>Description</strong></td>
        </tr>
        <tr>
            <td><code>swap!</code></td>
            <td><code>A => Mutable A -> Mutable A -> ()</code></td>
            <td>Swaps the values of its inputs</td>
        </tr>
        <tr>
            <td><code>add!</code></td>
            <td><code>Left Right where (Add Left Right Left) => Right -> Mutable Left -> ()</code></td>
            <td>Adds a value to its input</td>
        </tr>
        <tr>
            <td><code>increment!</code></td>
            <td><code>A where (Add A Number A) => A -> ()</code></td>
            <td>Increments its input by <code>1</code></td>
        </tr>
    </tbody>
</table>
