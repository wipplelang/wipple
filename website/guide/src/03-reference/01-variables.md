# Variables

In Wipple, you can declare new variables using the `:` operator, which is pronounced _"is"_:

```wipple
favorite-color : orange -- "favorite color is orange"
```

By convention, variable names are lowercase and words are separated using dashes, like in CSS. If `thisStyle` is called "camel case", then `this-style` is called "kebab case"!

Note that `:` must stand on its own, surrounded by whitespace. This doesn't work:

```wipple
favorite-color: orange
```

You can redeclare variables within the same block. But note, redeclaring is not the same as reassigning! Any code referencing the previous variable's value will continue to do so.

```wipple
n : 1
add : x -> x + n

n : 2
add 10 -- 11, not 12
```

If you want to change the value of an existing variable at runtime, you can do so using `Mutable`:

```wipple
n : Mutable 1
add : x -> x + get n

n | set! 2
add 10 -- 12
```

This isn't magic; the actual value of `n` is a reference to a piece of memory, which remains unchanged, but the value stored at that piece of memory can be mutated (hence the need for `get`, which reads from the memory, and `set!`, which writes to it).

Finally, you can create constants by specifying the type of the variable above its declaration:

```wipple
answer :: Number
answer : 42
```

Constants may not be redeclared in the same scope. In addition, constants are lazily loaded and may not capture variables. A file consisting entirely of constants may be imported by another file with `use`.