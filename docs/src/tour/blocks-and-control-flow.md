# Blocks and control flow

A block is a piece of code surrounded in braces (`{...}`). Blocks let you store code to run it later. To run the code in a block, use `do`:

```wipple
greeting : {show "Hello, world!"}
do greeting
```

```wipple-output
Hello, world!
```

Without `do`, the block will do nothing:

```wipple
greeting : {show "Hello, world!"}
-- nothing happens
```

You can call `do` on a block multiple times:

```wipple
greeting : {show "Hello, world!"}
do greeting
do greeting
```

```wipple-output
Hello, world!
Hello, world!
```

You can write multiple lines of code in a block; the value of the block is the value of the last line:

```wipple
sum : {
    show "calculating 1 + 2..."
    1 + 2
}

show (do sum)
```

```wipple-output
calculating 1 + 2...
3
```

You don't have to store a block in a variable before calling `do`:

```wipple
sum : do {1 + 2} -- equivalent to `sum : 1 + 2`
```

Blocks are useful for logic and control flow. For example, `if` accepts a condition and two blocks. If the condition is `True`, the first block will be evaluated, and if it's `False`, the second block will be evaluated.

```wipple
secret : 5
guess : 3
if (guess = secret) {show "You win!"} {show "Try again"}
```

```wipple-output
Try again
```

`repeat` accepts a number of `times` and runs the provided block that number of times:

```wipple
repeat (3 times) {
    show "Hello, world!"
}
```

```wipple-output
Hello, world!
Hello, world!
Hello, world!
```

It's important to remember that blocks are values, just like text and numbers are — they can be stored in variables and passed to functions. `repeat` is just a function that accepts a block as input. You can build your types of control flow easily, and we'll do just that in the next chapter!
