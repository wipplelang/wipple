# Getting started

Welcome to the Wipple Guide! This guide contains a tour of the Wipple programming language designed for people with an existing programming background.

You can jump between sections using the table of contents on the left.

## Hello, world!

Let's start by displaying the string "Hello, world!" on the screen. Wipple has a built-in function named `show` for this:

```wipple
show "Hello, world!"
```

Notice that in Wipple, you don't need parentheses to call a function. Instead, parentheses surround code that's nested inside a function call, like in these examples:

```wipple
show (increment 1)
show (increment (increment 1))
show (1 + 2)
```

You can also store values in variables to give them a name. In Wipple, variables are defined using a colon (`:`):

```wipple
name : "Wipple"
show name

sum : 1 + 2
show sum
```

If a string contains underscores (`_`s), you can perform string interpolation — variables placed after the string will replace the underscores:

```wipple
show ("Hello, _!" name)
```

## Blocks and control flow

A _block_ is a piece of code surrounded in braces (`{⋯}`). Blocks let you store code to run it later. To run the code in a block, use `do`:

```wipple
greeting : {show "displays second"}
show "displays first"
do greeting
```

Wipple uses blocks to implement control flow. For example, the `if` function accepts a Boolean condition (`True` or `False`) and two blocks. If the condition is `True`, the first block will be evaluated with `do`, and if it's `False`, the second block will be evaluated.

```wipple
secret : 5
guess : 3
if (guess = secret) {show "You win!"} {show "Try again"}
```

If the calls to `show` weren't wrapped in blocks, they would both run and both messages would appear on the screen.

You can run a block with `do` multiple times, which is how `repeat` is implemented:

```wipple
repeat (3 times) {
    show "Hello, world!"
}
```

Just like with `if`, without the block, the message would only be displayed once.

Blocks produce values, which means you can assign the result of `if` to a variable and factor out the `show`:

```wipple
message : if (guess = secret) {"You win!"} {"Try again"}
show message
```

It's important to remember that blocks are values, just like strings and numbers are — they can be stored in variables and passed to functions. `if` and `repeat` are regular functions that accept blocks as input. You can build your types of control flow easily, and we'll do just that in the next section!

## Functions

Functions in Wipple are written with an arrow (`->`) separating the inputs from the output. Just like blocks, Wipple's functions are also values. That means to give a function a name, you assign it to a variable; there's no special syntax for defining a named function. Putting it all together, here's a function that adds two numbers:

```wipple
add : a b -> a + b
show (add 1 2)
```

(Also notice that you don't need to separate multiple inputs with commas.)

If you want to have multiple statements in a function, you can run a block with `do`. If a block has multiple statements, the last one is the function's return value:

```wipple
debug-add : a b -> do {
    show ("called debug-add with _ and _" a b)
    a + b
}

show (debug-add 1 2)
```

Don't forget to put `do` before the block — since blocks are values, without `do`, the function will return the block itself instead of running its code!

You can combine blocks and functions to build your own control flow. Here, `twice` accepts a block as input and runs it twice:

```wipple
twice : block -> do {
    do block
    do block
}

twice {show "Hello, world!"}
```
