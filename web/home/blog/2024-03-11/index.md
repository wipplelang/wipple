---
layout: blog
title: Update on Wipple’s new compiler
date: 2024-03-11
---

I've been working on Wipple's new compiler since January, and I have some updates to share!

## Compiler progress

First, the new compiler architecture is almost complete. I've split each part of the compiler into their own independent Rust crates. All the crates are generic over a "driver", meaning the compiler can be tested in pieces without having to build an entire executable.

In the Wipple Playground, the compiler itself compiles to WebAssembly, and thanks to [ts-rs](https://github.com/Aleph-Alpha/ts-rs), it generates TypeScript typings, too! In fact, the interpreter is now written in TypeScript, removing a bunch of complexity that was required to send Wipple values between Rust and JS at runtime.

I am currently in the process of moving the "render diagnostics" phase of the compiler to TypeScript as well. That way, the compiler emits error codes as JSON and the Wipple Playground can render them in any number of formats. This will come in handy in the future when diagnostics need to be localized — it can all be done on the frontend.

## More syntax updates

I've decided to roll back most of the syntax changes I described in the last update. Comments are once again written with `--`, and blocks use braces. However, the semantics of blocks have changed under the hood!

### Block expressions

With the new compiler, when you wrap some code in a block, that code becomes a **block expression** whose value is computed later. It's the same idea as the `lazy` type described in the last post:

```wipple
block : {1 + 2}
```

In the above example, `block` has type `{Number}`, representing a piece of code that produces a `Number`. In order to actually run the code, you pass the block to the `do` function:

```wipple
-- Evaluate the provided block, returning its result.
do :: Body => {Body} -> Body

three : do {1 + 2}
show three -- 3
```

What makes this model powerful is that other functions accept blocks, too, like `repeat`!

```wipple
-- Execute a block of code repeatedly.
repeat :: Predicate Body Result where (...) => Predicate -> {Body} -> Result

repeat (3 times) {
  show "Hello, world!"
}
```

Of course, if you just want to run multiple statements in order, you can pass a block to `do` directly:

```wipple
greet :: Text -> Text
greet : name -> do {
  show ("Hello, _!" name)
  prompt "How are you?"
}
```

I think block expressions are a great way to solve the problem of delayed evaluation in Wipple. Braces almost universally represent code, but most languages have many different rules on where braces are valid. By making blocks a "first-class value" in Wipple and defining functions that accept blocks, there's now a clear and consistent distinction between control flow and grouping with parentheses. As for whether parentheses are easier to find on the keyboard versus braces, I think the mental model of evaluation this design brings outweighs the initial learning curve of having two different grouping symbols.

### Functions

One of the hardest things for people to learn in Wipple is the idea that functions only have one input. As soon as you encounter an error message related to functions, you have to understand how [currying](https://en.wikipedia.org/wiki/Currying) works, and the idea that (most of!) Wipple's functions return other functions is pretty hard to wrap your head around. So, I have decided to lift the restriction — functions can now have multiple inputs!

For example, here's how `add` would be defined with currying:

```wipple
add :: Number -> Number -> Number
add : a b -> a + b
```

Right away, you have to know that the `->` operator groups from right to left in order to read this code. I added the `a b ->` shorthand for `a -> b ->` a long time ago, but now, in the new compiler, the type of `add` has the same pattern as the definition:

```wipple
add :: Number Number -> Number
add : a b -> a + b
```

And if you try to call `add` with the wrong number of inputs, you get a much nicer error message:

| Code        | Before                                            | After                            |
| ----------- | ------------------------------------------------- | -------------------------------- |
| `add 1`     | `this code doesn't do anything`                   | `expected 2 inputs, but found 1` |
| `add 1 2 3` | `` expected `Number -> _`, but found `Number`  `` | `expected 2 inputs, but found 3` |

Currying is still useful, though, especially once you're introduced to the `.` operator. (`x . f` is equivalent to `f x`.) Most functions in the standard library now accept the "receiving" input as part of a separate function:

| Before                                                     | After                                                   |
| ---------------------------------------------------------- | ------------------------------------------------------- |
| `insert-entry :: Key -> Value -> Dictionary -> Dictionary` | `insert-entry :: Key Value -> Dictionary -> Dictionary` |

In both cases, you call `insert-entry` like `my-dictionary . insert-entry "a" 1` (or alternatively, `(insert-entry "a" 1) my-dictionary` — the parentheses are now required).

### Variables

This one is still experimental, but I have been thinking of replacing `mutable` with language support for variables. Wipple will still emphasize immutable state, but variables (instead of recursion, for example) can sometimes make code easier to reason about.

The basic idea is that the exclamation mark (`!`), currently used as a convention for functions that mutate their `Mutable` input, can be used in a few places to tell Wipple to overwrite a variable's value, rather than creating a new variable with the same name.

```wipple
number : 1
number! : 2
show number -- 2
```

To keep things simple, you can only do this with single variable patterns.

Second, the exclamation mark can be used on a function call with a single variable input to overwrite that variable with the function's return value:

```wipple
increment :: Number -> Number
increment : n -> n + 1

number : 1
increment! number
show number -- 2
```

`f! x` is equivalent to `x! : f x` — the value of `x` is copied into the function, and the function cannot mutate `x` directly. This is different from the `Mutable` type, whose values can be freely moved around, stored in structures, etc.

## New playground

I am also working on a brand-new Wipple Playground, and I'm excited to share more in the coming months!
