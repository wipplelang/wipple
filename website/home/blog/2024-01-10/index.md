---
layout: blog
title: Wipple's new compiler and language changes
date: 2024-01-10
---

I'm excited to announce that I've been working on a new compiler for Wipple! The new compiler is being written from the ground up for performance and stability. In addition, I've made some changes to Wipple's syntax and features to make the language easier to learn and use that will debut alongside the new compiler.

## Syntax updates

The most visible change in Wipple is the new syntax for comments and blocks. Comments are now written using brackets, and blocks are written with parentheses:

```wipple
[Represents a sport.]
Sport : type (
  name :: Text
  emoji :: Text
  players :: Number
)

instance (Show Sport) : ...

basketball : Sport (
  name : "Basketball"
  emoji : `🏀`
  players : 5
)

show basketball [Basketball 🏀 has 5 players]
```

In writing, parentheses are used much more often than braces, and students I've worked with who are still learning to type have trouble finding the braces on the keyboard. It's also hard for beginners to remember when to use braces versus parentheses, so having a single grouping symbol reduces the learning curve.

Comments' bracket syntax takes the place of attributes, which have been removed from the language. Wipple's new compiler parses comments rather than ignoring them, and can use them to generate documentation just like the previous `[help]` attributes.

## New compilation model

Although the final program is the same, Wipple's new compiler works much differently internally. Rather than parsing and compiling every file every time, the new compiler generates intermediate artifacts that can be reused. When compiling a library, you specify the list of files to be treated as a single unit:

```shell
$ wipple compile base/*.wipple \
    --interface base.wippleinterface \
    --library base.wipplelibrary
```

(`base` is the new name for the standard library, previously called `std`).

The `.wippleinterface` file contains the definitions of types, traits, constants, and instances contained within the provided files. The `.wipplelibrary` file contains the compiled bytecode for these constants and instances. The artifacts correspond to C's `.h` and `.o` files, respectively.

Now, we can use `base.wippleinterface` to refer to standard library definitions from other files:

```shell
$ wipple compile turtle.wipple base.wippleinterface \
    --interface turtle.wippleinterface \
    --library turtle.wipplelibrary
```

Now only `turtle.wipple` will be parsed and compiled — the results of `base` are reused!

Finally, we can compile our main program against `turtle` and `base` to get an executable:

```shell
$ wipple compile main.wipple base.wippleinterface turtle.wippleinterface \
    --library main.wipplelibrary
$ wipple link *.wipplelibrary -o main
$ ./main
```

`main` contains all the bytecode for our program — behind the scenes, it contains `#!/usr/bin/env wipple run` to actually run the code. You can also call `wipple run main` directly.

This new compilation model is more complicated, but the Wipple Playground will do it all automatically for you. And it means that instead of shipping the standard library's source code with the playground, I can just provide the `.wippleinterface` and `.wipplelibrary` files to be linked against! The result should be a dramatic speedup.

In order for all of this to work, the new compiler no longer monomorphizes constant and instance definitions, instead opting for dynamic dispatch for traits at runtime. Essentially, all of the instances of a trait are stored in a list that's iterated over whenever a trait expression is encountered. This decision is a tradeoff between compilation speed and runtime speed — the program will run more slowly if a lot of traits are used, but the compiler doesn't have to recursively resolve generic definitions anymore. In the Wipple Playground, code is compiled on every keystroke and usually spends most of the time waiting for user interaction while it's running, so I think this is a good tradeoff.

## Structure expressions and lazy values

In the new compiler, support for custom syntax has been removed. Instead, there are two new constructs that cover almost all of the use cases for custom syntax: structure expressions and lazy values!

Structure expressions are blocks containing only variable assignments. Previously, to initialize a structure, you provided the structure's name followed by this block. Now, the typechecker will infer the structure! This means you can write your own functions with "named parameters":

```wipple
[Options for `fraction`.]
Fraction-Options : type (
    round :: Number
)

[Display a number as a fraction.]
fraction :: Fraction-Options -> Number -> Text
fraction : ...

pi : 3.14 . fraction (round : 1)

show pi [31 / 10]
```

Structures also now support default values:

```wipple
Box : type (
    width :: Pixels
    width : 100 pixels

    height :: Pixels
    height : 100 pixels

    color :: Color
    color : `red`
)

[Draw a box on the screen.]
box :: Box -> ()
box : ...

box (color : `blue`)
```

If you still want to specify the name of the structure, you can — the new compiler generates a constructor function that just returns the provided structure expression:

```wipple
Box :: Box -> Box
```

The second new feature is lazy values: values that aren't computed right away. Non-lazy values are implicitly converted into `lazy` values, and you can use `evaluate` to compute the value. For example, `repeat` is now a regular function that accepts a lazy body:

```wipple
[Determines whether to evaluate a `repeat` body again.]
Repeat-Predicate : Predicate Body Result => trait (Predicate -> Body -> Result)

[Repeat using the `Control-Flow` produced by the `repeat` body.]
with-control-flow :: With-Control-Flow _
with-control-flow : With-Control-Flow

With-Control-Flow : Result => type

Result => instance (Repeat-Predicate With-Control-Flow (Control-Flow Result) Result) :
    _ body -> body

[Repeat so long as the provided condition is `True`.]
while :: lazy Boolean -> While
while : condition -> (condition : condition)

While : type (condition :: lazy Boolean)

instance (Repeat-Predicate While () ()) : (condition : condition) body ->
    if (evaluate condition) Continue (Break ())

[Repeat forever.]
forever :: Forever
forever : Forever

Forever : type

instance (Repeat-Predicate Forever _ _) : _ _ -> Continue

[Execute a block of code repeatedly.]
repeat :: Predicate Body Result where (Repeat-Predicate Predicate Body Result) =>
    Predicate -> lazy Body -> Result
repeat : predicate body -> (
    predicate : Repeat-Predicate predicate

    [The predicate and body are grouped together in a tuple to allow for
     tail-call optimization.]
    repeat' :: Body Result => ((Body -> Result) ; lazy Body) -> Result
    repeat' : (predicate ; body) -> when (predicate (evaluate body)) (
        Continue -> repeat' (predicate ; body)
        Break result -> result
    )

    repeat' (predicate ; body)
)
```

The resulting API is identical to the one that uses custom syntax!

```wipple
[Displays "1 2 3 4 5 6 7 8 9 10" on the screen.]
counter : mutable 1
repeat (while (get counter < 10)) (
    show (get counter)
    increment! counter
)
```

## Conclusion

In the playground and throughout the courses, Wipple will look and work mostly the same. But under the hood, almost everything has changed. The new compiler's implementation is available on GitHub under the [`new-compiler`](https://github.com/wipplelang/wipple/pull/140) branch. I look forward to releasing it once it's ready!
