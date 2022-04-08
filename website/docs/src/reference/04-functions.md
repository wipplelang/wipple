# Functions

Wipple is a hybrid procedural-functional programming language, encouraging the use of functions and composition to make series of steps more natural to read and write. You can declare a new function using the `->` operator, which is pronounced _"becomes"_:

```wipple
-- "name becomes 'Hello, (name)!'"
name -> format "Hello, _!" name

-- "greet is the action of a name becoming ..."
greet : name -> ...

-- "x becomes itself"
x -> x
```

Functions in Wipple may only accept one input. To have multiple inputs, make multiple functions!

```wipple
add : a -> b -> a + b
```

To call a function, just write the function followed by the inputs in a list:

```wipple
add 2 3
```

`(a b c)` is the same as `((a b) c)`. Importantly, this means that functions can be partially applied:

```wipple
increment : add 1 -- 'add 1' returns a new function
increment 5 -- 6
```

Many times, you'll want to write a program as a series of steps. Say you want to load some data from a file, and then parse it:

```wipple
file : load "my-file.csv"
contents : parse file
show contents
```

You can simplify this sequence a bit by using the `.` operator, pronounced _"then"_:

```wipple
-- "load, then parse, then show"
load "my-file.csv" . parse . show
```

You can use `.` with any function! `x . f` is equivalent to `f x`. You can use this property to simulate methods like in object-oriented languages:

```wipple
minus : x -> n -> n - x

3 . minus 2 . minus 1 -- 0
```

By convention, functions that take a "receiving" argument should put the receiver last so that it can be used with `.` notation.

Wipple also supports functors with the `|` operator, whose pronounciation depends on the context in which it's used. For example, you can use `|` to perform an operation on each item in a list, or perform an operation on a `Maybe` if it contains a value:

```wipple
increment : x -> x + 1

-- "increment each number in numbers"
numbers : '(1 2 3)
numbers | increment -- '(2 3 4)

-- "if number? contains a value, increment it"
number? : Some 1
number? | increment -- Some 2
```

Here are some builtin functions that come in handy when using the `.` and `|` operators:

| Function | Type                 | Description                               |
| -------- | -------------------- | ----------------------------------------- |
| `it`     | `A => A -> A`        | Returns its input                         |
| `just`   | `A B => A -> B -> A` | Returns a function that ignores its input |
