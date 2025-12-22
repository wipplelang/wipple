# Wipple Guide

Welcome to the Wipple Guide! This guide contains a tour of the Wipple programming language designed for people with an existing programming background.

You can jump between sections using the **table of contents** on the left.

To try any example, click on **Open in Playground**.

## Getting started

### Hello, world!

Let's start by displaying the string "Hello, world!" on the screen. Wipple has a built-in function named `show` for this:

```wipple,playground
show "Hello, world!"
```

Notice that in Wipple, you don't need parentheses to call a function. Instead, parentheses surround code that's nested inside a function call, like in these examples:

```wipple
show (increment 1)
show (increment (increment 1))
show (1 + 2)
```

You can also store values in variables to give them a name. In Wipple, variables are defined using a colon (`:`):

```wipple,playground
name : "Wipple"
show name

sum : 1 + 2
show sum
```

If a string contains underscores (`_`s), you can perform string interpolation — variables placed after the string will replace the underscores:

```wipple
show ("Hello, _!" name)
```

### Blocks and control flow

A _block_ is a piece of code surrounded in braces (`{⋯}`). Blocks let you store code to run it later. To run the code in a block, use `do`:

```wipple,playground
greeting : {show "displays second"}
show "displays first"
do greeting
```

Wipple uses blocks to implement control flow. For example, the `if` function accepts a Boolean condition (`True` or `False`) and two blocks. If the condition is `True`, the first block will be evaluated with `do`, and if it's `False`, the second block will be evaluated.

```wipple,playground
secret : 5
guess : 3
if (guess = secret) {show "You win!"} {show "Try again"}
```

If the calls to `show` weren't wrapped in blocks, they would both run and both messages would appear on the screen.

You can run a block with `do` multiple times, which is how `repeat` is implemented:

```wipple,playground
repeat (3 times) {
    show "Hello, world!"
}
```

Just like with `if`, without the block, the message would only be displayed once.

Blocks produce values, which means you can assign the result of `if` to a variable and factor out the `show`:

```wipple,playground
message : if (guess = secret) {"You win!"} {"Try again"}
show message
```

It's important to remember that blocks are values, just like strings and numbers are — they can be stored in variables and passed to functions. `if` and `repeat` are regular functions that accept blocks as input. You can build your types of control flow easily, and we'll do just that in the next section!

### Functions

Functions in Wipple are written with an arrow (`->`) separating the inputs from the output. Just like blocks, Wipple's functions are also values. That means to give a function a name, you assign it to a variable; there's no special syntax for defining a named function. Putting it all together, here's a function that adds two numbers:

```wipple,playground
add : a b -> a + b
show (add 1 2)
```

(Also notice that you don't need to separate multiple inputs with commas.)

If you want to have multiple statements in a function, you can run a block with `do`. If a block has multiple statements, the last one is the function's return value:

```wipple,playground
debug-add : a b -> do {
    show ("called debug-add with _ and _" a b)
    a + b
}

show (debug-add 1 2)
```

Don't forget to put `do` before the block — since blocks are values, without `do`, the function will return the block itself instead of running its code!

You can combine blocks and functions to build your own control flow. Here, `twice` accepts a block as input and runs it twice:

```wipple,playground
twice : block -> do {
    do block
    do block
}

twice {show "Hello, world!"}
```

## The type system

Wipple has a powerful _type system_ that can catch bugs in your program. Types in Wipple begin with a capital letter, such as `String`. All Wipple code is checked at compile time to ensure it has exactly one known data type. A lot of the time, Wipple can determine this type automatically — for example, Wipple knows that `"abc"` is a `String`, which means the `show` in `show "abc"` must be a function accepting a `String`.

### How it works

At compile time, Wipple simulates the flow of information through the program and sorts the code into _groups_. All members of each group must have the same data type. Groups are formed using a few rules:

-   Variables are grouped with their values.

-   All uses of a variable belong to the same group. In other words, accessing the same variable will always give you a value of the same data type.

-   When you call a function, each argument is grouped with its corresponding parameter.

-   When you run a block with `do` or call a function, the result is grouped with the output defined within the block or function.

Let's apply these rules to an example program:

```wipple,playground
increment : x -> x + 1
a : increment 123
b : increment "abc"
```

We get two groups of interest:

1.  The group containing `x`, `123`, and `"abc"` (the inputs).

2.  The group containing `x + 1`, `a`, and `b` (the outputs).

After forming groups, Wipple then considers data types like `Number` and `String`. Unlike in other languages, data types apply to groups rather than individual expressions. In the first group:

-   `x` doesn't contribute a type of its own.
-   `123` contributes `Number` to the group.
-   `"abc"` contributes `String` to the group.

That means Group 1 contains two data types, `Number` and `String`. Since there is more than one type in the group, Wipple produces an error, which appears on the first member of the group (the input `x`).

```
x -> x + 1
~ error: `x` is a `Number` or a `String`, but it can only be one of these.
```

Wipple's approach differs from languages where you might see an "expected `Number`, but found `String`" error message on `"abc"`. These languages create a placeholder for the input `x` that is replaced with `Number` when the function is first called with `123`. Wipple, on the other hand, doesn't assume any one group member is the "right" candidate — it waits to fill the placeholders with concrete data types until all group members are considered, and is thus able to provide more accurate error messages.

As a bonus, this group-based design means you can easily see code related to your cursor! Try pasting this program into the playground, click on `x`, and observe how `123` and `"abc"` are highlighted.

### Type annotations

In addition to letting Wipple infer types, you can use `::` to annotate any piece of code with its expected data type, and Wipple will verify that it's correct at compile time. These _type annotations_ serve as a form of documentation, describing the kinds of values your code works with and produces.

```wipple,playground
sum : 1 + 1
show (sum :: Number) -- correct
show (sum :: String) -- error
```

As a special case, if you add a type annotation on its own line, Wipple defines a _constant_ value that is "lifted" out of the normal control flow and can be used anywhere. Constant values are evaluated when they are used, not at their definition like with variables.

```wipple,playground
show pi -- defined below

pi :: Number
pi : 3.14
```

In general, type annotations mirror the syntax of their corresponding expression. If you use a lowercase name in a type annotation, the constant becomes _generic_ and can work with any type:

```wipple
add :: Number Number -> Number
add : a b -> a + b

identity :: value -> value
identity : x -> x

if :: Boolean {output} {output} -> output
if : todo
```

When a constant is generic, a **copy** of its type annotation is made whenever it is referenced. That means uses of constants don't share groups with each other.

### Constraints

Recall our earlier program involving `increment` that had an error (because `x`, `123`, and `"abc"` were all grouped together):

```wipple,playground
increment : x -> x + 1
a : increment 123
b : increment "abc"
```

Let's try to fix the error by making `increment` generic, and run our rules again!

```wipple,playground
increment :: value -> value
increment : x -> x + 1

a : increment 123
b : increment "abc"
```

Now our groups look different:

1.  One group contains a unique copy of `value` and `123`. This group has type `Number`.
2.  The other group contains a unique copy of `value` and `"abc"`. This group has type `String`.

In addition, because `value` is also the output of the function, Group 1 contains `a` and Group 2 contains `b`. Therefore, `a` is a `Number` and `b` is a `String` as expected.

However, in fixing this issue, we have introduced another one! Because a new copy of `value` is made each time `increment` is referenced, _inside_ the definition of `increment`, `value` is unknown. In other words, because our constant is generic, we can no longer assume anything about the input. That means `x + 1` isn't necessarily a valid operation. (It may be when `value` is a `Number`, but not when it is a `String`.)

Wipple supports fixing this new error by introducing a _constraint_ to our function. With the `where` keyword, we move the requirement that `+ 1` is valid to each user of `increment`. It looks like this:

```wipple,playground
increment :: value -> value where (Add value Number value)
increment : x -> x + 1
```

Reading this code out loud, it says "`increment` requires that you can `Add` your `value` and a `Number` to get a new `value`."

(As an exercise, what would the constraint be for the function `double` shown below?)

```wipple
double :: value -> value where ???
double : x -> x + x
```

Now it's up to each caller of `increment` to ensure that the input they provide can be added with a `Number`.

```wipple
a : increment 123 -- works!
b : increment "abc" -- error: can't add `"abc"` and `Number`
```

Constraints are a powerful way to make your code more flexible. Wipple has several built-in constraints, including `Add` for `+`; `Describe`, which converts the input to a `String` and is used by `show`; and `Equal`, which lets you compare two values for equality.

### Traits and instances

Wipple also supports defining _traits_, which can be used to make custom constraints. They work similarly to interfaces in other languages and look like this:

```wipple
Count : container => trait (container -> Number)
```

Let's look at each component:

```
Count : container => trait (container -> Number)
-----   ---------           -------------------
name    parameters          value
```

The `=>` means that `Count` behaves like a function on types: provide an input type corresponding to `container`, such as `String`, and you get back a constraint that requires `Count` to have a valid `String -> Number` value.

Before we can use a trait, we also have to define its value for each type we're interested in. That is done using `instance`:

```wipple
instance (Count (List element)) : list -> length list
instance (Count String) : string -> length (characters string)
instance (Count (Dictionary key value)) : dict -> length (keys dict)
```

You can use a trait directly, in which case the parameters are inferred, or you can use it as part of a constraint:

```wipple
show (Count "abc") -- 3

count-both :: left right -> Number where (Count left) (Count right)
count-both : a b -> Count a + Count b
```

Finally, instances may themselves have constraints!

```wipple
-- Implementation omitted
instance (Count (Pair first second)) where (Count first) (Count second)
```

## Modeling data

### Structures

To define a structure type, use the `type` keyword:

```wipple,playground
Sport : type {
    name :: String
    players :: Number
}
```

You can construct a structure value similarly:

```wipple,playground
basketball : Sport {
    name : "Basketball"
    players : 5
}
```

To get the values out of a structure, you can put a block on the left-hand side of the colon (`:`), listing the field(s)' names and the corresponding variable names.

```wipple,playground
Sport {name : sport-name} : basketball
show sport-name
```

```
Basketball
```

### Pattern matching

Wipple uses pattern matching to express control flow. For example, let's say we want to generate a report card:

```wipple,playground
Grade : type {
    A
    B
    C
    D
    F
}

report-card :: Grade -> String
report-card : grade -> when grade {
    A -> "top of the class"
    B -> "good work"
    C -> "need to study"
    D or F -> "didn't pass"
}

show (report-card A) -- top of the class
```

First, we define our patterns using `type`. Rather than providing fields, we list the _variants_, and Wipple will create an enumeration for us. Then, we use `when` to return a different value for each variant. You can use `or` to match multiple variants at once.

In fact, in Wipple, `if` is just a regular function that matches on `Boolean`. We can create our own easily:

```wipple,playground
My-Boolean : type {
    My-True
    My-False
}

my-if : bool then else -> when bool {
    My-True -> do then
    My-False -> do else
}

show (my-if My-True {123} {456}) -- 123
```

In addition to enumerations like these, you can store data alongside each pattern, allowing you to express values that are tied to a condition — in other words, the value is "wrapped" in a pattern, and you need to "unwrap" the value by checking for the condition using `when`. This may sound a bit confusing if you've used other languages without this feature, so let's look at an example:

```wipple
Maybe-Number : type {
    Some-Number Number
    No-Number
}
```

Here, we create a `Maybe-Number` value with two patterns. The first pattern contains a `Number`, and the second pattern contains nothing. Now, we can use pattern matching to "unwrap" the `Maybe-Number`:

```wipple
describe-maybe-number : maybe -> when maybe {
    Some-Number n -> "we have a number: _" n
    No-Number -> "we don't have a number"
}

show (describe-maybe-number (Some-Number 42))
show (describe-maybe-number No-Number)
```

Why is this useful? It means we can represent errors in our program! Let's go back to our report card example, and allow the user to specify a grade as input:

```wipple,playground
Maybe-Grade : type {
    Valid-Grade Grade
    Invalid-Grade
}

parse-grade :: String -> Maybe-Grade
parse-grade : string -> when string {
    "A" -> Valid-Grade A
    "B" -> Valid-Grade B
    "C" -> Valid-Grade C
    "D" -> Valid-Grade D
    "F" -> Valid-Grade F
    _ -> Invalid-Grade
}

repeat forever {
    grade : parse-grade (prompt "Enter your grade")

    when grade {
        Valid-Grade g -> show (report-card g)
        Invalid-Grade -> show "invalid grade"
    }
}
```

Wipple's type system will check for us that we handle the error — watch what happens if we pass our `Maybe-Grade` to `report-card` directly:

```wipple
grade : parse-grade (prompt "Enter your grade")
show (report-card grade) -- error
```

Wipple includes a built-in type for error handling, `Maybe`. Here is its definition:

```wipple
Maybe : value => type {
    Some value
    None
}
```

Similarly, the built-in `Read` trait is defined as:

```wipple
Read : output => trait (String -> Maybe output)
```

The example above can be rewritten to use `Maybe` and `Read`, and `prompt` will handle validation for us!

```wipple,playground
instance (Read Grade) : string -> when string {
    "A" -> Some A
    "B" -> Some B
    "C" -> Some C
    "D" -> Some D
    "F" -> Some F
    _ -> None
}

grade : prompt "Enter your grade"
show (report-card grade)
```

## Custom feedback

In Wipple, you're encouraged to provide custom feedback when your API is used incorrectly. Wipple supports defining custom error messages using the type system.

### Error instances

An _error instance_ is an instance with the `[error]` attribute and has no value. When a trait resolves to an error instances, Wipple will replace the default error message with the instance's comment:

```wipple
-- Can't count the contents of a block. Are you missing `do`?
[error] instance (Count {output})
```

You can combine `[error]` with `[default]` to customize the error displayed when no other instances match:

```wipple
-- Can't count the contents of [`value`].
[default] [error] instance (Count value)
```

The example above uses a link to reference the `value` parameter. Wipple will automatically replace the link with the first member of the group `value` belongs to:

```wipple
Uncountable : type

x : Uncountable
Count x -- Can't count the contents of `x`.
```

To list all members of the group, use `` [`value@related`] ``, and to display the type of the group, use `` [`value@type`] ``.

```wipple
-- Can't count the contents of [`value`] (a [`value@type`] value).
[default] [error] instance (Count value)

x : Uncountable
Count x -- Can't count the contents of `x` (a `Uncountable` value).
```

Finally, if you want to reference group members in an error message, but your instance works with concrete types, you can use an inline type annotation:

```wipple,playground
Count : value => trait (value -> Number)

-- Can't count the contents of [`block`]. Try adding `do` to count the
-- inner [`output`].
[error] instance (Count (block :: {output}))

number : {123}
Count number
```

### The `Mismatched` trait

Wipple also supports defining traditional "expected/found"-style error messages with the `Mismatched` trait. Whenever a group has more than one type, Wipple will attempt to resolve `Mismatched` instances for each pair of types. To display a custom error message, define an error instance for the types you are interested in.

```wipple,playground
Distance : type {m :: Number}

[unit] m :: Number -> Distance
m : value -> Distance {m : value}

[unit] cm :: Number -> Distance
cm : value -> (value / 100) m

-- Missing a unit for distance. Try adding `m` or `cm` after [`n`].
[error] instance (Mismatched (n :: Number) Distance)

forward :: Distance -> ()
forward : todo

forward 50
```
