# The type system

Wipple has a powerful _type system_ that can catch bugs in your program. Types in Wipple begin with a capital letter, such as `String`. All Wipple code is checked at compile time to ensure it has exactly one known data type. A lot of the time, Wipple can determine this type automatically — for example, Wipple knows that `"abc"` is a `String`, which means the `show` in `show "abc"` must be a function accepting a `String`.

## How it works

At compile time, Wipple simulates the flow of information through the program and sorts the code into _groups_. All members of each group must have the same data type. Groups are formed using a few rules:

- Variables are grouped with their values.

- All uses of a variable belong to the same group. In other words, accessing the same variable will always give you a value of the same data type.

- When you call a function, each argument is grouped with its corresponding parameter.

- When you run a block with `do` or call a function, the result is grouped with the output defined within the block or function.

Let's apply these rules to an example program:

```wipple
increment : x -> x + 1
a : increment 123
b : increment "abc"
```

We get two groups of interest:

1.  The group containing `x`, `123`, and `"abc"` (the inputs).

2.  The group containing `x + 1`, `a`, and `b` (the outputs).

After forming groups, Wipple then considers data types like `Number` and `String`. Unlike in other languages, data types apply to groups rather than individual expressions. In the first group:

- `x` doesn't contribute a type of its own.
- `123` contributes `Number` to the group.
- `"abc"` contributes `String` to the group.

That means Group 1 contains two data types, `Number` and `String`. Since there is more than one type in the group, Wipple produces an error, which appears on the first member of the group (the input `x`).

```
x -> x + 1
~ error: `x` is a `Number` or a `String`, but it can only be one of these.
```

Wipple's approach differs from languages where you might see an "expected `Number`, but found `String`" error message on `"abc"`. These languages create a placeholder for the input `x` that is replaced with `Number` when the function is first called with `123`. Wipple, on the other hand, doesn't assume any one group member is the "right" candidate — it waits to fill the placeholders with concrete data types until all group members are considered, and is thus able to provide more accurate error messages.

As a bonus, this group-based design means you can easily see code related to your cursor! Try pasting this program into the playground, click on `x`, and observe how `123` and `"abc"` are highlighted.

## Type annotations

In addition to letting Wipple infer types, you can use `::` to annotate any piece of code with its expected data type, and Wipple will verify that it's correct at compile time. These _type annotations_ serve as a form of documentation, describing the kinds of values your code works with and produces.

```wipple
sum : 1 + 1
show (sum :: Number) -- correct
show (sum :: String) -- error
```

As a special case, if you add a type annotation on its own line, Wipple defines a _constant_ value that is "lifted" out of the normal control flow and can be used anywhere. Constant values are evaluated when they are used, not at their definition like with variables.

```wipple
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

## Constraints

Recall our earlier program involving `increment` that had an error (because `x`, `123`, and `"abc"` were all grouped together):

```wipple
increment : x -> x + 1
a : increment 123
b : increment "abc"
```

Let's try to fix the error by making `increment` generic, and run our rules again!

```wipple
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

```wipple
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

## Traits and instances

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
