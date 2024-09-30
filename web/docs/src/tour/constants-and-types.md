# Constants and types

Wipple has a powerful type system that can catch bugs in your program. By default, it works invisibly — all the code we've written so far has been fully typechecked! — but sometimes it's helpful to provide type annotations. Type annotations serve as a form of documentation, describing the kinds of values your code works with and produces.

To add a type annotation to a variable, use a double colon (`::`):

```wipple
add :: Number Number -> Number
add : a b -> a + b

show (add 1 2)
```

```wipple-output
3
```

Adding a type annotation also changes how the variable is represented — rather than evaluating its value immediately, the variable is "lifted" out of the list of statements and is accessible anywhere as a constant. That means you don't have to worry about the order in which constants are defined.

```wipple
show (add 1 2)

-- Works, even though `add` is declared after the call to `show`
add :: Number Number -> Number
add : a b -> a + b
```

```wipple-output
3
```

Because of this order independence, constants are "lazy": they are evaluated whenever they are used, _not_ when they are declared. You can think of the constant's value as being wrapped in a block. In practice, this isn't a problem because most constants produce functions that need to be called anyway.

Constants can also be generic — see [Type functions and traits](./type-functions-and-traits.md) for more information.

Let's look at some of the types that can be used in a type annotation:

-   `Number` is the type of numbers.
-   `Text` is the type of text.
-   `Boolean` is the type of `True` and `False`.
-   `None` is the "unit type", and is returned by functions like `show` that do something but produce no meaningful value.
-   `{A}` is the type of a block evaluating to a value of type `A`. For example, `{1 + 1}` has type `{Number}`.
-   `A -> B` is the type of a function accepting a single input of type `A` and producing a value of type `B`. Likewise, `A B C -> D` is the type of a function accepting three inputs.

You can also make your own types! We'll discuss structure types in this chapter, and enumeration types in the next chapter.

To define a structure type, pass a block of fields to `type`:

```wipple
Sport : type {
    name :: Text
    players :: Number
}
```

Any block containing only variables is assumed to be a structure value:

```wipple
#Sport : type {
#    name :: Text
#    players :: Number
#}
basketball :: Sport
basketball : {
    name : "Basketball"
    players : 5
}
```

When you define the `Sport` type, Wipple also generates a function `Sport` that accepts the block and returns it as-is. This is useful because it allows Wipple to infer the type of the structure without needing a type annotation:

```wipple
#Sport : type {
#    name :: Text
#    players :: Number
#}
basketball : Sport {
    name : "Basketball"
    players : 5
}
```

To get the values out of a structure, you can put a block on the left-hand side of the colon (`:`), listing the field(s)' names and the corresponding variable names.

```wipple
#Sport : type {
#    name :: Text
#    players :: Number
#}
#
#basketball : Sport {
#    name : "Basketball"
#    players : 5
#}
{name : sport-name} : basketball
show sport-name
```

```wipple-output
Basketball
```

Finally, you might also see the double colon (`::`) used to annotate the type of an expression. For example, you can write:

```wipple
show (42 :: Number)
```

```wipple-output
42
```

Usually this is unnecessary, but in some cases, Wipple's type inference algorithm needs help. You'll see an example of type annotations being used for this purpose in [Type functions and traits](./type-functions-and-traits.md).
