# Types

Wipple has a powerful static type system that works similarly to Haskell's. In short, every expression has a type, which can be expressed using the `::` operator (pronounced _"is a"_):

```wipple
42 :: Number -- "42 is a number"
"Hello" :: Text
'(1 2 3) :: List Number
x -> x :: Number -> Number
```

You can also create your own types with `type`:

```wipple
Name : type Text

Person : type {
    name :: Name
    age :: Number
}

Color : type {
    Red
    Orange
    Yellow
    Green
    Blue
    Purple
}

Maybe-Number : type {
    Some Number
    None
}
```

By convention, variables representing types and variants of enums are capitalized.

The `Maybe-Number` type above can be made more general using type functions, which use a `=>` arrow:

```wipple
Maybe : A => type {
    Some A
    None
}
```

This also works on other values, like regular functions:

```wipple
it :: A => A -> A
it : x -> x
```

By convention, generic types are named `A`, `B`, `C`, etc., but you should try to give a more specific name if applicable.

Finally, you can use an underscore (`_`) to represent a placeholder type:

```wipple
Identity : A => -> type (A -> A)
(x :: Number) -> x :: Identity _ -- inferred as Identity Number
```
