# Types

Wipple has a powerful static type system that works similarly to Haskell's. In short, every expression has a type, which can be expressed using the `::` operator (pronounced _"is a"_):

```wipple
42 :: Number -- "42 is a number"
"Hello" :: Text
'(1 2 3) :: List Number
```

You can also create your own types with `data` and `enum`:

```wipple
Name : data Text

Person : data {
    name :: Name
    age :: Number
}

Color : enum (Red Orange Yellow Green Blue Purple)

Maybe-Number : enum {
    Some Number
    None
}
```

By convention, variables representing types and variants of enums are capitalized.

The `Maybe-Number` type above can be made more general using type functions, which are prefixed with `for`:

```wipple
Maybe : for A -> enum {
    Some A
    None
}
```

This also works on other values, like regular functions:

```wipple
self :: for A -> A -> A : x -> x
```

By convention, generic types are named `A`, `B`, `C`, etc., but you should try to give a more specific name if applicable.
