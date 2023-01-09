# Types and generics

A **type** is a way to identify what "kind" of value something is. For example, the expression `"hello"` has type `Text`, and `1 + 2` has type `Number`.

There are six main kinds of types in Wipple:

-   **Marker types** have a single value and contain no information.
-   **Structure types** represent a collection of values ("fields"), where each field has a name and stores a single value.
-   **Enumeration types** represent a fixed set of values ("variants"), where each variant has zero or more associated values.
-   **Tuple types** represent a fixed-size, heterogeneous collection of values.
-   **Function types** represent a function that accepts a value of one type and returns a value of another type.
-   The **bottom type** represents a computation that will never resolve to a value (eg. when the program crashes or enters an infinite loop).

## Type annotations

You can use the `::` operator to explicitly declare the type of a value. If Wipple determines that your type is incorrect, it will raise an error. For example, to explicitly declare that `42` has type `Number`:

```wipple
42 :: Number
```

> **Note:** If you want to give a variable `x` a type `T`, you can't write `x :: T` at the statement level, as this defines a [constant](03-files-and-constants.html). To get around this, wrap the type annotation in parentheses: `(x :: T)`.

## Catalog of types

### Markers

Marker types can be declared using the `type` template:

```wipple
Marker : type
```

To refer to the value the marker represents, just write the name of the marker type. For example, if `x : Marker`, then `x` has type `Marker`.

### Structures

Structure types can be declared using the `type` template followed by a block of type annotations:

```wipple
Structure : type {
    x :: Number
    y :: Text
}
```

To create a new structure, write the structure's name followed by a block of variable assignments:

```wipple
s : Structure {
    x : 42
    y : "hello"
}
```

### Enumerations

Enumeration types can be declared using the `type` template followed by a block of variants:

```wipple
Grade : type {
    A
    B
    C
    D
    F
}
```

You can also add associated values to each variant:

```wipple
Either : type {
    Left Number
    Right Text
}
```

To create a variant of the enumeration, write the enumeration's name, followed by the variant's name, followed by any associated values:

```wipple
g : Grade A
e : Either Left 42
```

If you want to refer to the variants directly without having to write the enumeration's name every time, you can use the `use` template:

```wipple
use Grade
g : A

use Either
e : Left 42
```

### Tuples

Tuples and tuple types can be declared using the `,` operator:

```wipple
(1 , "a" , True) :: (Number , Text , Boolean)
```

The empty tuple `()` is also valid. Usually, `()` is used for a function that accepts and/or returns no meaningful value:

```wipple
show 42 :: ()
```

### Function types

Functions and function types can be declared using the `->` operator:

```wipple
(x -> x) :: (Number -> Number)
```

In Wipple, functions may only accept one value. To accept another value, make the function return another function and move your computation into that new function:

```wipple
f : (x -> y -> x + y) :: (Number -> Number -> Number)
g : (f 1) :: (Number -> Number)
h : (g 2) :: Number
```

### The bottom type

The bottom type (`!`) has no values and cannot be constructed. Instead, you use type annotations to indicate that a computation has no result:

```wipple
x : (crash "oh no") :: !
```

Because `!` has no values, an expression of type `!` may be used in place of an expression of any type. For example:

```wipple
binary : x -> when x {
    "0" -> 0
    "1" -> 1
    _ -> crash "invalid binary digit"
}
```

Here, `crash` has type `!`, but the `when` expression has type `Number`. Normally, all branches must produce a value of the same type, but `crash` (and other constructs that never produce a value) is an exception.

## Generics

Wipple supports generics in the form of **type functions**, which accept one or more types and produce a new type as a result. For example, we can redefine `Either` from above to be more generic:

```wipple
Either : A B => type {
    Left A
    Right B
}
```

To use such a type function, you call it by its name, providing the specific types as input:

```wipple
Left 42 :: Either Number Text
```

Here, the annotation is required because `Left` only refers to `A`, meaning there's no way for Wipple to automatically determine `B`.

### Type placeholders

You can use `_` to represent a placeholder, the type of which Wipple should determine automatically. For example, we know the type of `A` in the above example to be `Number`, so we can make the type annotation more concise using a placeholder:

```wipple
Left 42 :: Either _ Text
```

In a type function, you can use `_` to create an implicit type parameter:

```wipple
left :: Left => Either Left _ -> Maybe Left
left : ...

right :: Right => Either _ Right -> Maybe Right
right : ...
```
