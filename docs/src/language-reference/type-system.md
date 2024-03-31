# Type system

In Wipple, every expression must have a type known at compile-time. For most code, though, Wipple can infer the type.

## Type annotations

To annotate the type of an expression explicitly, use the `::` operator:

```wipple
show ("Hello, world!" :: Text)
```

A type annotation at the top level is instead considered to be a constant definition, where the constant's body is defined on the next line:

```wipple
-- Create a new constant named `pi` with type `Number`
pi :: Number
pi : 3.14
```

If you actually want a type annotation rather than a constant definition, wrap the expression in parentheses:

```wipple
-- Assert that the existing constant `pi` has type `Number`
(pi :: Number)
```

## Builtin types

### Literals

Number literals have the type defined by the `number` language item. In the standard library, this is `Number`.

Text literals have the type defined by the `text` language item. In the standard library, this is `Text`.

### Functions

Function types are written `A B C -> D`, where `A`, `B`, and `C` are the inputs and `D` is the output.

### Tuples

Tuple types are written `A ; B ; C`. Each element type may be different.

A tuple with a single element is written with a trailing semicolon (`A ;`). A single-element tuple is not equal to the element type on its own (ie. `A ;` is not equivalent to `A`).

A tuple with no elements is written `()`.

### Blocks

Block types are written `{A}`, where `A` is the type of the last statement in the block.

The empty block produces a value of type `()`, and so it has type `{()}`. As a shorthand, `{()}` is syntactically equivalent to `{}`.

### Intrinsics

The keyword `intrinsic` may be used in type position to refer to a runtime-provided type. All `intrinsic` types are equivalent to each other, but are not equivalent to any other type.

It's not possible to create a value of type `intrinsic` in Wipple. The runtime always uses wrapper types like `Number`, `Text`, and `List`, instead of returning a value of type `intrinsic`.

### Type-level text

Type-level text is written the same way as regular text, except the inputs are types. Type-level text is currently only used for producing custom error messages at compile time — it's not possible to create a value with this type at runtime.

## User-defined types

Wipple has four kinds of user-defined types: marker types, wrapper types, structure types, and enumeration types.

### Marker types

Marker types contain no information and have a single value. They are defined with the `type` keyword:

```wipple
Marker : type
instance (Describe Marker) : _ -> "marker"

show Marker -- marker
```

### Wrapper types

Wrapper types contain a single value of the wrapped type, but you must explicitly convert between the wrapper and the wrapped value. They are defined with the `type` keyword, followed by the type to wrap:

```wipple
User-ID : type Text
instance (Equal User-ID) : (User-ID a) (User-ID b) -> a = b

id : User-ID "abc"
id = "abc" -- error: expected `User-ID`, but found `Text`
```

### Structure types

Structure types contain one or more fields. They are defined with the `type` keyword, followed by a block containing fields:

```wipple
Sport : type {
    name :: Text
    players :: Number
}

instance (Describe Sport) : {
    name : name
    players : players
} -> "_ has _ players per team" name players

basketball : Sport {
    name : "Basketball"
    players : 5
}

show basketball -- Basketball has 5 players per team
```

### Enumeration types

Enumeration types contain one or more variants. An enumeration value may only be one variant at a time. Enumeration types are defined with the `type` keyword, followed by a block containing variants:

```wipple
Primary-Color : type {
    Red
    Green
    Blue
}

instance (Describe Primary-Color) : color -> when color {
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
}

favorite-color : Blue
show favorite-color -- blue
```

Variants may also store values:

```wipple
JSON : type {
    Null-Value
    String-Value Text
    Number-Value Number
    Array-Value (List JSON)
    Object-Value (Dictionary Text JSON)
}
```

## Generics

Types can accept type parameters to make them generic:

```wipple
Linked-List : Value => type {
    Nil
    Cons Value (Linked-List Value)
}
```

You can make constants generic with the same syntax:

```wipple
join :: Value => (Linked-List Value) (Linked-List Value) -> Linked-List Value
join : ...
```

## Traits and instances

Traits allow you to define functionality across a range of types. For example, the standard library's `Describe` trait is used to convert values into `Text`. It's used by `show` to display things on the screen.

### Defining a trait

You can define a trait using the `trait` keyword, followed by the type of the value the trait represents:

```wipple
Describe : Value => trait (Value -> Text)
```

Traits may have multiple type parameters:

```wipple
Add : Left Right Sum => trait (Left Right -> Sum)
```

The value doesn't have to be a function; for example, the `Empty` trait defines the "empty value" of a type, like `0` for `Number` and `""` for `Text`:

```wipple
Empty : Value => trait Value
```

### Implementing a trait

You can define an instance for a trait using the `instance` keyword on the left side of the `:` assignment operator:

```wipple
Person : type {name :: Text}

instance (Describe Person) : {name : name} -> "Hi, I'm _" name
```

A trait `T` is said to be "implemented" for a type(s) if there is an `instance (T ...)` corresponding to those type(s).

Note that there is no "primary type" that the trait is implemented _on_ — an instance just represents a set of types corresponding to a trait implementation. In the following example, it's more correct to say that `Add` is implemented for the three `Numbers` rather than _on_ the `Left` or `Right` one.

```wipple
Add : Left Right Sum => trait (Left Right -> Sum)
instance (Add Number Number Number) : ...
```

You can only define one instance for some set of types at a time:

```wipple
instance (Describe Person) : ...
instance (Describe Person) : ... -- error
```

Instances

### Using a trait

To use a trait, refer to it by name — Wipple will infer the types of the trait's type parameters based on the surrounding context and select the instance that matches:

```wipple
Foo : type
Bar : type

Trait : A => trait A
instance (Trait Foo) : Foo -- (1)
instance (Trait Bar) : Bar -- (2)

(Trait :: Foo) -- selects (1)
(Trait :: Bar) -- selects (2)
```

This works for functions and other complex types, too:

```wipple
-- infers `Left` and `Right` as `Number`, so `instance (Add Number Number Number)`
-- is selected and the entire expression has type `Number` (aka. `Sum`)
Add 1 2
```

### Inferred type parameters

When you mark a type parameter with `infer` in a trait, that type parameter is not considered when checking for overlapping instances. This constraint aids in type inference and can produce better error messages. For example, `Sum` in `Add` is marked `infer`:

```wipple
Add : Left Right (infer Sum) => trait (Left Right -> Sum)
```

As a result, writing `(Add 1 2 :: Text)` produces `` expected `Number`, but found `Text` ``, rather than `` could not find instance `(Add Number Number Text)` ``.

## Bounds

Bounds allow you to make a generic constant or instance available conditionally. For example, `show` requires that its input implement `Describe`:

```wipple
show :: Value where (Describe Value) => Value -> ()
show : ...
```

Similarly, `Maybe` only implements `Equal` if its `Value` does:

```wipple
Value where (Equal Value) => instance (Equal (Maybe Value)) : ...
```

Within a constant's or instance's body, the bounds are implied, so you can use instances that refer to the bounded type parameters. But when using the constant or instance from the outside, it's up to the caller to ensure the bounds are satisfied.

```wipple
Value where (Equal Value) => instance (Equal (Maybe Value)) : a b -> when (a ; b) {
    (Some a ; Some b) -> Equal a b -- resolves to `instance (Equal Value)`
    ...
}
```
