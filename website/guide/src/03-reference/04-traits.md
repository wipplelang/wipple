# Traits

**Traits** are a way to describe the "behavior" of one or more types. For example, the `Show` trait defines what the `show` function should display for a value of a given type. To define a trait, use the `trait` template in conjunction with a type function:

```wipple
Default : A => trait A
```

Here, `Default` is a trait that defines the "default value" for a type.

We can implement a trait for a specific type using an `instance` pattern:

```wipple
instance (Default Number) : 0
instance (Default Text) : ""
instance (Default Boolean) : False
-- ...and so on
```

To use a specfic implementation of a trait, refer to the trait by its name. The implementation used depends on the type of the surrounding expression:

```wipple
a : (Default :: Number) -- a : 0
b : (Default :: Text) -- b : ""
c : (Default :: Boolean) -- c : False
```

You can also store more complicated values inside the trait's implementation, eg. a function:

```wipple
Equal : A => trait (A -> A -> Boolean)

Person : type {
    name :: Text
    age :: Number
}

instance (Equal Person) -> p1 -> p2 ->
    name of p1 = name of p2
        and age of p1 = age of p2

alice : Person {
    name : "Alice"
    age : 25
}

bob : Person {
    name : "Bob"
    age : 30
}

Equal alice bob -- False
Equal bob bob -- True
```

And you can use generics in `instance` declarations by providing a type function:

```wipple
-- The "default" value of a list is the empty list
A => instance (Default (List A)) : (list)
```

## Bounded constants and instances

You can use a `where` clause in a type function to provide bounds on the type parameters. For example:

```wipple
show :: A where (Show A) => A -> ()
```

```wipple
A B C where (Default A) (Default B) (Default C) =>
    instance (Default (A , B , C)) : Default , Default , Default
```
