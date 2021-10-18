# Traits

A trait is a variable whose value depends on its type, similar to Haskell's typeclasses. For example, we can define an `Equal` trait and give values for each type we want to compare as equal:

```wipple
Equal : for A -> trait A -> A -> Boolean


User : data {
    id :: User-ID
    name :: Text
    age :: Number
}

Equal : (u1 :: User) -> u2 -> u1 id = u2 id
```

Assigning to an existing trait does not declare a new variable, instead it gives the trait a value for the value's type. By convention, traits are capitalized.

To use a trait, add a bound indicating that the trait has a value for the provided type:

```wipple
equals? : for A (Equal :: A -> A -> Boolean) -> b -> a -> equal a b


alice : User ...
bob : User ...

alice | equals? bob
```

Another useful trait is `Default`, which provides a "default value" for a type. For example:

```wipple
Default : 0
Default : ""
Default : False
Default : None
...


-- identity :: for A (Default :: A) -> A -> A
identity : n -> n + Default

identity 42 -- 42 + 0 = 42
```
