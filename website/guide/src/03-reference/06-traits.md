# Traits

A trait is a variable whose value depends on its type, similar to Haskell's typeclasses. For example, we can define an `Equal` trait and give values for each type we want to compare as equal:

```wipple
Equal : A => trait A -> A -> Boolean


User : type {
    id :: User-ID
    name :: Text
    age :: Number
}

instance Equal : User { id : id1 } -> User { id : id2 } -> id1 = id2
```

To use a trait, refer to it by its name:

```wipple
equals? :: A where (Equal A) => A -> A -> Boolean
equals? : b -> a -> equal a b


alice : User { ... }
bob : User { ... }

alice . equals? bob
```

Another useful trait is `Default`, which provides a "default value" for a type. For example:

```wipple
instance Default : 0
instance Default : ""
instance Default : False
instance Default : None
...


add-identity :: A B where (Add A A B) (Default A) => A -> B
add-identity : n -> n + Default

add-identity 42 -- 42 + 0 = 42
```