# Generics

Wipple has a powerful type system that lets you express relationships between values. Often, you'll want to implement a function or instance that works for any input type — for example, implementing `Equal` for `Maybe Value` where `Equal Value` is implemented.

Wipple lets you express generics using **type functions**, which use the `=>` operator. The left-hand side of the type function introduces **type parameters** into scope, and the right-hand side is a type depending on these parameters. For example, we can define `Maybe` as follows:

```wipple
Maybe : Value => type {
    Some Value
    None
}
```

Type functions can also be used with traits, constants and instances:

```wipple
Show : A => trait (A -> Text)

unwrap :: A => Maybe A -> A
unwrap : ...

A where (Show A) => instance Show (Maybe A) : ...
```

That `where` clause in the above example allows you to introduce **bounds** on the type parameters — that is, the type, trait, constant or instance may only be used if there are instances matching the trait with the provided parameters.

You can provide as many parameters and bounds as you want:

```wipple
A B C where (T A) (U B) (V C) => ...
```

In a type declaration, you don't need to actually use the parameters anywhere in the type. This is useful for creating "type states" that represent data at the type level:

```wipple
Idle : type
Hovering : type

Drone : State => type

take-off :: Drone Idle -> Drone Hovering
take-off : just Drone

land :: Drone Hovering -> Drone Idle
land : just Drone


my-drone :: Drone Idle
my-drone : Drone

my-drone . take-off . land -- works!
my-drone . land -- cannot land because drone is already idle
```