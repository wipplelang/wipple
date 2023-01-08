# Specialization

Wipple doesn't support overloading like many other languages. Instead, you can use a trait to describe how functions operating on the trait should behave for a certain type. For example, you can implement the `count` function for every iterable type:

```wipple
count :: Collection where (Iterate Collection _) => Collection -> Natural
count : ... -- iterate over each item and increment a counter
```

And if you implement `Iterate` on, say, a `List`, you get `count` for free!

There's one problem with this approach, and that's performance. While `count`'s current implementation (iterating over each item and incrementing a counter) is indeed the most generic and flexible way to do it, it's very inefficient for types that already know the number of elements they contain (like `List`s). Luckily, Wipple has a solution to this problem, and it's called **specialization**!

## What is specialization?

Specialization allows you to declare a new constant for a more specific type and tell Wipple to use it instead of the generic implementation. This is done using the `[specialize]` attribute:

```wipple
Greet : A => trait (A -> Text)

greet :: A where (Greet A) => A -> Text
greet : x -> format "Hello, _!" (Greet x)

Foo : type
instance (Greet Foo) : just "Foo"

Bar : type
instance (Greet Bar) : just "Bar"

[specialize]
greet :: Bar -> Text
greet : just "Greetings!"

show (greet Foo) -- prints "Hello, Foo!"
show (greet Bar) -- prints "Greetings!"
```

When the program reaches `greet Bar`, it uses the specialized `greet` function for `Bar` instead of the generic one that works on both `Foo` and `Bar`. As a result, the program outputs `Greetings!` instead of `Hello, Bar!`.

## Rules and conventions

Because specialization is intended solely for performance, it's supposed to be invisible to the end user of your library. Therefore, there are some restrictions on specialized constants:

-   They must have the same name as the generic constant.
-   They must have the same type as the generic constant and satisfy all of its bounds.
-   You can't specialize a constant that is a specialization of another constant.

In addition, there are some conventions that Wipple can't check, but you should follow:

-   The specialized constant should have the same behavior as the generic constant; it should be a "drop-in replacement".
-   You should only specialize a constant you created, or your specialization should operate at least one type you created. This prevents conflicts between libraries.
