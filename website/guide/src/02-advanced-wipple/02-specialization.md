# Specialization

Wipple doesn't support overloading like many other languages. Instead, you can use a trait to describe how functions operating on the trait should behave for a certain type. For example, you can implement the `count` function for every iterable type:

```wipple
count :: Collection where (Iterate Collection _) => Collection -> Natural
count : ... -- iterate over each item and increment a counter
```

And if you implement `Iterate` on, say, a `List`, you get `count` for free!

There's one problem with this approach, and that's performance. While `count`'s current implementation (iterating over each item and incrementing a counter) is indeed the most generic and flexible way to do it, it's very inefficient for types that already know the number of elements they contain. Let's say we have a type called `Repeat` that produces a value `count` times:

```wipple
Repeat : A => type {
    value :: A
    count :: Natural
}

A => instance (Iterate (Repeat A) A) : ...
```

Because `Repeat` implements `Iterate`, `count` will work just fine. But we can eliminate a bunch of unnecessary work because `Repeat` already knows its `count`! To accomplish this, we can use **specialization**.

## How does specialization work?

Specialization allows you to declare a new constant for a more specific type and tell Wipple to use it instead of the generic implementation. This is done using the `[specialize]` attribute:

```wipple
[specialize]
count :: Repeat _ -> Natural
count : { count } -> count
```

Now Wipple will use the specialized implementation of `count` whenever it's called with a `Repeat` value.

## Rules and conventions

Because specialization is intended solely for performance, it's supposed to be invisible to the end user of your library. Therefore, there are some restrictions on specialized constants:

-   They must have the same name as the generic constant.
-   They must have the same type as the generic constant and satisfy all of its bounds.
-   You can't specialize a constant that is a specialization of another constant.

In addition, there are some conventions that Wipple can't check, but you should follow:

-   The specialized constant should have the same behavior as the generic constant; it should be a "drop-in replacement".
-   You should only specialize a constant you created, or your specialization should operate at least one type you created. This prevents conflicts between libraries.
