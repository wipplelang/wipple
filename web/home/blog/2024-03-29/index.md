---
layout: blog
title: Custom error messages in Wipple
date: 2024-03-29
---

Previously, Wipple used attributes for custom error messages and other diagnostics. The new compiler doesn't support attributes, though, so now there's a new way that uses Wipple's powerful type system! It looks like this:

```wipple
-- Produce a piece of `Text` describing the value.
Describe : (Value : Text) => trait (Value -> Text)

Value where (Error ("cannot describe a _ value" Value)) =>
    default instance (Describe Value) : ...

instance (Describe Text) : ...

Describe "Hello, world!" -- ✅
Describe (1 , 2 , 3) -- ❌ cannot describe a `List Number` value
```

Here's another example:

```wipple
Carnivore : type
Herbivore : type

Meat : type
Plants : type

Eats? : Animal Food => trait ()

Animal Food where (Error ("_s do not eat _" Animal Food)) =>
    default instance (Eats? Animal Food) : ()

instance (Eats? Carnivore Meat) : ()
instance (Eats? Herbivore Plants) : ()

test :: Animal Food where (Eats? Animal Food) => Animal Food -> ()
test : ...

test Carnivore Meat -- ✅
test Herbivore Plants -- ✅
test Carnivore Plants -- ❌ `Carnivore`s do not eat `Plants`
test Herbivore Meat -- ❌ `Herbivore`s do not eat `Meat`
```

You can even use this system to correct misspellings:

```wipple
print :: (Value : Text) where (Error "use `show` to display on the screen") => Value -> ()
print : ...

print "Hello, world!" -- ❌ use `show` to display on the screen
```

Custom error messages are available for testing on the new Wipple Playground — try it out at [preview.wipple.dev](https://preview.wipple.dev)!

### How does it work?

Custom error messages rely on three new features: `default` instances, type-level `Text`, and the `Error` trait. Let's look at how they work!

First, `default` is a new keyword that can be applied to an `instance` to reduce its priority. Wipple will first check all non-default instances, and if none apply, it will use the default instance if available:

```wipple
Do-Something : A => trait (A -> ())

A => default instance (Do-Something A) : _ -> show "using default instance"
instance (Do-Something Text) : text -> show ("using specific instance: _" text)

Do-Something 42 -- using default instance
Do-Something "Hello, world!" -- using specific instance: Hello, world!
```

Default instances have two main use cases — first, it allows for specialization, meaning you can "override" the default behavior of a trait with an implementation specific to your type. For example, the `count` function currently takes a `Sequence`, but we could instead have it depend on a `Count` trait. This `Count` trait would have a default instance that works for all sequences, as well as a specialized implementation for `Range`s, `List`s, and other collections that already know their size. The other use case, of course, is custom error messages! If none of the other instances match, we can produce a custom error message when Wipple tries to use the default instance.

To make custom error messages work, Wipple now supports text at the type level. Normally, the compiler will display types to the user in code format (ie. `Text` rather than "Text"). Type-level text is always rendered as-is instead. You can even do formatting with `_` placeholders!

And finally, the `Error` trait looks like this:

```wipple
Error : Message => trait Message
Message => instance (Error Message) : unreachable
```

The Wipple compiler knows about `Error`, and whenever it appears in the `where` clause of a function or constant, Wipple will produce an error message. That `instance (Error Message)` on the second line is needed to prevent additional errors from appearing — we want `Error` _itself_ to be implemented for all types so that the compiler always succeeds in producing a message.

### Create your own messages

Putting all of this together, here are the steps to creating your own error messages:

-   If you want to produce a message when a trait isn't implemented, add a default instance:

    ```wipple
    Value where (Error ("_ has no empty value" Value)) =>
        default instance (Empty Value) : ...
    ```

-   If you want to produce a message when you encounter a specific type, add a regular instance:

    ```wipple
    Right Sum where (Error ("cannot add _ to `()`; are you missing parentheses?" Right)) =>
        instance (Add () Right Sum) : ...
    ```

-   If you want to produce an error for a deprecated function or a common misspelling of a function, use `Error` directly:

    ```wipple
    turn :: A where (Error "use `left` or `right` to turn") => Angle -> ()
    turn : ...
    ```
