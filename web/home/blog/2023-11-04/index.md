---
layout: blog
title: Fixing type inference involving multiple bounds
date: 2023-11-04
---

Let's say we want to implement `Default` for a tuple, defined to be a tuple of the default value of each element. To accomplish this, we can define an instance `Default (A , B)` for any types `A` and `B`:

```wipple
A B => instance (Default (A , B)) : (Default , Default)
```

This will fail to compile...

```
error:
  ┌─ test.wpl:1:38
  │
1 │ A B => instance (Default (A , B)) : (Default , Default)
  │                                      ^^^^^^^ could not find instance `Default A` for any type `A`
  │
  = for more information, see https://wipple.dev/playground/?lesson=errors/missing-instance

error:
  ┌─ test.wpl:1:48
  │
1 │ A B => instance (Default (A , B)) : (Default , Default)
  │                                                ^^^^^^^ could not find instance `Default B` for any type `B`
  │
  = for more information, see https://wipple.dev/playground/?lesson=errors/missing-instance
```

...because whatever types `A` and `B` end up being don't necessarily have a `Default` implementation. For example, there is no default `Grade`:

```wipple
Grade : type { A B C D F }

-- What implementation would we use here???
(Default) :: (Grade , Grade)
```

To resolve this, we can use a `where` clause to add bounds to the instance, propagating the `Default` requirements to the caller:

```wipple
A B where (Default A) (Default B) => \
  instance (Default (A , B)) : (Default , Default)
```

Great — now within the instance, we can assume that `Default A` and `Default B` exist, and our code compiles!

However, before today, there was a bug in Wipple that caused the compiler to crash or even allow invalid code to compile. Let's say we want to infer the second element of the tuple:

```wipple
my-tuple : Default :: (Number , _) -- instance (Default Number) : 0
```

Previous versions of Wipple would accept this code, inferring the second element to be `Number` as well! Logically, this doesn't make sense — within the `Default (A , B)` instance, `A` and `B` have no relation to each other; their `Default` bounds are separate, so there's no reason the type of one should be able to determine the type of the other.

Even worse, the information about `B`'s type wasn't passed back to the instance, meaning the type of `B` was still unknown within the instance and no `Default` implementation was ever found. Due to the order in which Wipple performs type inference, this caused the compiler to crash after typechecking completed, or sometimes even accept code with mismatched types!

So why did this bug occur? When Wipple encounters a trait in expression position, it searches for an `instance` that's compatible in the current context. For example, the following code prints `X` because the instance `Show X` is chosen over `Show Y`:

```wipple
X : type
instance (Show X) : "X"

Y : type
instance (Show Y) : "Y"

value : X
show value -- the input to `show` is a value of type `X`
```

This works fine because we're at the top level. But inside the body of a generic constant or instance, we are dealing with abstract type parameters about which no information can be assumed except what is provided by bounds.

```wipple
show :: A where (Show A) => A -> ()
show : input -> {
  -- First, produce a `Text` value using `Show`...
  text : Show input

  -- Then, display it on the screen.
  intrinsic "display" text
}
```

A generic constant by itself doesn't ever appear in the final program — it only appears in _monomorphized form_, where the type parameter `A` is replaced with a concrete type like `Number` or `Text`. If we refer to `show` in the program, the compiler immediately creates a new copy of `show`'s body where all the type parameters are replaced with placeholders that can be substituted with any type. For example, if we have the following program (ignoring the bounds for a moment):

```wipple
show :: A => A -> ()
show : <body>

show 3.14
```

Then the equivalent monomorphized program looks like this:

```wipple
(<body> :: (_ -> ())) 3.14
```

And the placeholder is inferred to be `Number` due to `3.14`. You can see this effect more clearly if you assign a generic function to a variable, and then attempt to call the variable with inputs of different types:

```wipple
monomorphized-show : show
monomorphized-show 3.14
monomorphized-show "Hello, world!"
```

```
error:
  ┌─ test.wpl:3:20
  │
3 │ monomorphized-show "Hello, world!"
  │                    ^^^^^^^^^^^^^^^ expected `Number`, but found `Text`
  │
  = for more information, see https://wipple.dev/playground/?lesson=errors/mismatched-types
```

The next step is to resolve the bounds. Just like with the body, any type parameters in the bounds are also replaced with placeholders. Bounds are evaluated after inferring the concrete types of the type parameters (unless you mark the type parameter with `infer`), and once a bound is monomorphized, it is added to the list of available instances.

So let's go back to our original example and perform monomorphization (I'll denote the different placeholders with lowercase letters):

```wipple
-- We have:
A B where (Default A) (Default B) => \
  instance (Default (A , B)) : (Default , Default)

-- So when Wipple sees this:
Default :: (Number , _)

-- It generates this:
((Default :: a) , (Default :: b)) :: (Number , _)
```

After type inference, `a` is known to be `Number` and `b` is still unknown. And here lies the bug: bound instances have a higher priority than declared instances. This means that when searching the list of available instances, we check `instance (Default a)` and `instance (Default b)` before checking any instances defined on concrete types. This search is done in the same order as the bounds.

So, because `a` is `Number` and `Default a` is the first bound, there are no other high-priority instances to choose from yet, and we fall back to the declared instance `Default Number`. We then register the body of `Default Number` as the body of the `Default a` bound.

But when searching for a suitable instance `Default b`, we now have this high-priority `Default a` bound available! And since we know `a` is `Number`, we again choose the `Default Number` instance and infer `b` as `Number`.

Before this bug was fixed, you could actually see the order of bounds checking in the problem. This code compiled:

```wipple
A B where (Default A) (Default B) => \
  instance (Default (A , B)) : (Default , Default)

(Default) :: (Number , _)
```

But this code did not:

```wipple
A B where (Default A) (Default B) => \
  instance (Default (A , B)) : (Default , Default)

(Default) :: (_ , Number)
```

```
error:
  ┌─ test.wpl:4:2
  │
4 │ (Default) :: (_ , Number)
  │  ^^^^^^^
  │  │
  │  could not determine the type of this expression
  │  this has type `_ , Number`
  │
  = annotate the type with `::`: `:: {% raw %}{%type%}{% endraw %}`
  = for more information, see https://wipple.dev/playground/?lesson=errors/unknown-type
```

The fix for the bug is actually pretty simple — just wait to add the bounds to the list of available instances until after all bounds have been monomorphized. That way, type inference within bounds can only use the low-priority instances declared for concrete types. Now, Wipple correctly raises an error at compile time!

```
error:
  ┌─ test.wpl:4:2
  │
4 │ (Default) :: (Number , _)
  │  ^^^^^^^
  │  │
  │  could not determine the type of this expression
  │  this has type `Number , _`
  │
  = annotate the type with `::`: `:: {% raw %}{%type%}{% endraw %}`
  = for more information, see https://wipple.dev/playground/?lesson=errors/unknown-type
```

I hope this article helped you understand a bit more about how Wipple's type system works under the hood. As you can see, there are a lot of parts interacting with each other, and things can fail in subtle ways. I'm working on improving Wipple's automated test suite to catch issues like this — as of this writing, there are 75 tests!

If you're interested in learning more about Wipple's type system, try exploring [this lesson on type-level programming](https://wipple.dev/playground/?lesson=expert/type-level-programming) in the Wipple Playground!
