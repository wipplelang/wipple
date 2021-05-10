# Patterns

Patterns are a way to do two things:

1. Ensure a value has a specific representation or behavior
2. Provide a new value that has the representation or behavior needed

The primary way you'll use patterns is with traits. Let's see how it works using `add`:

```wipple
add : a -> b -> a + b
```

We've always passed numbers into this function, but technically there's nothing stopping us from passing in anything we want! Let's try passing in two text values:

```wipple
add "hello " "world" -- helloworld
```

Looks like text can be added together as well! Can we add a text value and a number?

```wipple
add "we are number " 1 -- error
```

Nope, looks like we can't. Really, what we want to do is ensure that `add` only accepts values that can be added together. "Adding" sounds like a behavior, which means it's a good fit for a trait, and indeed Wipple provides an `Add` trait!

```wipple
add : a (is Add) -> b -> a + b
```

Closures allow you to provide a pattern after the parameter name to restrict the kinds of values you can provide. Notice that we only need to restrict `a`, because `a`'s implementation of `Add` will have its own restrictions (eg. only numbers can be added to other numbers). If you don't provide a pattern, the default is `_`, which is a pattern that accepts any value.

Let's try using patterns on our own trait!

```wipple
Person : trait Text
```

We were using `_` before, but now we're requiring values with the `Person` trait to contain a text value. Let's use our trait to build a `greet` function!

```wipple
greet : person (is Person) -> show (format "Hello, _!" person)
```

Now if we make a `Person` value, we can pass it to `greet`:

```wipple
bob : Person "Bob"
greet bob
```

And... error? How come? Well, we're providing a `Person` value to `greet`, but we probably want to be using the name (ie. the `Text` value) contained _within_ the `Person `value. This use case leads to patterns' other purpose — to provide a new value that does what we need.

Traits are patterns that:

1. Ensures a value has the trait
2. Provides the value contained within the trait

So if we just use the trait on its own as a pattern, the code will work!

```wipple
greet : name Person -> show (format "Hello, _!" name)

bob : Person "Bob"
greet bob -- Hello, Bob!
```

There seem to be inconsistencies here. Why do we still use `(is Add)` for `add`, and why don't we write `(is Text)` instead of just `Text` for our `Person` trait? Well, the `Add` trait requires a function; that is, `Add` is defined as `Add : trait Function`. This function is used to add to the right-hand side of `+`. If we wrote `a Add` instead of `a (is Add)`, then our `add` implementation would receive the actual function used to add instead of the `Add` value. Because functions have the `Function` trait and not the `Add` trait, our call to `+` would fail.

Here's a good rule to use:

- When you want to deal with the implementation of a trait, use the trait as a pattern directly
- When you want to accept a value with the trait so you can pass it to other functions, use `is`

`is` is just a function that changes a pattern to only perform purpose 1; that is, it makes the pattern only check that a value has a specific representation or behavior, but doesn't provide a new value in its place.

To address why we write `Person : trait Text` instead of `Person : trait (is Text)`: this is due to `Text` being a "primitive" trait. A text value doesn't _contain_ anything, it just _is_ text. `Number`, `Name` and `List` are other "primitive" traits.

> In technical terms, a primitive trait is a trait whose pattern is of itself: `T : trait (is T)`.

## Other patterns

Traits are one example of patterns, but here are some others provided by Wipple:

- Names, numbers and text can be used to exactly match those values
- Lists of patterns can be used to match lists of values matching those patterns
- The empty value (`_`) is a pattern that accepts any value and provides it unchanged

## Using patterns in other places

Closures aren't the only place you can use patterns. One common place is with the `match` function:

```wipple
-- FYI: 'list' creates a list by evaluating the provided values
fizz-buzz : x -> match (list ((x mod 3) (x mod 5))) {
  (0 0) : "fizzbuzz"
  (0 _) : "fizz"
  (_ 0) : "buzz"
  _ : x as Text
}
```

`match` accepts a block and changes the meaning of `:` to "match this pattern" instead of "assign to this variable". The provided value is passed to each pattern until it matches one. If the value doesn't match any patterns, an error is raised — to avoid this, you usually make the last pattern `_` unless you're sure that the other patterns cover all possible inputs.

The `as` operator applies a pattern to a value, raising an error if the value doesn't match the pattern. Since numbers can be converted to text, `as` will always work. If you aren't sure if the value will match or not, you can use `as?` instead, which returns a maybe value (explained later on).

`match` also lets you provide a name for each pattern that will be set to the matched value, just like in closures:

```wipple
increment-if-not-0 : x Number -> match x {
  0 : 0
  n _ : n + 1
}

add : x -> match x {
  text Text : "hello " + text
  number Number : 1 + number
  name Person : Person (name + " Jr.")
  _ : x
}
```

> **Note:** If you provide a name for a pattern, the right-hand side will be evaluated like a template, replacing all occurrences of the name with the matched value. `match` does not create its own scope.

Another place you can use patterns is in **relations**, which is the subject of the next section!
