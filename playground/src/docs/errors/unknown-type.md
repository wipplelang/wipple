---
error: could not determine what kind of value this code produces
---

Wipple displays this error when it couldn't figure out the type of a value on its own.

### What is a type?

Before running your code, Wipple needs to know what kind of data that code will produce when it runs — for example, a number or a piece of text. That way, Wipple can tell the computer how much space it needs in its memory to store the data. A **type** is a description of that data, and begins with a capital letter — for example, `Number` represents a numeric piece of data, and `Text` represents a list of characters. All pieces of code must be assigned a type in Wipple.

Most of the time, Wipple can figure out the type of your code automatically — if you have `pi : 3.14`, for example, then `pi` will be given the type `Number` automatically. But in some cases, there isn't enough information for Wipple to determine the type.

### How can I fix this error?

If you're getting this error on a function that you haven't used anywhere yet, try calling the function with an input. For example:

```wipple
-- Return the input unchanged
just : x -> x
```

Here, Wipple doesn't know what `x` is — it could be a number, or a piece of text, or anything else — so it doesn't know how much space the computer needs to store `x` in memory.

If you call `just` with an input, like `"Hello"`, then Wipple gives `x` the type `Text` automatically.

```wipple
just : x -> x -- `x` has type `Text` now
greeting : just "Hello"
show greeting -- Hello
```

Another way to fix the error is to use a double colon (`::`) to explicitly say what type `x` must be:

```wipple
just : (x :: Text) -> x -- `x` has type `Text`
```

Finally, for more advanced use cases, you can make `just` _generic_:

```wipple
just :: Value => Value -> Value
just : x -> x

-- Now `just` works with any input!
show (just "Hello") -- Hello
show (just (1 + 1)) -- 2
show ((just just) 5) -- 5
```

If you want to learn more about how that code works, check out [Constants and types](https://www.wipple.org/docs/tour/constants-and-types) as well as [Type functions and traits](https://www.wipple.org/docs/tour/type-functions-and-traits) in the Tour of Wipple.
