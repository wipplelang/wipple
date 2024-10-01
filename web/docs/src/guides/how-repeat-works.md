# How `repeat` works

> Before you read this article, you might want to read about [type functions and traits](../tour/type-functions-and-traits.html).

The definition of `repeat` may seem really strange:

```wipple
repeat :: State Body Result where (Repeat-Predicate State Body Result) => State {Body} -> Result
```

`repeat` is written this way to allow different methods for controlling the loop. `repeat` doesn't just accept `n times` as its first input, it also accepts `while {x}`, `forever`, `with-control-flow`, and anything of your own creation! Let's build our own implementation of `repeat` to learn how it works.

## Start with the types

Before we write any executable code, let's think about how having a user-specified method for controlling the loop — a **predicate** — influences what types we need. For simplicity, our initial version of `repeat` will always return `None` rather than a generic `Result`. Let's start by thinking about the relationship the predicate establishes between its inputs and outputs.

We have two components in our relationship:

-   Some value that tells `repeat` whether to run its body again or stop.
-   Some way for the user to control the conditions for this value.

This maps to two types. The second, `State`, is initially given by the user, and the job of the predicate is to update this state over time.

The first component, the one that tells `repeat` whether to run again or stop, is actually one we define. We could use `Boolean`, but then it's hard to tell whether `True` means "continue" and `False` means "stop", or vice versa. Let's make our own type specifically for `repeat`!

```wipple
Control-Flow : type {
    Continue
    Stop
}
```

This won't be enough to make `repeat` actually repeat, but it's close enough for now. Let's define our relationship in the simplest way possible, and we'll extend it with more type parameters later!

```wipple
-- A predicate in a `repeat` takes a state and tells `repeat` whether to
-- `Continue` or `Stop`.
Repeat-Predicate : State => trait (State -> Control-Flow)
```

> A quick refresher on why we have this at all — we don't want users of `repeat` to have to worry about how to get a `Control-Flow` from a `State`, we just want them to provide a state and let `repeat` figure out what to do with it. Traits in Wipple let you make split the implementation of `repeat` apart so it works on different types. Within the implementation of `repeat`, we'll call the function defined by `Repeat-Predicate` on the state provided by the user.

Do you see the problem? The state never changes! Once `repeat` calls `Repeat-Predicate` with the _initial_ state (the one the user provides, like `4 times`), there's no way to get a _new_ state (like `3 times`, `2 times`, `1 times`, and finally `0 times`).

There are multiple ways we could do this — one is by attaching a `State` to the return value of the function:

```wipple
Repeat-Predicate : State => trait (State -> (Control-Flow ; State))
```

But this is actually more strict than we'd like. What if `Control-Flow` is `Stop`? There may no longer be a valid state to return, since we've exhausted the loop! Let's move `State` to the `Continue` variant of `Control-Flow`.

```wipple
Control-Flow : State => type {
    Continue State
    Stop
}
```

> Note that this version of `Control-Flow` is basically `Maybe`, just with a more specific meaning. `Maybe` is kind of like `Boolean` in this scenario — it works, but it's not as clear.

Now, the predicate is only required to produce a new `State` when it actually wants the loop to continue.

OK, let's update our trait to use our new version of `Control-Flow`!

```wipple
-- A predicate in a `repeat` takes a state and tells `repeat` whether to
-- `Continue` with the next state or `Stop`.
Repeat-Predicate : State => trait (State -> Control-Flow State)

repeat :: State where (Repeat-Predicate State) => State {None} -> None
```

## Writing the implementation

Now that we have types representing the possible interactions in our code, we can put them all together and implement `repeat`. We'll write expressions that have the appropriate type at every step, working our way inward.

Here is the type of `repeat` from above, ignoring the bounds:

```wipple
repeat :: State {None} -> None
```

So we need a function that takes a `State` and a `{None}`:

```wipple
repeat : state body -> ...
```

On the right-hand side, we'll call `Repeat-Predicate` on our `state` to get a `Control-Flow`:

```wipple
repeat : state body -> when (Repeat-Predicate state) {
    Continue next -> ...
    Stop -> ...
}
```

The `Stop` arm is simple — just produce a unit value:

```wipple
    Stop -> ()
```

On the `Continue` arm, we execute `body`, and then call `repeat` again with our next state:

```wipple
    Continue next -> do {
        do body
        repeat next body
    }
```

> You might be thinking, "wait, we aren't actually looping — that's just recursion!" Wipple will convert it to a traditional loop under the hood using something called [tail call optimization](https://stackoverflow.com/questions/310974/what-is-tail-call-optimization). This is really how `repeat` is implemented in Wipple!

Here's the full code:

```wipple
repeat :: State where (Repeat-Predicate State) => State {None} -> None
repeat : state body -> when (Repeat-Predicate state) {
    Continue next -> do {
        do body
        repeat next body
    }
    Stop -> ()
}
```

## Adding a predicate

Let's now create a predicate we can use with our `repeat` function. `n times` sounds fun!

```wipple
-- `Times` wraps `Number`
Times : type Number

-- Usage: `n times`
times :: Number -> Times
times : n -> Times n

-- Our predicate stops the loop when `n` is `0`
instance (Repeat-Predicate Times) : (Times n) ->
    if (n > 0) {Continue ((n - 1) times)} {Stop}
```

And now we can try out our implementation!

```wipple
repeat (4 times) {
    show "Hello, world!"
}
```

```wipple-output
Hello, world!
Hello, world!
Hello, world!
Hello, world!
```

> <p>
> <details>
> <summary><strong>Full code</strong></summary>
>
> ```wipple
> -- All the definitions are prefixed with "my" here, so you can easily run this
> -- in the Wipple Playground.
>
> My-Control-Flow : State => type {
>     My-Continue State
>     My-Stop
> }
>
> My-Repeat-Predicate : State => trait (State -> My-Control-Flow State)
>
> my-repeat :: State where (My-Repeat-Predicate State) => State {None} -> None
> my-repeat : state body -> when (My-Repeat-Predicate state) {
>     My-Continue next -> do {
>         do body
>         my-repeat next body
>     }
>     My-Stop -> ()
> }
>
> My-Times : type Number
>
> my-times :: Number -> My-Times
> my-times : n -> My-Times n
>
> instance (My-Repeat-Predicate My-Times) : (My-Times n) ->
>     if (n > 0) {My-Continue ((n - 1) my-times)} {My-Stop}
>
> my-repeat (4 my-times) {
>     show "Hello, world!"
> }
> ```
>
> </details>
> </p>

## Supporting more predicates

Let's try to implement the `with-control-flow` predicate, which allows the user to provide a `Control-Flow` themselves in the body, and is more complicated in two ways:

-   Whether the loop continues or not depends on the result of the loop's body.
-   When the loop exits, it can return something other than `None`.

We'll need to change our definitions of `Control-Flow` and `Repeat-Predicate` to accommodate these requirements.

```wipple
Control-Flow : Next Result => type {
    Continue Next
    Stop Result -- now `Stop` contains a value, too
}

Repeat-Predicate : State Body Result =>
    trait (State -> Control-Flow (Body -> State) Result)
```

Notice that the `Continue` variant now accepts a **function** to convert the result of the body into the next state. This function is provided by the repeat predicate, and called by the implementation of `repeat` after evaluating the body. Now we can break our task into three steps from the perspective of `repeat`:

1. Call `Repeat-Predicate` on the current state.
2. If the predicate returns `Continue next` (where `next` is the function, ie. `next :: Body -> State`), evaluate the body and call `next` with the result to get the new state.
3. Call `Repeat-Predicate` with the new state, and so on.

Of course, if the predicate returns `Stop result` instead, we just return `result` rather than continuing. In fact, we have no choice but to stop, because we don't have another state to pass back to `Repeat-Predicate`!

```wipple
repeat :: State Body Result where (Repeat-Predicate State Body Result) => State {Body} -> Result
repeat : state body ->
    -- (1)
    when (Repeat-Predicate state) {
        Continue next -> do {
            -- (2)
            new-state : next (do body)

            -- (3)
            repeat new-state body
        }

        Stop result -> result
    }
```

This is the same as the built-in definition of `repeat`!

Now we can implement `with-control-flow`, which starts in the `Continue` state and updates the stored `Control-Flow` value based on the result of the body:

```wipple
-- Store the previous `Control-Flow` returned by the body
With-Control-Flow : Result => type {
    current :: Control-Flow None Result
}

-- Start in the `Continue` state so the loop runs at least once
with-control-flow :: Result => With-Control-Flow Result
with-control-flow : {current : Continue ()}

-- The loop body must return a `Control-Flow None Result`
Result => instance (Repeat-Predicate (With-Control-Flow Result) (Control-Flow None Result) Result) :
    {current : current} -> when current {
        -- If we stored a `Continue`, the new state is the result of the body,
        -- which will influence whether the loop is run again the next time
        -- `Repeat-Predicate` is called
        Continue () -> Continue (new -> {current : new})

        -- If we stored a `Stop`, produce the stored result. The loop won't run
        -- again, but if it did, this would keep producing `result` because we
        -- never update the state again
        Stop result -> Stop result
    }
```

Let's try it!

```wipple
-- Loop a random number of times and eventually print "done"

result : repeat with-control-flow {
    if (random ()) {Continue ()} {Stop "done"}
}

show result
```

```wipple-output
done
```

> As an exercise, try updating the `Repeat-Predicate` instance for `Times`.
>
> <p>
> <details>
> <summary><strong>Show answer</strong></summary>
>
> `Body` and `Result` are both `None`, just like before. Wrap `(n - 1) times` in a function:
>
> ```wipple
> instance (Repeat-Predicate Times None None) : (Times n) ->
>     if (n > 0) {Continue (() -> (n - 1) times)} {Stop ()}
> ```
>
> </details>
> </p>

## Conclusion

Now that you've implemented `repeat` yourself, hopefully you have a better idea of why it has the type signature it does — `repeat` is a very flexible construct that abstracts over the looping behavior. Adding your own predicates is a good way to practice your understanding of traits in Wipple! If you want to learn more, check out [`repeat.wipple` in the Wipple standard library on GitHub](https://github.com/wipplelang/wipple/blob/main/library/base/repeat.wipple).
