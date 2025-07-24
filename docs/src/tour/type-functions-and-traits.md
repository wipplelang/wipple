# Type functions and traits

In the same way functions create new values from any given input, _type functions_ create new _types_ from any given input type — in other words, types can be "generic". That means we can create a `Maybe` that works for any value!

To make a type function, you use the "fat arrow" (`=>`), where the input types go on the left and the output type goes on the right:

```wipple
My-Maybe : Value => type {
    My-Some Value
    My-None
}

describe-maybe-number :: (My-Maybe Number) -> Text
describe-maybe-number : maybe -> when maybe {
    My-Some number -> "we have a number: _" number
    My-None -> "we don't have a number"
}

describe-maybe-text :: (My-Maybe Text) -> Text
describe-maybe-text : maybe -> when maybe {
    My-Some text -> "we have text: _" text
    My-None -> "we don't have text"
}

show (describe-maybe-number (My-Some 42))
show (describe-maybe-text (My-Some "Hello, world!"))
```

```wipple-output
we have a number: 42
we have text: Hello, world!
```

The arrow can also be used to make constants generic, so we only need one `describe-maybe`!

```wipple
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
describe-maybe :: Value => (My-Maybe Value) -> Text
describe-maybe : maybe -> when maybe {
    My-Some value -> "we have a value: _" value
    My-None -> "we don't have a value"
}
```

```wipple-output
example:7:22: error: cannot describe a `Value` value
```

Hmm, we run into some trouble here. The problem is that in Wipple, not all values can be converted into text! For example, let's define a `Sport` type:

```wipple
Sport : type {
    name :: Text
    players :: Number
}

basketball : Sport {
    name : "Basketball"
    players : 5
}
```

How should we display a `My-Some basketball`? Should we show the `name` first and then the number of `players`, or the other way around? What if we want to display an emoji instead? Wipple doesn't assume any particular format in which to display your custom types. What we need to do is tell Wipple how to convert `Sport` into text.

If you've used other languages, you might be familiar with the concept of an "interface"; for example, in Java, the [`Comparable`](https://docs.oracle.com/javase/8/docs/api/java/lang/Comparable.html) interface defines a `compareTo` function so you can use things like `Arrays.sort`.

Wipple has something similar called _traits_ — a trait is a container for a value that changes depending on its type. We can define a trait like so:

```wipple
My-Describe : Value => trait (Value -> Text)
```

`My-Describe` accepts a `Value` type, and produces a trait that stores a function to convert the `Value` into `Text`. Now, we can use `where` to say that `describe-maybe` is only available when `My-Describe` is implemented for `Value`:

```wipple
#My-Describe : Value => trait (Value -> Text)
#
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
describe-maybe :: Value where (My-Describe Value) => (My-Maybe Value) -> Text
describe-maybe : maybe -> when maybe {
    My-Some value -> "we have a value: _" (My-Describe value)
    My-None -> "we don't have a value"
}
```

Because `My-Describe` contains a function, we can call it, passing in our `value`. Now we have a `Text` value we can use to fill in the placeholder!

Under the hood, `_` placeholders wrap the provided value in a call to the built-in `Describe` trait automatically. `Describe` is defined in the same way as our `My-Describe` trait, so from now on, we'll use `Describe` instead:

```wipple
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
describe-maybe :: Value where (Describe Value) => (My-Maybe Value) -> Text
describe-maybe : maybe -> when maybe {
    My-Some value -> "we have a value: _" value -- equivalent to `(Describe value)`
    My-None -> "we don't have a value"
}
```

Now, if we provide a piece of text or a number, our code works!

```wipple
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
#describe-maybe :: Value where (Describe Value) => (My-Maybe Value) -> Text
#describe-maybe : maybe -> when maybe {
#    My-Some value -> "we have a value: _" value
#    My-None -> "we don't have a value"
#}
show (describe-maybe (My-Some 42))
show (describe-maybe (My-Some "Hello, world!"))
```

```wipple-output
we have a value: 42
we have a value: Hello, world!
```

> **Note:** We run into trouble if we provide a `My-None`. That's because Wipple can't infer for us what `Value` is supposed to be. We can fix this with a type annotation:
>
> ```wipple
> #My-Maybe : Value => type {
> #    My-Some Value
> #    My-None
> #}
> #describe-maybe :: Value where (Describe Value) => (My-Maybe Value) -> Text
> #describe-maybe : maybe -> when maybe {
> #    My-Some value -> "we have a value: _" value
> #    My-None -> "we don't have a value"
> #}
> show (describe-maybe (My-None :: My-Maybe Number))
> ```
>
> ```wipple-output
> we don't have a value
> ```
>
> It's rare that you'll have to do this, though.

So what about our `Sport` type from earlier? We can implement `Describe` for `Sport` using `instance`:

```wipple
#Sport : type {
#    name :: Text
#    players :: Number
#}
#
#basketball : Sport {
#    name : "Basketball"
#    players : 5
#}
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
#describe-maybe :: Value where (Describe Value) => (My-Maybe Value) -> Text
#describe-maybe : maybe -> when maybe {
#    My-Some value -> "we have a value: _" value
#    My-None -> "we don't have a value"
#}
instance (Describe Sport) : sport -> do {
    {
        name : name
        players : players
    } : sport

    "_ is played with _ people on each team" name players
}

show (describe-maybe (My-Some basketball))
```

```wipple-output
we have a value: Basketball is played with 5 people on each team
```

`instance` can be used in place of a variable name — whenever Wipple sees an assignment to `instance`, it registers the right-hand side with the provided trait. Then, when we use `describe-maybe`, `Value` becomes `Sport`, and Wipple looks up the corresponding instance and makes it available inside `describe-maybe`.

You can also use `where` to make _instances_ conditionally available — let's replace `describe-maybe` with a custom implementation of `Describe` for our `My-Maybe` type!

```wipple
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
Value where (Describe Value) => instance (Describe (My-Maybe Value)) : maybe -> when maybe {
    My-Some value -> "we have a value: _" value
    My-None -> "we don't have a value"
}
```

`show` actually uses `Describe`, so now we can pass both `Sport` and `My-Maybe` values to `show`, and everything just works!

```wipple
#My-Maybe : Value => type {
#    My-Some Value
#    My-None
#}
#
#Value where (Describe Value) => instance (Describe (My-Maybe Value)) : maybe -> when maybe {
#    My-Some value -> "we have a value: _" value
#    My-None -> "we don't have a value"
#}
#Sport : type {
#    name :: Text
#    players :: Number
#}
#
#instance (Describe Sport) : {
#    name : name
#    players : players
#} -> "_ is played with _ people on each team" name players
#
#basketball : Sport {
#    name : "Basketball"
#    players : 5
#}
show basketball
show (My-Some "Hello, world!")
show (My-Some basketball)
```

```wipple-output
Basketball is played with 5 people on each team
we have a value: Hello, world!
we have a value: Basketball is played with 5 people on each team
```

Now that you know how type functions and traits work, you don't need to define your own `My-Maybe` type. Just use the built-in `Maybe` type, along with `Some` and `None` — it's implemented in exactly the same way you saw here!

Let's put everything together and refactor our report card program to use traits. We'll take advantage of the `Read` trait, which contains a function accepting `Text` and producing a `Maybe Value`. `prompt` uses `Read` to validate the user's input for us, so we don't need to use `repeat` either!

```wipple
Grade : type {
    A
    B
    C
    D
    F
}

report-card :: Grade -> Text
report-card : grade -> when grade {
    A -> "top of the class"
    B -> "good work"
    C -> "need to study"
    D or F -> "didn't pass"
}

-- Read : Value => trait (Text -> Maybe Value)
instance (Read Grade) : text -> when text {
    "A" -> Some A
    "B" -> Some B
    "C" -> Some C
    "D" -> Some D
    "F" -> Some F
    _ -> None
}

grade : prompt "Enter your grade"
show (report-card grade)
```

<!-- not marked with `wipple-output` because `prompt` can't be tested -->

```
Enter your grade: Z
invalid input, please try again
Enter your grade: 42
invalid input, please try again
Enter your grade: A
top of the class
```
