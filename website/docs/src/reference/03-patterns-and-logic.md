# Patterns and Logic

Wipple has a concept of "patterns", which bind to one of many options for a piece of data. For example, we can define a `Maybe` type:

```wipple
Maybe : A => type {
    Some A
    None
}

use Maybe
```

And then bind to a `Some` or `None` value:

```wipple
x? : Some 42
Some x : x? -- x : 42
```

If `x` doesn't contain a `Some` value, the program crashes:

```wipple
x? : None
Some x : x? -- runtime error
```

You can use patterns in function parameters too!

```wipple
unwrap : Some x -> x -- unwrap :: for A -> Maybe A -> A
```

On its own, this isn't very useful — why would you want your program to crash? To get around this, we can use Wipple's fundamental logic operation, `when`. You give `when` a bunch of functions, and it will call the one whose pattern matches the input:

```wipple
Grade : type {
    A
    B
    C
    D
    F
}

use Grade

when grade {
    A -> "Top of the class"
    B -> "Pretty good"
    C -> "Getting there"
    D or F -> "Need to study"
}
```

Thanks to `when`, Wipple doesn't have regular booleans! Boolean logic is implemented using the `Boolean` type:

```wipple
Boolean : type {
    True
    False
}

use Boolean

if : bool then else ~> when bool {
    True -> then
    False -> else
}

show (format "x _ 5" (if (x = 5) "is" "is not"))
```

You can bind variables inside `when` like so:

```wipple
Map : f -> x? -> when x? {
    Some x -> Some (f x)
    None -> None
}

area : when shape {
    Square s -> s ^ 2
    Rectangle l w -> l * w
    Circle r -> 3.14 * r ^ 2
}
```

The cases you provide to `when` must be exhaustive (ie. they must cover all variants of the type). To ignore some cases, or to handle multiple cases in one branch, you can use `_` and `or`:

```wipple
when color? {
    Some (Red or Blue) -> "hooray"
    _ -> "oh no"
}
```

Alternatively, you can use `when?`:

```wipple
when? color? (Some (Red or Blue)) (show "hooray")
```

If you just want to execute a piece of code when a condition is true, you can use `when!`:

```wipple
when! (2 + 2 = 4) (show "woohoo")
```

The opposite form is `unless!`:

```wipple
unless! : template bool body -> when! (not bool) body
```
