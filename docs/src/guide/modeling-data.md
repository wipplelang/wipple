# Modeling data

## Structures

To define a structure type, use the `type` keyword:

```wipple
Sport : type {
    name :: String
    players :: Number
}
```

You can construct a structure value similarly:

```wipple
basketball : Sport {
    name : "Basketball"
    players : 5
}
```

To get the values out of a structure, you can put a block on the left-hand side of the colon (`:`), listing the field(s)' names and the corresponding variable names.

```wipple
Sport {name : sport-name} : basketball
show sport-name
```

```
Basketball
```

## Pattern matching

Wipple uses pattern matching to express control flow. For example, let's say we want to generate a report card:

```wipple
Grade : type {
    A
    B
    C
    D
    F
}

report-card :: Grade -> String
report-card : grade -> when grade {
    A -> "top of the class"
    B -> "good work"
    C -> "need to study"
    D or F -> "didn't pass"
}

show (report-card A) -- top of the class
```

First, we define our patterns using `type`. Rather than providing fields, we list the _variants_, and Wipple will create an enumeration for us. Then, we use `when` to return a different value for each variant. You can use `or` to match multiple variants at once.

In fact, in Wipple, `if` is just a regular function that matches on `Boolean`. We can create our own easily:

```wipple
My-Boolean : type {
    My-True
    My-False
}

my-if : bool then else -> when bool {
    My-True -> do then
    My-False -> do else
}

show (my-if My-True {123} {456}) -- 123
```

In addition to enumerations like these, you can store data alongside each pattern, allowing you to express values that are tied to a condition — in other words, the value is "wrapped" in a pattern, and you need to "unwrap" the value by checking for the condition using `when`. This may sound a bit confusing if you've used other languages without this feature, so let's look at an example:

```wipple
Maybe-Number : type {
    Some-Number Number
    No-Number
}
```

Here, we create a `Maybe-Number` value with two patterns. The first pattern contains a `Number`, and the second pattern contains nothing. Now, we can use pattern matching to "unwrap" the `Maybe-Number`:

```wipple
describe-maybe-number : maybe -> when maybe {
    Some-Number n -> "we have a number: _" n
    No-Number -> "we don't have a number"
}

show (describe-maybe-number (Some-Number 42))
show (describe-maybe-number No-Number)
```

Why is this useful? It means we can represent errors in our program! Let's go back to our report card example, and allow the user to specify a grade as input:

```wipple
Maybe-Grade : type {
    Valid-Grade Grade
    Invalid-Grade
}

parse-grade :: String -> Maybe-Grade
parse-grade : string -> when string {
    "A" -> Valid-Grade A
    "B" -> Valid-Grade B
    "C" -> Valid-Grade C
    "D" -> Valid-Grade D
    "F" -> Valid-Grade F
    _ -> Invalid-Grade
}

repeat forever {
    grade : parse-grade (prompt "Enter your grade")

    when grade {
        Valid-Grade g -> show (report-card g)
        Invalid-Grade -> show "invalid grade"
    }
}
```

Wipple's type system will check for us that we handle the error — watch what happens if we pass our `Maybe-Grade` to `report-card` directly:

```wipple
grade : parse-grade (prompt "Enter your grade")
show (report-card grade) -- error
```

Wipple includes a built-in type for error handling, `Maybe`. Here is its definition:

```wipple
Maybe : value => type {
    Some value
    None
}
```

Similarly, the built-in `Read` trait is defined as:

```wipple
Read : output => trait (String -> Maybe output)
```

The example above can be rewritten to use `Maybe` and `Read`, and `prompt` will handle validation for us!

```wipple
instance (Read Grade) : string -> when string {
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
