# Patterns

Wipple uses pattern matching to express control flow. For example, let's say we want to generate a report card:

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

show (report-card A)
```

```wipple-output
top of the class
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

show (my-if My-True {123} {456})
```

```wipple-output
123
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
#Maybe-Number : type {
#    Some-Number Number
#    No-Number
#}
describe-maybe-number : maybe -> when maybe {
    Some-Number n -> "we have a number: _" n
    No-Number -> "we don't have a number"
}

show (describe-maybe-number (Some-Number 42))
show (describe-maybe-number No-Number)
```

```wipple-output
we have a number: 42
we don't have a number
```

Why is this useful? It means we can represent errors in our program! Let's go back to our report card example, and allow the user to specify a grade as input:

```wipple
#Grade : type {
#    A
#    B
#    C
#    D
#    F
#}
#
#report-card :: Grade -> Text
#report-card : grade -> when grade {
#    A -> "top of the class"
#    B -> "good work"
#    C -> "need to study"
#    D or F -> "didn't pass"
#}
Maybe-Grade : type {
    Valid-Grade Grade
    Invalid-Grade
}

parse-grade :: Text -> Maybe-Grade
parse-grade : text -> when text {
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

<!-- not marked with `wipple-output` because `prompt` can't be tested -->

```
Enter your grade: A
top of the class
Enter your grade: B
good work
Enter your grade: Z
invalid grade
...
```

Wipple's type system will check for us that we handle the error — watch what happens if we pass our `Maybe-Grade` to `report-card` directly:

```wipple
#Grade : type {
#    A
#    B
#    C
#    D
#    F
#}
#
#report-card :: Grade -> Text
#report-card : grade -> when grade {
#    A -> "top of the class"
#    B -> "good work"
#    C -> "need to study"
#    D or F -> "didn't pass"
#}
#
#Maybe-Grade : type {
#    Valid-Grade Grade
#    Invalid-Grade
#}
#
#parse-grade :: Text -> Maybe-Grade
#parse-grade : text -> when text {
#    "A" -> Valid-Grade A
#    "B" -> Valid-Grade B
#    "C" -> Valid-Grade C
#    "D" -> Valid-Grade D
#    "F" -> Valid-Grade F
#    _ -> Invalid-Grade
#}
grade : parse-grade (prompt "Enter your grade")
show (report-card grade)
```

```wipple-output
example:32:19: error: expected `Grade` here, but found `Maybe-Grade`
```

If you've used a language with exceptions, Wipple's pattern matching is kind of like `try...catch`, but you are forced to handle every error explicitly. This can seem cumbersome at first, but it makes bugs much easier to track down. And don't worry, you don't have to define your own `Maybe` type every time — Wipple has one built in that works for any type! We'll learn how to use it in the next chapter.
