# Loops

To repeatedly evaluate an expression, you can use `loop`. `loop` accepts a block evaluating to a `Flow` value, namely `Continue` or `End`:

```wipple
Flow : A => type {
    Continue
    End A
}

ten : {
    n : mutable 0
    loop {
        if (n = 10) {
            End n
        } {
            increment! n
            Continue
        }
    }
}
```

You can also use `while` and `until` to repeatedly evaluate a condition:

```wipple
while : condition body ~> loop {
    if condition {
        body
        Continue
    } {
        End ()
    }
}

until : condition body ~> while (not condition) body

ten : {
    n : mutable 0
    while (n < 10) (increment! n)
    n
}

ten : {
    n : mutable 0
    until (n = 10) (increment! n)
    n
}
```