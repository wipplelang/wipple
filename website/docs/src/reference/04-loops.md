# Loops

To repeatedly evaluate an expression, you can use `loop` combined with `end`:

```wipple
ten : {
    n : mutable 0
    loop (if (n = 10) (end n) (increment! n))
}
```

Calling `end` with a value exits the loop and returns that value.

You can also use `while` and `until` to repeatedly evaluate a condition:

```wipple
while : template condition body ->
    loop (if condition (body :: .) (end .))

until : template condition body ->
    while (not condition) body

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
