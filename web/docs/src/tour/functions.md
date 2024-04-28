# Functions

Functions are written with an arrow (`->`). The inputs go on the left side of the arrow, and the output goes on the right:

```wipple
add : a b -> a + b
sum : add 1 2
show sum -- 3
```

Functions are also just values, so they can be assigned to variables as shown above, or they can be used inline:

```wipple
sum : (a b -> a + b) 1 2
show sum -- 3
```

If you want to have multiple statements in a function, you can use a `do` block:

```wipple
debug-sum : a b -> do { -- don't forget `do`!
    show "called `debug-sum`"
    a + b
}

show (debug-sum 1 2) -- called `debug-sum`
                     -- 3
```

Let's build a function that takes a block and runs it twice:

```wipple
twice : block -> do {
    do block
    do block
}

twice {
    show "Hello, world!"
}
-- Hello, world!
-- Hello, world!
```

We just defined our own control flow!

Finally, you can use text values as functions — if you put underscore (`_`) placeholders in the text and provide values afterward, you can do string interpolation:

```wipple
greet : name -> "Hello, _!" name
show (greet "world") -- Hello, world!
```
