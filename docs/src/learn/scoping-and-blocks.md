# Scoping and blocks

Say you wanted to split a large computation up into smaller ones, storing the intermediate results in variables:

```wipple
a : do-something-expensive
b : do-something-else
result : handle a b
```

The problem is now you have these extra `a` and `b` variables lying around, which could have overwritten something important from earlier in the program. To solve this problem, you can use the `do` function, which accepts a block (a list of statements surrounded by `{}`) and runs it in its own scope:

```wipple
result : do {
    a : do-something-expensive
    b : do-something-else
    handle a b
}
```

The last statement in the block is returned, and the `a` and `b` variables are deleted after finishing. Here's an example of scoping you can try out in the playground:

```wipple
foo : "hi"

do {
    foo : "howdy"
    show foo -- shows "howdy"
}

show foo -- shows "hi"
```

Notice how the value of `foo` outside the `do` block remains unchanged.

`do` is just one function that accepts a block as input — the standard library provides other functions (like `inline` and `match`) which we'll get to later.
