# Blocks and scope

Most Wipple operations take place on a single value or list of values — for example, you can only assign one value to a variable. But what if you want to perform multiple operations in a place where one value is required? You can use blocks for this:

```wipple
result : {
  a : 1 -- or some other complicated code
  b : 2
  a + b
}

show result -- 3
```

Blocks are denoted with braces (`{` and `}`) and contain one statement per line. Each statement is evaluated in order, and the value of the last statement is used as the value of the block.

> In fact, you've already been using blocks — Wipple files are blocks implicitly, with one statement per line!

There's one problem with this code, however: even though `a` and `b` were only used as intermediate values to obtain the final result, they still exist!

```wipple
show a -- 1
show b -- 2
```

To avoid this, you can use the `do` function, which keeps any variables declared inside the scope of the input:

```wipple
result : do {
  a : 1
  b : 2
  a + b
}

show result -- 3
show a -- error
```

You can still use the final result, but the `a` and `b` variables are deleted after the block is evaluated.

> **Note:** `do` is unnecessary inside closures because closures already form their own scope:
>
> ```wipple
> add : a -> b -> {
>    sum : a + b
>    sum
> }
>
> add 1 2 -- 3
> show sum -- error
> ```

Here's another example of scoping you can try out in the playground:

```wipple
greeting : "hi"

do {
    greeting : "howdy"
    show greeting -- howdy
}

show greeting -- hi
```

Notice how the value of `foo` outside the `do` block remains unchanged.
