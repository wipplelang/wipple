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

Blocks form a **scope**, which means any variables declared inside the block don't affect what is accessible outside the block:

```wipple
show result -- 3
show a -- error: 'a' does not refer to a variable
```

You can still use the final result, but the `a` and `b` variables are deleted after the block is evaluated.

Here's another example of scoping you can try out in the playground:

```wipple
greeting : "hi"

{
  greeting : "howdy"
  show greeting -- howdy
}

show greeting -- hi
```

Notice how the value of `foo` outside the block remains unchanged.
