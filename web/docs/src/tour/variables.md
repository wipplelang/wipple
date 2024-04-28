# Variables

In Wipple, variables are defined using a colon (`:`):

```wipple
name : "Wilson"
show name -- Wilson

sum : 1 + 2
show sum -- 3
```

You can create multiple variables with the same name. When you refer to a name in your code, the most recent variable is chosen:

```wipple
n : 1
show n -- 1

n : 2
show n -- 2
```

Each declaration is its own variable with its own value; they don't need to be the same type:

```wipple
n : 1
show n -- 1

n : "n"
show n -- n
```

The right-hand side of the `:` is evaluated before bringing the variable into scope:

```wipple
n : 1
n : n + 1
show n -- 2
```

A variable can only be accessed after it's defined, not before:

```wipple
n : n + 1 -- error: cannot find `n`
n : 1
show n
```
