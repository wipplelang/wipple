# Variables

In Wipple, variables are defined using a colon (`:`):

```wipple
name : "Wilson"
show name

sum : 1 + 2
show sum
```

```wipple-output
Wilson
3
```

You can create multiple variables with the same name. When you refer to a name in your code, the most recent variable is chosen:

```wipple
n : 1
show n

n : 2
show n
```

```wipple-output
1
2
```

Each declaration is its own variable with its own value; they don't need to be the same type:

```wipple
n : 1
show n

n : "n"
show n
```

```wipple-output
1
n
```

The right-hand side of the `:` is evaluated before bringing the variable into scope:

```wipple
n : 1
n : n + 1
show n
```

```wipple-output
2
```

A variable can only be accessed after it's defined, not before:

```wipple
n : n + 1
n : 1
show n
```

```wipple-output
example:1:5: error: can't find `n`
```

Sometimes, you need to change the value of an existing variable. You can do this by putting an exclamation mark (`!`) after the variable name:

```wipple
n : 0
n! : n + 1
show n
```

Now, any code that refers to `n` will observe the updated value.
