# Templates

Templates are functions that take and return expressions. Wipple templates are "hygienic", meaning that any variables the template refers to will resolve to values in the template's scope, and vice versa.

```wipple
if : template bool then else -> when bool {
    True => then
    False => else
}
```

You can also restrict the type of expression passed to a template like so:

```wipple
t : template (x :: Name or Number) -> x

t foo -- foo
t 42 -- 42
t "hi" -- error
t { ... } -- error
```

In the future, more control over the structural form of the expression (like Rust macros) will be added.
