# Templates

Templates are functions that take and return expressions. Wipple templates are "hygienic", meaning that any variables the template refers to will resolve to values in the template's scope, and vice versa.

```wipple
if : bool then else ~> when bool {
    True -> then
    False -> else
}
```

In the future, more control over the structural form of the expression (like Rust macros) will be added.