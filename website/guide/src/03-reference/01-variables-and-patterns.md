# Variables and patterns

A **variable** is a way to give a name to a value. In Wipple, you can declare one or more variables using the `:` operator:

```wipple
sum : 1 + 1
```

The left-hand side of the `:` is a **pattern**, and the right-hand side is an expression. The expression is evaluated and then matched according to the pattern, assigning to the new variables.

## Patterns

A pattern is a way to describe the structure of a value and extract its parts into variables. There are several kinds of patterns:

-   A **name pattern** (eg. `x`) matches an entire value and assigns it to a variable name.
-   A **variant pattern** (eg. `Some x`, `None`) matches a variant of an enumeration and its associated values.
-   A **destructuring pattern** (eg. `{ x y z }`) matches the fields of a structure. Providing just the name of a field (eg. `{ x }`) is equivalent to matching the field as a variable (ie. `{ x : x }`). It is not required to list all fields; missing fields are ignored.
-   A **tuple pattern** (eg. `x , y , z`) matches each element of a tuple.
-   A **literal pattern** (eg. `42`, `3.14`, `"hi"`) matches a value if it is equal to the literal.
-   An **`or` pattern** (eg. `x or y`) attempts to match the first pattern and then the second pattern.
-   A **`where` pattern** (eg. `x where y`) matches the pattern only if the condition following the `where` is satisfied.
-   A **wildcard pattern** matches everything and binds no variables.
-   An **instance pattern** has to do with [traits](04-traits.html) and are discussed in that section.

Patterns may be composed however you want; for example, you can match a `Maybe (Number , Maybe Number)` using the pattern `Some (1 , Some x)`.

You can also use patterns when defining functions, eg. `a , b , c -> a + b + c` accepts a tuple and returns the sum of its elements.

## Exhaustiveness and the `when` expression

Patterns on the left-hand side of a `:` must be **exhaustive**, meaning they match every possible value the right-hand side could contain. For example, the `Some x` in `Some x : m` where `m :: Maybe Number` is invalid because `m` could also be `None`. To match multiple patterns on a single value, you can use a `when` expression:

```wipple
when m {
    Some x -> a
    None -> b
}
```

`when` evaluates `m` and then attempts to match each provided pattern in order. The first pattern that matches the input will have its associated body executed. `when` also checks for exhaustiveness, but does so by combining the structure of all the patterns provided into a single set of possible matches. In the above example, since a `Maybe` may only contain `Some` or `None`, the `when` expression is exhaustive. If you want to have a "default" branch that's executed when none of the other patterns match, just add a wildcard pattern to the end:

```wipple
show (when m {
    Some 42 -> "matched 'Some' with 42"
    None -> "matched 'None'"
    _ -> "matched something else"
})
```

## Scope

All variables are **scoped** to the block, function, or `when` expression in which they are declared. Wipple uses lexical scope, not function scope, so the following code doesn't work:

```wipple
if True {
    x : 1
} {
    x : 2
}

show x -- error: cannot find `x`
```

The correct way is to "lift" the variable assignment to the block level:

```wipple
x : if True 1 2
show x
```

The `:` syntax is equivalent to writing a `when` expression as follows:

```wipple
-- This...
x : a
f x

-- is equivalent to...
when a {
    x -> f x
}
```

Since each new variable assignment effectively introduces its own scope, you can declare two different variables with the same name. The original variable is no longer accessible, but functions that refer to it will continue to do so instead of referring to the new variable:

```wipple
x : 1
show-x : () -> show x -- refers to the above 'x'
x : 2 -- this creates a new variable named 'x' and does not change the original 'x'
show-x () -- displays "1", not "2"
show x -- displays "2"
```

A consequence of this is that the new variable does not need to have the same type as the original. If this "shadowing" of variable names is confusing, we can translate the `:` syntax into the `when` syntax to make the scoping explicit:

```wipple
when 1 {
    x -> when (() -> show x) {
        show-x -> when 2 {
            x -> {
                show-x () -- displays "1", not "2"
                show x -- displays "2"
            }
        }
    }
}
```

## Mutability

Wipple encourages structuring your code so that functions produce new values instead of mutating their inputs. But if you need to have mutability, Wipple offers the `Mutable` type:

```wipple
x : mutable 1
show-x : () -> show (get x)
x . set! 2
show-x () -- displays "2"
```

The `mutable` function creates a new mutable value (of type `Mutable Number`), the `get` function retrieves the value, and the `set!` function mutates the value. By convention, functions that mutate `Mutable` values end in `!`.
