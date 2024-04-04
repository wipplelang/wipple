# Custom error messages

Wipple has a few built-in types and traits that you can use to create custom error messages for your API.

## The `Error` trait

Whenever Wipple resolves an instance of the `Error` trait, it generates an error message. So if you add a bound involving `Error` to a function or instance, you can create different messages depending on how your API is misused.

In this example, `Error` is used to mark a function as unavailable:

```wipple
print :: Value where (Error "use `show` to display on the screen") => Value -> ()
print : unreachable

print "Hello, world!" -- error: use `show` to display on the screen
```

`Error` accepts a text value, which can also contain placeholders. The inputs to the placeholders are types, rather than values.

```wipple
Value where (Error ("_ has no description" Value)) =>
    default instance (Describe Value) : unreachable

my-value : ()
Describe my-value -- error: `()` has no description
```

If you want to display the source code of the input rather than its type, surround the placeholder with backticks (`` `_` ``):

```wipple
Value where (Error ("`_` has no description" Value)) =>
    default instance (Describe Value) : unreachable

my-value : ()
Describe my-value -- error: `my-value` has no description
```

## Error utilities

In addition to providing a message to `Error`, you may provide a tuple containing the message alongside these types:

-   The `Error-Location` type is used to change where the error appears in the source code:

    ```wipple
    Value where (Error ("`_` has no description" Value ; Error-Location Value)) =>
        default instance (Describe Value) : unreachable

    my-value : ()
    Describe my-value -- error: `my-value` has no description
    --       ~~~~~~~~
    ```

-   The `Error-Fix` type is used to generate a fix. The first input is the fix message, and the second input is the replacement for the code highlighted by the error:

    ```wipple
    Body where (Error ("missing `repeat` here" ; Error-Fix "add `repeat`" ("`repeat _`" Source))) =>
        instance (Mismatch Times ({Body} -> ())) : ...

    (4 times) { -- error: missing `repeat` here
        show "Hello, world!"
    }

    -- After clicking "add `repeat`"...
    repeat (4 times) {
        show "Hello, world!"
    }
    ```

-   The `Source` type is always displayed as the source code for the expression that caused Wipple to resolve the bound. The above example uses `Source` to add `repeat` before `(4 times)`.

## The `Mismatch` type

Whenever two types mismatch, Wipple will attempt to resolve an instance of `Mismatch`. You can define your own instances and add `Error` bounds to generate custom error messages:

```wipple
Box : Value => type Value

Value where (Error ("not a box" ; Error-Location Value ; Error-Fix "add `Box`" ("`(Box _)`" Value))) =>
    instance (Mismatch Value (Box Value)) : ...

(42 :: Box _) -- not a box

-- After clicking "add `Box`"...
((Box 42) :: Box _)
```
