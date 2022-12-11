# Files and constants

It's encouraged to split your program into files to make it easier to maintain. In Wipple, you can import the contents of another file with the `use` template. For example, consider a file named `x.wpl` that imports `y.wpl`:

```wipple
-- x.wpl
use "y.wpl"
```

If you want to import only specific declarations in the file, you can assign the `use` to a destructuring pattern:

```wipple
-- x.wpl
{ a b c } : use "y.wpl"
```

When importing files, the order you import them in doesn't matter. This means that executable code at the top level is only allowed in the root file.

> **Note:** Wipple imports types, traits, instances and constants from a file.

## Constants

Constants are similar to variables, but can be imported by another file.

There are two steps to declaring a constant. The first is to write the constant's name followed by a type annotation:

```wipple
sum :: Number
```

And the second step is to give the constant a value:

```wipple
sum : 1 + 2
```

Importantly, constants are lazily evaluated — where variables indicate a computation to be performed _now_, a constant indicates a computation to be performed when the constant is first referenced. This means two things:

-   A file containing only constants can be `use`d by another file, since the file contains no executable code at the top level.
-   Constants can refer to themselves recursively.

The type annotation of a constant, aka. it's **signature**, cannot have type placeholders. For example, the following constant is invalid:

```wipple
x :: Maybe _
x : Some 42
```

> **Why not?** Wipple's type checker performs _local_ type inference (within constant bodies), not _global_ type inference (across constant bodies). Global type inference makes programs much harder to reason about. Consider the following (invalid) code:
>
> ```wipple
> -- a.wpl
> t :: _ -> _
> t : x -> x
>
> -- b.wpl
> use "a.wpl"
> f :: Number
> f : t 42
>
> -- c.wpl
> use "a.wpl"
> g :: Text
> g : t "hi"
>
> -- d.wpl
> use "b.wpl"
> use "c.wpl"
> ```
>
> If type placeholders were allowed in `foo`'s signature, then the type of `foo` depends on where it's used first: if `b.wpl` is type-checked before `c.wpl`, then `c.wpl` will fail to type-check, and vice versa. Since Wipple type-checks the program in a non-determistic order (or even in parallel), you would get different errors every time you compile! That would be very frustrating.

### Generic constants

In lieu of type placeholders, you can define a generic constant by using a type function in its signature. For example, we can define a function `first` which returns the first of its two inputs:

```wipple
first :: A B => A -> B -> A
first : x -> _ -> x
```
