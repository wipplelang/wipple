# Typechecking

The typechecker is responsible for determining that every expression in your program is used correctly. For example, adding a string to a number would raise an error in a typechecked program instead of producing invalid results. To resolve the error, you have to be explicit: do you want to convert the string to a number and add the two numbers, or convert the number into a string and concatenate the two strings?

Wipple's typechecker does a lot of things, namely:

1.  **Type inference:** Assigning every expression a type
2.  **Unification:** Checking that every expression's type is valid in the surrounding context
3.  **Trait resolution and bounds checking:** Determining which instance of a trait to use and that all bounds are satisfied
4.  **Monomorphization and specialization:** Instantiating constants and instances so their generic type parameters are replaced with concrete types, and using specialized constants if applicable
5.  **Exhaustiveness checking:** Checking that a pattern or set of patterns cover all possible input values
6.  **Finalization:** Ensuring that every expression in the program has a concrete type

Not all of these steps happen in order; the typechecker repeats some steps to collect more type information so the user doesn't have to provide explicit annotations.

## Type inference

The basic idea behind type inference is that every expression resolves to some value at runtime, and we want to determine the shape of that value at compile time. This "shape", or **type**, can be determined automatically based on how the value is used. To make our programs deterministic, there is a rule: the type of an expression must be preserved while it is being evaluated. Essentially, we should be able to "pause" the program at any point and see that the types haven't changed:

```wipple
f :: A -> B
g :: B -> C
h :: C -> D

-- The type of every expression in this program is preserved during evaluation.
h (g (f (... :: A) :: B) :: C) :: D
h (g (... :: B) :: C) :: D
h (... :: C) :: D
(... :: D)
```

With this rule in mind, we can apply some constraints to the expressions. For example, if `f :: A -> B`, we know that `f x :: B` and `x :: A`. Likewise, if we know `x :: A`, then we at least know `f :: A -> _` (where `_` is unknown). If the result of `f x` is then assigned to a variable with type `B`, now we know the full type of `f` to be `A -> B`. By building a giant list of constraints and solving them, we can **infer** the types of most expressions automatically, without the need for explicit type annotations everywhere!

In Wipple, there is only one type of constraint used for type inference: the **substitution**. Essentially, we have a map between **type variables** and types that is stored in a **context**. We can:

-   Collect new substitutions by **unifying** two types together (see below)
-   **Apply** a type to substitute its type variables with the substitutions stored in the context
-   **Finalize** a type to assert that it doesn't contain any type variables; ie. the type is fully known

So if we collect a giant list of substitutions from all the expressions in a program, apply all of these types, and then finalize them, we'll know whether our program is fully typed or not! In Wipple, if the program is not fully typed (ie. there are type variables remaining even after applying), we produce an error. But in dynamically/gradually typed languages, you could replace all remaining type variables with `Any` instead.

## Unification

Unification is basically equality checking with type variables thrown in. Here is the algorithm:

1.  Apply both types.
    -   If we find a substitution between two variables, substitute the old variable and apply the new variable; that way, we can have multi-step substitutions.
2.  If either type is a type variable, add a substitution between the variable and the other type.
3.  Otherwise, check if the two types are equal, recursively unifying any sub-terms.

For example, if we want to unify a type `Number -> {0}` with a type `{1} -> Text` (where `{n}` represents a unique type variable):

1.  Apply both types; since we don't have a substitution for `{0}` or `{1}`, there's nothing to do here.
2.  Are both types function types? Yes; continue.
3.  Unify the input types of both sides:
    1.  Found a variable, `{1}`; add the substitution `1 => Number`.
4.  Unify the output types of both sides:
    1.  Found a variable, `{0}`; add the substitution `0 => Text`.
5.  Done.

Now we can apply both types, getting `Number -> Text` in both cases!

What if the types are incompatible? Let's try unifying `{1} -> Text` with `Text -> Text`:

1.  Apply both types; we have a substitution for `{1}`, so the left type becomes `Number -> Text`.
2.  Are both types function types? Yes; continue.
3.  Unify the input types of both sides:
    1.  Unify `Number` and `Text`; the two types aren't equal, so stop and produce an error.

One important thing here is that we don't care about the actual expressions at all, only their types. In Wipple, types do not contain span information or anything that relates them to a particular expression. That's because the whole point of typechecking is to determine if _two or more_ expressions are compatible with each other.

One more example — function calling. For function calling, we split the work into two steps:

1.  Determine the type of the function expression, and unify this type with the type `{0} -> {1}`, where `{0}` and `{1}` are fresh type variables.
2.  Determine the type of the input expression, and unify this type with `{0}`.
3.  The type of the function call expression as a whole is `{1}`.

For expressions that instantiate a type or refer to a trait or constant, we copy the type from the declaration and replace all type parameters with new type variables. So if `make-tuple :: A B => A -> B -> (A , B)`, the expression `make-tuple` will have type `{0} -> {1} -> ({0} , {1})`. This does _not_ happen inside `make-tuple`'s body, where `A` and `B` are preserved so that `make-tuple` cannot construct values of type `A` or `B` or assume anything else about them.

And finally, expressions that resolved to an error during lowering (eg. undefined variables) are assigned the error type, which unifies with every other type. This is so that error expressions don't produce even more errors during typechecking, confusing the user.

## Trait resolution and bounds checking

After every expression has been assigned a type (which may or may not contain type variables),
