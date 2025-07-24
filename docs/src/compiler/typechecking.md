# Typechecking

The typechecker is responsible for determining that every expression in your program is used correctly. For example, adding a string to a number would raise an error in a typechecked program instead of producing invalid results. To resolve the error, you have to be explicit: do you want to convert the string to a number and add the two numbers, or convert the number into a string and concatenate the two strings?

Wipple's typechecker works in four main steps:

1.  **Type inference:** Assigning every expression a type
2.  **Unification:** Checking that every expression's type is valid in the surrounding context
3.  **Bounds checking:** Finding implementations for constant and trait expressions
4.  **Finalization:** Ensuring that every expression in the program has a concrete type

The typechecker works on one declaration (constant, instance, or top-level code) at a time; the implementation of one declaration cannot affect how the implementation of a different declaration is compiled.

## Type inference

The basic idea behind type inference is that every expression resolves to some value at runtime, and we want to determine the "shape" of that value at compile time. This shape, or **type**, can be determined automatically based on how the value is used. To make our programs deterministic, there is a rule: the type of an expression must be preserved while it is being evaluated. Essentially, we should be able to "pause" the program at any point and see that even if an expression has been reduced, its type hasn't changed:

```wipple
f :: A -> B
g :: B -> C
h :: C -> D

-- The type of every expression in this program is preserved during evaluation.
(h (g (f (x :: A) :: B) :: C) :: D)
(h (g (f x :: B) :: C) :: D)
(h (g (f x) :: C) :: D)
(h (g (f x)) :: D)
```

With this rule of **type preservation** in mind, we can apply some constraints to the expressions. For example, if `f :: A -> B`, we know that `f x :: B` and `x :: A`. Likewise, if we know `x :: A`, then we at least know `f :: A -> _` (where `_` is unknown). If the result of `f x` is then assigned to a variable with type `B`, now we know the full type of `f` to be `A -> B`. By building a giant list of constraints and solving them, we can **infer** the types of most expressions automatically, without the need for explicit type annotations everywhere!

In Wipple, there is only one type of constraint used for type inference: the **substitution**. Essentially, we have a map between **type variables** (not to be confused with variables defined with `:`) and types that is stored in a **context**. We can:

-   Collect new substitutions by **unifying** two types together (see below)
-   **Apply** a type to substitute its type variables with the substitutions stored in the context
-   **Finalize** a type to assert that it doesn't contain any type variables; ie. the type is fully known

So if we collect a giant list of substitutions from all the expressions in a declaration, apply all of these types, and then finalize them, we'll know whether our declaration is well-typed or not! In Wipple, if a declaration is not fully typed (ie. there are type variables remaining even after applying), we produce an error. But in dynamically/gradually typed languages, you could replace all remaining type variables with `Any` instead.

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

What if the types are incompatible? Let's now try unifying `{1} -> Text` with `Text -> Text`:

1.  Apply both types; we have a substitution for `{1}`, so the left type becomes `Number -> Text`.
2.  Are both types function types? Yes; continue.
3.  Unify the input types of both sides:
    1.  Unify `Number` and `Text`; the two types aren't equal, so stop and produce an error.

One more example — function calling. For function calling, we split the work into two steps:

1.  Determine the type of the function expression, and unify this type with the type `{0} -> {1}`, where `{0}` and `{1}` are fresh type variables.
2.  Determine the type of the input expression, and unify this type with `{0}`.
3.  The type of the function call expression as a whole is `{1}`.

For expressions that instantiate a type or refer to a trait or constant, we copy the type from the declaration and replace all type parameters with new type variables. So if `make-tuple :: A B => A -> B -> (A ; B)`, the _expression_ `make-tuple` will have type `{0} -> {1} -> ({0} ; {1})`. This does _not_ happen inside the body of `make-tuple` itself, where `A` and `B` are preserved so that `make-tuple` cannot construct values of type `A` or `B` or assume anything else about them.

And finally, expressions that resolved to an error during lowering (eg. undefined variables) are assigned the error type, which unifies with every other type. This is so that error expressions don't produce even more errors during typechecking, confusing the user.

## Bounds checking

After every expression has been assigned a type (which may or may not contain type variables), the typechecker attempts to find implementations for as many trait expressions and bounds as possible. This process is repeated until an implementation has been found for all expressions that need one, no progress is made (resulting in an error), or a predefined limit is reached (64 repetitions).

Remember that before this phase, we instantiate the types of constant and trait expressions to type variables representing concrete types. So during bounds checking, all we need to do is attempt to unify the types of these expressions with every possible implementation, and the first one that unifies is chosen. This is done by cloning the typechecker's context (ie. set of substitutions) before unifying, and if unification fails, reverting the context to this snapshot. That way, if unification succeeds, the typechecker can incorporate any inferred types into future rounds of bounds checking.

In addition to unifying the types of the implementations, any **bounds** attached to the implementation's signature are checked too. This uses the same logic as trait expressions — internally, a trait expression `T : A B C => trait (A ; B ; C)` is actually represented as a constant `T :: A B C where (T A B C) => (A ; B ; C)`.

If there aren't any implementations that satisfy the current type of the expression, the expression is left as-is and will be checked again in the next pass, when hopefully more types have been inferred. However, if the type unifies but the bounds don't, an error is raised immediately.

Bounds may refer to themselves recursively, so to accommodate this, the typechecker maintains a stack of bounds it has already checked; if the stack already contains the current bound, that bound is assumed to be satisfied. This allows things like `A where (Describe A) => instance (Describe (Maybe A))` where `A` is `Maybe _`.

## Finalization

Finally, the typechecker does one last pass over all expressions to ensure they don't contain any unresolved type variables. If one does, the `could not determine what kind of value this code produces` error is produced.

## Other things the typechecker does

-   **Exhaustiveness checking:** The typechecker also performs exhaustiveness checking for variable assignments, function parameters and `when` expressions. The algorithm was adapted from [this paper](https://julesjacobs.com/notes/patternmatching/patternmatching.pdf) and [this example](https://github.com/yorickpeterse/pattern-matching-in-rust) to support tuple, destructuring and literal patterns in addition to enumeration ("constructor") patterns.

-   **Instance collision checking:** Whenever a new instance is processed, the typechecker loops over all previous instances to see if they overlap. During this check, type parameters are treated like type variables and unify with everything (so that `A B => instance (T A B)` and `A B => instance (T B A)` overlap). No instance may unify with any other instance, even if the bounds are different.

-   **Default types:** If you assign a default type to a type parameter, it will be used when a better type can't be inferred. During bounds checking, if no progress is made, the typechecker will traverse the syntax tree until it finds an expression with a default type. If one is found, it will be substituted and bounds checking will continue.

-   **Inferred parameters:** You can prefix a type parameter with `infer` to change the order of bounds checking. Usually, the type checker determines the types of type parameters from the type of the expression provided at the use site, and then checks to make sure any bounds are satisfied. `infer` reverses this process — it determines the type of the type parameter marked `infer` from any bounds, and then checks to make sure this type matches the type at the use site. `infer` doesn't change the behavior of valid code, but it can produce better error messages for invalid code.

-   **Units for numbers:** As a special case, if a number appears in function position with an input of type `Number -> _`, Wipple will swap the expressions so that the input is called with the number. This allows you to write a unit after a number; for example, if `pixels :: Number -> Pixels`, then `5 pixels` is equivalent to `pixels 5` and both expressions have type `Pixels`.

-   **Custom error messages:** The typechecker has special knowledge of the `Error` and `Mismatch` traits to produce custom error messages. See [Custom error messages](../guides/custom-error-messages.md) for more information.
