# The trait system

Wipple supports ad-hoc polymorphism using **traits**. Traits in Wipple are similar to multi-parameter type classes in Haskell. A trait is defined over one or more type parameters, and users can define **instances** replacing these type parameters with specific types. Wipple will select the appropriate instance automatically.

For example, a common trait in Wipple is `Describe`, which is used by `show` to convert its input to a `String`:

```wipple
Describe : value => trait (value -> String)

instance (Describe String) : s -> s
instance (Describe Boolean) : b -> if b {"True"} {"False"}
```

To reference an instance of a trait, you write the trait's name (forming a **trait expression**), and the compiler will select the matching instance based on the data type of the trait expression.

```wipple
(Describe :: String -> String) -- instance (Describe String)
(Describe :: Boolean -> String) -- instance (Describe Boolean)
```

If the trait is a function, then you can also simply call the trait expression directly, and type inference allows you to omit the annotation.

```wipple
Describe "abc" -- instance (Describe String)
Describe True -- instance (Describe Boolean)
```

The remainder of this article explains how the compiler selects instances.

## The trait's signature

When the compiler encounters a trait expression, it first instantiates the trait's signature, replacing all referenced type parameters with "fresh" placeholders. For `Describe`, the trait expression begins with the type `<value> -> String`, where `<value>` represents the instantiated `value` parameter.

Recall that Wipple's type checker operates on a list of ordered constraints, so each trait expression actually generates the following constraints:

- A **group constraint** grouping the instantiated signature with the trait expression;

- **Type constraints** for each concrete type in the signature; and

- A **bound constraint** to find the matching instance.

Because all constraints of each type are evaluated before moving to the next type, the bound constraint won't be evaluated until all available type information from the context surrounding the trait expression is collected. For example, given the program `Describe True`, the constraint `True :: Boolean` will be evaluated before searching for a matching `Describe` instance, allowing the compiler to easily select `instance (Describe Boolean)`.

## Bound resolution

When the compiler reaches the bound constraint, it iterates through each instance defined on the trait and attempts to instantiate them. This process is performed on copies of the type checker, because otherwise, non-matching instances would "pollute" the groups involved in the trait expression. (For example, `instance (Describe String)` adds the type `String` to the group containing `True` in `Describe True`, but that group already contains the type `Boolean`, leading to an error. Instead of showing the user this error, we want to ignore this non-matching instance.)

First, the compiler sorts the instances into four groups: "regular" instances, `error` instances, `default` instances, and `default error` instances. These groups are considered in order, so that the compiler will only try default instances if no regular instances match. Similarly, when the compiler checks for overlapping instances, this check is done per group, allowing users to define generic `default` instances as well as specific regular instances.

Within each of these groups, the compiler creates a copy of the type checker for every instance. Within these copies, the associated instance is instantiated, and the type checker is ran until the first bound constraint is reached (i.e., only group and type constraints are evaluated). If at this point the type checker has reported an error, the instance is discarded. Otherwise, the instance and the copy of the type checker are added to a list.

Next, the compiler looks at the length of the resulting list of instances:

- If there are no matching instances, the next group of instances is considered. If all groups have been considered and there are still no matching instances, then the compiler reports an error.

- If there is more than one matching instance, then the bound constraint is added back into the queue, and will run again when more type information is available (i.e., by resolving another instance or via a default type constraint).

- If there is exactly one matching instance, then evaluation of the bound constraint stops and the outer type checker's state is replaced with the copy's state.

Assuming there is a single matching instance, the type checker will now contain the remaining constraints not evaluated by the inner copy (namely, the bounds on the selected instance itself). This enables instances to have bounds of their own. To prevent infinite recursion of the form `instance (A value) where (B value)` and `instance (B value) where (A value)`, the type checker has an iteration limit.

`error` instances are treated the same as regular instances during bound resolution. The compiler runs a separate query later to report `error` instances as error messages.

## Implied instances

Bounds placed on instance and constant definitions are the responsibility of the caller to satisfy. That means within the definitions, we can assume these bounds will have a single matching instance. Importantly, these bounds are _not_ instantiated and instead only apply to the specific type parameters they use — in other words, the type parameters are treated as concrete types. For example, `unzip` has two bounds for `Initial` containing different type parameters:

```
unzip :: collection -> (left; right)
  where (Initial left) (Initial right) ...
```

When `unzip`'s implementation references `Initial`, it could refer to either of these bounds. If the bounds were instantiated like regular instances, then the trait expression `(Initial :: right)` would resolve to both `Initial <left>` _and_ `Initial <right>` (where `<left` and `<right>` are instantiated placeholders). However, since `Initial left` and `Initial right` are implied, they remain uninstantiated and only `Initial right` will be selected.

Finally, when type checking instance definitions, the instance being defined is also implied inside its own definition, allowing for recursive instances.

## Inferred parameters

Traits can mark one or more of their type parameters as **inferred** using `infer`. During bound resolution, inferred parameters are _not_ checked and instead propagate back to the caller. If regular type parameters behave as "inputs" to the trait when searching for bounds, inferred parameters effectively behave as "outputs". For example, the `Add` trait defines its `sum` parameter as inferred:

```wipple
Add : left right (infer sum) => trait (left right -> sum)
instance (Add Number Number Number) : ...
```

As a result, if you call `Add` with two `Number`s where a `String` is expected, the `instance (Add Number Number Number)` is still selected (because the third `Number` is ignored), and you get a standard type conflict error on the call to `Add` instead.

```wipple
sum :: String
sum : 1 + 2 -- error: `1 + 2` is a `Number` or a `String`
```
