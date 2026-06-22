# Generics and instantiation

Instantiation is the process performed by the compiler to give each use of a generic definition a "fresh" set of types. For example, in the program below, `show` is called with both a `Number` and a `String`, which is allowed because the `value` type parameter is copied into each call separately, forming different groups:

```wipple
show :: value -> () where (Describe value)
show : output (Describe value)

show 123 -- `value` is `Number`
show "abc" -- `value` is `String`
```

## Definition site

In generic code, such as within the signature and body of `show` above, type parameters are given a unique _concrete type_ using a type constraint. This prevents values of parameter type (e.g., `value`) from being used in place of other concrete types or type parameters. Without this constraint, you could easily define a `transmute` function:

```wipple
transmute :: a -> b
transmute : x -> x -- if `a` was assignable to `b` here...

x : (transmute 123 :: String) -- ...then `x` can be a `String` but hold a `Number`!
```

Similarly, in the body of `show` above, this means the `Describe value` bound only applies to the specific `value` parameter, not any arbitrary type like at the use site. Since bounds are implied inside the body of generic definitions, allowing type parameters to be assignable to each other would allow you to refer to an undefined instance.

When a type parameter appears multiple times in a signature, only the first appearance is given the type constraint; the other appearances are grouped with the first.

## Use site

During instantiation, instead of deeply applying the type of the definition once and then replacing encountered type parameters, Wipple creates a copy of **all** the constraints generated — including bounds — while traversing the definition's signature (but not its implementation). Then, every node referenced in these constraints is replaced with a fresh node, and any type constraints involving type parameters are similarly replaced by fresh group constraints. Finally, these new constraints are enqueued in the type checker.

The mapping between generic and instantiated nodes/parameters is recorded in a mutable _substitutions_ list, which can be shared across multiple instantiations using a key. This allows, for example, bound resolution to retrieve back the resolved parameters from each candidate.

In addition, the compiler passes around the _source node_ responsible for instantiating a given definition. This source node is attached to all instantiated nodes via the `Instantiated` fact. When there are no non-generic nodes available (i.e., all relevant nodes are constants or traits), this allows diagnostics to show conflicts in instantiated type parameters on the source node, such as for the program below:

```wipple
f :: Number -> ()
list :: List String

-- There are no variables here!
(each f) list -- error: in `each`, `element` is a `String` or a `Number`
```

This information is also used to populate links in comments. Links to the definition are replaced with the source node, and links to type parameters are replaced with a representative from the instantiated parameter's group:

```wipple
-- [`Equal`] returns `True` if both [`value@type`] values are equal.
Equal : value => trait (value value -> Boolean)

123 = 456 -- on hover: `123 = 456` returns `True` if both `Number` values are equal.
```
