# Variants and logic

How have we made it this far in the documentation without even discussing the `if` statement!? While boolean logic is the lowest-level primitive in most languages, in Wipple it's actually a high-level construct. Let's take a look at **variants**!

Wipple variants are similar to enums in other languages. You can create one using the `variant` function:

```wipple
Grade : variant (A B C D F)
```

By convention, variant sets and individual variants are capitalized.

All variant sets have the `Variant-Set` trait, which derives `Module`. This means you can access individual variants just like modules, and you can bring them all into scope with `use`:

```wipple
my-grade : Grade A

-- or
use Grade
my-grade : A
```

You can also use variant sets and individual variants as patterns:

```wipple
report-card : Grade grade -> {
  use Grade

  match grade {
    A : "Top of the class"
    B : "Average"
    C : "Getting there"
    _ : "Need to study"
  }
}

show (report-card my-grade) -- Top of the class
```

Hopefully you can see how booleans ("conditions") might work in Wipple — it's just `True` and `False` variants!

```wipple
Condition : variant (True False)
```

To implement `if`, we can use `match`:

```wipple
if : condition => then => else => match (condition as Condition) {
  True : then
  False : else
}
```

We can derive `Not`, `And` and `Or` using `if`:

```wipple
Condition x == Not (if x False True)
Condition a == And (Condition b -> if a b False)
Condition a == Or (Condition b -> if a True b)
```

We just implemented fundamental programming concepts using Wipple traits and variants!

## Maybes

Variants have another trick up their sleeve — they can store a value! This lets us express the concept of "some value" and "no value" (eg. `null` in other languages) using a `Maybe` variant:

```wipple
Maybe : variant {
  Some : is _
  None : _
}
```

Instead of providing a list of variants, you can provide a block. `variant` changes the meaning of `:` to "define a variant" instead of "assign to a variable". A variant can either have one value, restricted by a pattern, or no value, denoted by `_`. That is, the following two variants are equivalent:

```wipple
My-Variant : variant (A B)

My-Variant : variant {
  A : _
  B : _
}
```

We need to use `is _` when defining `Some` to indicate that `Some` can contain _any_ value and not _no_ values. Normally `is _` is the same as `_`, but in this case `_` already has a special meaning. Of course, you can use any pattern you'd like:

```wipple
-- a maybe that can only hold numbers
Maybe-Number : variant {
  Some : Number
  None : _
}
```

Variants accepting values are functions, which you can call to obtain a variant storing a value:

```wipple
maybe-a-number : Some 42
```

When a variant that has a value is used as a pattern, the contained value is provided. This lets you use `match` to get the value out of a variant:

```wipple
increment-if-exists : n -> match n {
  n Some : Some (n + 1)
  None : None
}

-- Does this look familiar? Later on we'll implement "map"
-- in a similar way!
```

You can also use `as` to "unwrap" the maybe, obtaining the `Some` value and raising an error if it's `None`:

```wipple
definitely-a-number : Some 42
number : definitely-a-number as Some
```

As mentioned earlier, `as?` is a form of `as` that returns a `Maybe` depending on if the match succeeds or not. For example:

```wipple
1 as? 1 -- Some 1
1 as? 2 -- None
"hi" as? Number -- None
(Person "Bob") as? Text -- Some "Bob" (when Person == Text)
(Some 42) as? Some -- Some 42 (does nothing)
```

If all you want to do is check that a value matches a pattern, you can use `is?` instead:

```wipple
1 is? 1 -- True
1 is? 2 -- False
"hi" is? Number -- False
(Person "Bob") is? Text -- True (when Person == Text)
(Some 42) is? Some -- True
```
