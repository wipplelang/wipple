---
layout: blog
title: <code>Mutable</code> projections
date: 2023-11-13
---

Wipple's `Mutable` type is used to provide a binding to a value that's shared across multiple places in the program. Now, `Mutable` is more flexible: you can create a binding to a part of a value! Let's take a look at an example to see why this is useful.

Say you have a `Mutable Person` with a `name`, and you want to add a suffix to the name. Previously, you would have to retrieve the `name`, change it, and then build a whole new `Person` value to pass to `set!`.

```wipple
Person : type {
  name :: Text
  age :: Natural
}

graduate! :: Mutable Person -> ()
graduate! : person -> person . set! (Person {
  name : (name of get person) + ", Ph.D."
  age : age of get person
})
```

Now, you can use projections to make this code much simpler!

```wipple
graduate! :: Mutable Person -> ()
graduate! : project-field name | add! ", Ph.D."
```

In a language with traditional references like C++, that code might look like this:

```cpp
void graduate(Person &person) {
  person.name += "Ph.D.";
}
```

So how does `project-field` work? Under the hood, Wipple has two new constructs. The first is `where` for simplifying the process of updating a single field in a structure. It can be used anywhere, not just for `Mutable` values!

```wipple
-- The functional way
graduate :: Person -> Person
graduate : person -> \
  person where { last-name : (last-name of person) + ", Ph.D." }
```

And the second is the way `Mutable` is implemented. Previously, `Mutable` was essentially a reference to a value on the heap. That functionality has been moved to the new `Reference` type, and `Mutable` is now implemented in terms of `Reference`. But in addition to reference-based `Mutable` values, you can now create computed `Mutable` values that act like two-way bindings:

```wipple
-- Remove leading and trailing whitespace from a `Text` value
trim-whitespace :: Text -> Text
trim-whitespace : ...

-- Project a `Mutable Text` so that it never contains leading or trailing whitespace
project-trim-whitespace :: Mutable Text -> Mutable Text
project-trim-whitespace : project trim-whitespace (new _ -> trim-whitespace new)
```

That new `project` function is best explained by looking at its type. You provide a function that computes a `B` from an `A`, and a function that applies the new `B` to the original `A`. `project` is intended to be partially applied; that is, you usually don't provide the `Mutable A` immediately. Instead, you use `project` to define your own functions that operate on `Mutable` values.

```wipple
project :: A B => (A -> B) -> (B -> A -> A) -> Mutable A -> Mutable B
```

`project-field` isn't magic, either — it's implemented as a syntax rule!

```wipple
project-field : syntax {
  project-field 'field -> \
    project ({ 'field } -> 'field) (new val -> val where { 'field : new })
}
```

Finally, you can create a `Mutable` value that ignores changes with the `constant` function:

```wipple
one : constant 1
increment! one
show (get one) -- 1
```

The API for interacting with mutable values hasn't changed at all — you still use `get` and `set!` as normal, and all the new features work automatically!
