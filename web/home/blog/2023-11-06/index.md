---
layout: blog
title: New syntax for lists and tuples
date: 2023-11-06
---

For a long time, Wipple has used the `list` syntax to construct a list and the `,` syntax to construct a tuple. Today, this changes — the `,` syntax is now used for lists!

```wipple
numbers : 1 , 2 , 3
numbers . each show
```

```
1
2
3
```

I decided to make this change for two reasons. First, lists are used much more often than tuples, so it makes sense to give list syntax priority. Second, Wipple parses syntax rules before resolving variables, so having a syntax rule named `list` means that you can't declare a variable named `list` as well. The standard library worked around this by using names like `l` or `input`, but now you can just use the obvious variable name `list`.

If you provide elements of different types, you still get a nice error message:

```wipple
my-list : 1 , "2"
```

```
error:
  ┌─ test.wpl:1:15
  │
1 │ my-list : 1 , "2"
  │               ^^^
  │               │
  │               expected `Number`, but found `Text`
  │               this element must have the same type as the other elements
  │
  = for more information, see https://wipple.dev/playground/?lesson=errors/mismatched-types
```

To create an empty list, use the `,` operator by itself (or the `Default` implementation defined below):

```wipple
instance (Default (List _)) : (,)
```

And to create a list with a single element:

```wipple
just-one-number :: List Number
just-one-number : 1 ,
```

Trailing commas are allowed, so you can easily add a new item to a large list:

```wipple
constants : (
    1.41 ,
    1.62 ,
    2.72 ,
    3.14 ,
    6.28 ,
)
```

The `,` syntax for lists is defined in Wipple, too, meaning Wipple now supports variadic operators!

```wipple
[operator Variadic-Precedence]
, : syntax {
    , ...elements -> ...
}
```

And finally, to create a tuple, you now separate each element with a semicolon (`;`):

```wipple
my-tuple :: Number ; Text ; Boolean
my-tuple : 1 ; "a" ; True

first ; second ; third : my-tuple
show first
show second
show third
```

```
1
a
True
```

These changes are live on the [Wipple Playground](/playground), and the lessons have been updated to use the new syntax.
