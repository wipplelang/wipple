---
layout: blog
title: Automatic multi-line statements
date: 2023-12-12
---

Wipple has a new feature that makes it easier to put a single statement across multiple lines. Previously, you had to explicitly use the backslash character (`\`) to tell Wipple to merge the next line with the previous one:

```wipple
numbers : 1 , 2 , 3

numbers \
  . transform (+ 1) \
  . filter (< 3) \
  . each show
```

Now, Wipple will automatically treat the statement as continuing across multiple lines if you use any "common operator" like `:`, `->` and `.`!

```wipple
numbers : 1 , 2 , 3

numbers
  . transform (+ 1)
  . filter (< 3)
  . each show
```

The set of common operators is fixed in order to keep formatting separate from compilation; that is, Wipple doesn't need to parse attributes like `[operator]` in order to format your code. You can always continue to use `\` where needed!
