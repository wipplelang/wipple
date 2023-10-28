---
layout: blog
title: Handling math errors in Wipple
date: 2023-10-28
---

Previously, Wipple's built-in math operations like `/` and `sqrt` would cause the program to crash if provided an invalid input. This caused problems when graphing functions using the `math` library:

```wipple
plot (x -> 1 / x) -- crashed when x = 0!
```

Really, what we want is to skip graphing any points that produce an undefined result. So now, Wipple's `Number` type has a new member — `undefined`!

Rather than crashing, all of Wipple's math operations now return `undefined` if provided an invalid or `undefined` input. This means `undefined` propagates through the program:

```wipple
x : sqrt -1
show x -- undefined
show (x + 1) -- undefined
```

When comparing `undefined` with another number, the result is always `False`. If you need to check whether an number is `undefined`, you can use `undefined?`:

```wipple
x : 0 / 0
show (x = undefined) -- False
show (undefined? x) -- True
```

This behavior matches the [NaN](https://en.wikipedia.org/wiki/NaN) value in the IEEE 754 floating-point standard (Wipple's `Number` type has a decimal representation, however), and indeed `undefined` is represented as `NaN` in JavaScript.
